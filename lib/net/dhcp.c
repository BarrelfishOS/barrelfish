/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

// stdlib includes

// barrelfish includes

// lwip includes
#include <lwip/ip.h>
#include <lwip/dhcp.h>
#include <lwip/timeouts.h>

#include <octopus/octopus.h>

#include <net_interfaces/flags.h>
#include "networking_internal.h"

///< the debug subsystem
#define debug_printf_SUBSYSTEM "dhcpd"

///< the DHCP timeout in milli seconds
#define DHCP_TIMEOUT_MSECS (120UL * 1000)

#define DHCP_RECORD_FIELDS "{ ip: %d, gw: %d, netmask: %d }"


#define DHCP_RECORD_FORMAT "net.ipconfig " DHCP_RECORD_FIELDS

#define DHCP_RECORD_REGEX "net.ipconfig  {ip: _,  gw: _, netmask: _}"


static void dhcpd_timer_callback(void *data)
{
    errval_t err;

    struct net_state *st = data;

    dhcp_fine_tmr();



    if ((st->dhcp_ticks % (DHCP_COARSE_TIMER_MSECS / DHCP_FINE_TIMER_MSECS)) == 0) {
        dhcp_coarse_tmr();
    }

    if (!ip_addr_cmp(&st->netif.ip_addr, IP_ADDR_ANY) && st->dhcp_done == 0) {

        debug_printf("setting ip record\n");

        /* register IP with octopus */
        err = oct_set(DHCP_RECORD_FORMAT, netif_ip4_addr(&st->netif)->addr,
                      netif_ip4_gw(&st->netif)->addr,
                      netif_ip4_netmask(&st->netif)->addr);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to set the DHCP record\n");
        }
        st->dhcp_done = 1;
    }

    st->dhcp_ticks++;
}

static void dhcpd_timer_callback_polling(void *data)
{
    dhcpd_timer_callback(data);
    sys_timeout(DHCP_FINE_TIMER_MSECS, dhcpd_timer_callback_polling, data);
}

static bool dhcpd_has_ip(void)
{
    struct net_state *st = get_default_net_state();

    return !ip_addr_cmp(&st->netif.ip_addr, IP_ADDR_ANY);
}


/**
 * @brief starts the dhcpd service on the interface
 *
 * @param flags flags to provide
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t dhcpd_start(net_flags_t flags)
{
    errval_t err;

    struct net_state *st = get_default_net_state();

    // initialize octopus if not already done
    err = oct_init();
    if (err_is_fail(err)) {
        return err;
    }

    debug_printf("starting DHCP...\n");
    err_t lwip_err = dhcp_start(&st->netif);
    if(lwip_err != ERR_OK) {
        printf("ERRRRRR dhcp start: %i\n", lwip_err);
        return -1;
    }

    st->dhcp_ticks = 1;
    st->dhcp_running = 1;

    if (flags & NET_FLAGS_POLLING) {
        sys_timeout(DHCP_FINE_TIMER_MSECS, dhcpd_timer_callback_polling, st);
    } else {
        /* DHCP fine timer */
        err = periodic_event_create(&st->dhcp_timer, st->waitset,
                (DHCP_FINE_TIMER_MSECS * 1000),
                MKCLOSURE(dhcpd_timer_callback, st));
    }


    if (err_is_fail(err)) {
        dhcp_stop(&st->netif);
        return err;
    }

    if (flags & NET_FLAGS_BLOCKING_INIT) {
        printf("waiting for DHCP to complete");
        while(!dhcpd_has_ip()) {
            event_dispatch_non_block(st->waitset);
            networking_poll();

            if (st->dhcp_ticks > DHCP_TIMEOUT_MSECS / DHCP_FINE_TIMER_MSECS) {
                dhcpd_stop();
                return -1;
            }
        }
        printf("OK\nDHCP completed.\n");
    }

    return SYS_ERR_OK;
}


/**
 * @brief stops the dhcpd service
 */
errval_t dhcpd_stop(void)
{
    struct net_state *st = get_default_net_state();

    periodic_event_cancel(&st->dhcp_timer);

    dhcp_stop(&st->netif);

    st->dhcp_ticks = 0;
    st->dhcp_running = 0;

    return SYS_ERR_OK;
}


/* functions for querying the current settings */


static void dhcpd_change_event(octopus_mode_t mode, const char* record, void* arg)
{
    errval_t err;

    struct net_state *st = arg;

    debug_printf("DHCP change event: %s\n", record);

    if (mode & OCT_ON_SET) {

        uint64_t ip, nm, gw;
        err = oct_read(record, "_" DHCP_RECORD_FIELDS, &ip, &gw, &nm);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "cannot read DHCPD record '%s\n", record);
            return;
        }

        ip_addr_t ipaddr, netmask, gateway;
        ipaddr.addr = (uint32_t)ip;
        netmask.addr = (uint32_t)nm;
        gateway.addr = (uint32_t)gw;

        debug_printf("DHCP got ip set: %s \n", ip4addr_ntoa(&ipaddr));
        debug_printf("DHCP got gw set: %s\n", ip4addr_ntoa(&gateway));
        debug_printf("DHCP got nm set: %s\n", ip4addr_ntoa(&netmask));

        netif_set_addr(&st->netif, &ipaddr, &netmask, &gateway);
        netif_set_up(&st->netif);

        st->dhcp_done = true;
    }

    if (mode & OCT_ON_DEL) {

        /* DHCP has been removed */
        netif_set_down(&st->netif);
    }
}

/**
 * @brief queries the DHCPD settings of the machine
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t dhcpd_query(net_flags_t flags)
{
    errval_t err;

    // initialize octopus if not already done
    err = oct_init();
    if (err_is_fail(err)) {
        return err;
    }

    struct net_state *st = get_default_net_state();
    assert(st);

    st->dhcp_ticks = 1;
    st->dhcp_running = 1;

    err = oct_trigger_existing_and_watch(DHCP_RECORD_REGEX, dhcpd_change_event,
                                         st, &st->dhcp_triggerid);
    if (err_is_fail(err)) {
        return err;
    }

    if (flags & NET_FLAGS_BLOCKING_INIT) {
        printf("waiting for DHCP to complete");
        while(!dhcpd_has_ip()) {
            event_dispatch(get_default_waitset());
        }
    }

    return SYS_ERR_OK;
}


/**
 * @brief returns the IP configuration
 *
 * @param ip    return the IP address
 * @param gw    returns the gateway
 * @param nm    returns the netmask
 *
 * @return
 */
errval_t dhcpd_get_ipconfig(ip_addr_t *ip, ip_addr_t *gw, ip_addr_t *nm)
{
    struct net_state *st = get_default_net_state();
    if (ip) {
        ip->addr = netif_ip4_addr(&st->netif)->addr;
    }

    if (gw) {
        gw->addr = netif_ip4_gw(&st->netif)->addr;
    }

    if (nm) {
        nm->addr = netif_ip4_netmask(&st->netif)->addr;
    }

    return SYS_ERR_OK;
}

/**
 * @brief sets the IP configuration
 *
 * @param ip    the IP address
 * @param gw    the Gateway
 * @param nm    the Netmask
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t dhcpd_set_ipconfig(ip_addr_t *ip, ip_addr_t *gw, ip_addr_t *nm)
{
    errval_t err;
    struct net_state *st = get_default_net_state();

    if (st->dhcp_running == 1) {
        if (st->dhcp_triggerid) {
            err = oct_remove_trigger(st->dhcp_triggerid);
        } else {
            err = dhcpd_stop();
        }
        if (err_is_fail(err)) {
            return err;
        }
    }

    netif_set_addr(&st->netif, ip, nm, gw);
    netif_set_up(&st->netif);

    st->dhcp_done = true;

    return SYS_ERR_OK;
}
