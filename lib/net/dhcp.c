/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

// stdlib includes
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

// barrelfish includes

// lwip includes
#include <lwip/ip.h>
#include <lwip/dhcp.h>
#include <lwip/prot/dhcp.h>
#include <lwip/timeouts.h>

#include <octopus/octopus.h>


#include <net_interfaces/flags.h>
#include "networking_internal.h"

///< the debug subsystem
//
#define NETDEBUG_SUBSYSTEM "dhcpd"

///< the DHCP timeout in milli seconds
#define DHCP_TIMEOUT_MSECS (120UL * 1000)


static void dhcpd_timer_callback(void *data)
{
    struct net_state *st = data;

    dhcp_fine_tmr();
    if ((st->dhcp_ticks % (DHCP_COARSE_TIMER_MSECS / DHCP_FINE_TIMER_MSECS)) == 0) {
        dhcp_coarse_tmr();
    }
    st->dhcp_ticks++;
}

/*
static void dhcpd_timer_callback_polling(void *data)
{
    dhcpd_timer_callback(data);
    sys_timeout(DHCP_FINE_TIMER_MSECS, dhcpd_timer_callback_polling, data);
}
*/

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

    /* DHCP fine timer */
    err = periodic_event_create(&st->dhcp_timer, st->waitset,
                                (DHCP_FINE_TIMER_MSECS * 1000),
                                MKCLOSURE(dhcpd_timer_callback, st));


    if (err_is_fail(err)) {
        dhcp_stop(&st->netif);
        return err;
    }

    if (flags & NET_FLAGS_BLOCKING_INIT) {
        printf("waiting for DHCP to complete \n");
        while (!dhcpd_has_ip()) {
            // will call event_dispatch()/event_dispatch_nonblock()
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


/**
 * @brief queries the current ip setting of the machine
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_config_current_ip_query(net_flags_t flags, uint32_t* ip_address)
{
    errval_t err;

    NETDEBUG("query current IP...\n");

    // initialize octopus if not already done
    err = oct_init();
    if (err_is_fail(err)) {
        return err;
    }

    char* record = NULL;
    err = oct_get(&record, "net.current_ip");
    if (err_no(err) == OCT_ERR_NO_RECORD && (flags & NET_FLAGS_BLOCKING_INIT)) {
        printf("waiting for DHCP to complete \n");
        err = oct_wait_for(&record, NET_CONFIG_CURRENT_IP_RECORD_REGEX);
        if (err_is_fail(err)) {
            return err;
        }
    } else if (err_is_fail(err)) {
        DEBUG_ERR(err, "cannot get static ip record\n");
        return err;
    }

    uint64_t ip, nm, gw;
    err = oct_read(record, "_" NET_CONFIG_IP_RECORD_FIELDS, &ip, &gw, &nm);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cannot read current ip record '%s\n", record);
        free(record);
        return err;
    }
    free(record);

    struct in_addr ipaddr, netmask, gateway;
    ipaddr.s_addr = (uint32_t)ip;
    netmask.s_addr = (uint32_t)nm;
    gateway.s_addr = (uint32_t)gw;
    *ip_address = (uint32_t)ip;

    debug_printf("Got current IP set: %s\n", inet_ntoa(ipaddr));
    debug_printf("Got current GW set: %s\n", inet_ntoa(gateway));
    debug_printf("Got current NM set: %s\n", inet_ntoa(netmask));

    return SYS_ERR_OK;
}


/**
 * @brief queries the static ip setting of the machine and sets it
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_config_static_ip_query(net_flags_t flags)
{
    errval_t err;

    NETDEBUG("query static IP...\n");

    // initialize octopus if not already done
    err = oct_init();
    if (err_is_fail(err)) {
        return err;
    }

    struct net_state *st = get_default_net_state();
    assert(st);

    char* record = NULL;
    err = oct_get(&record, "net.static_ip");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cannot get static ip record\n");
        return err;
    }

    uint64_t ip, nm, gw;
    err = oct_read(record, "_" NET_CONFIG_IP_RECORD_FIELDS, &ip, &gw, &nm);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cannot read static ip record '%s\n", record);
        free(record);
        return err;
    }
    free(record);

    struct in_addr ipaddr, netmask, gateway;
    ipaddr.s_addr = (uint32_t)ip;
    netmask.s_addr = (uint32_t)nm;
    gateway.s_addr = (uint32_t)gw;

    debug_printf("Got static IP set: %s\n", inet_ntoa(ipaddr));
    debug_printf("Got static GW set: %s\n", inet_ntoa(gateway));
    debug_printf("Got static NM set: %s\n", inet_ntoa(netmask));

    err = netif_set_ipconfig(&ipaddr, &gateway, &netmask);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cannot set static ip\n");
        return err;
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
errval_t netif_get_ipconfig(struct in_addr *ip, struct in_addr *gw, struct in_addr *nm)
{
    struct net_state *st = get_default_net_state();
    if (ip) {
        ip->s_addr = netif_ip4_addr(&st->netif)->addr;
    }

    if (gw) {
        gw->s_addr = netif_ip4_gw(&st->netif)->addr;
    }

    if (nm) {
        nm->s_addr = netif_ip4_netmask(&st->netif)->addr;
    }

    return SYS_ERR_OK;
}

/**
 * @brief sets the IP configuration, overrides DHCP
 *
 * @param ip    the IP address
 * @param gw    the Gateway
 * @param nm    the Netmask
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t netif_set_ipconfig(struct in_addr *ip, struct in_addr *gw, struct in_addr *nm)
{
    errval_t err;
    struct net_state *st = get_default_net_state();

    if (st->dhcp_running == 1) { // stop dhcp, if it's running
        err = dhcpd_stop();
        if (err_is_fail(err)) {
            return err;
        }
    }

    ip_addr_t _ipaddr, _netmask, _gateway;
    _ipaddr.addr = ip->s_addr;
    _netmask.addr = nm->s_addr;
    _gateway.addr = gw->s_addr;
    netif_set_addr(&st->netif, &_ipaddr, &_netmask, &_gateway);
    netif_set_up(&st->netif);

    return SYS_ERR_OK;
}
