/**
 * \file
 * \brief LWIP test/demo code
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <lwip/netif.h>
#include <lwip/dhcp.h>
#include <netif/etharp.h>
#include <lwip/init.h>
#include <lwip/tcp.h>
#include <netif/bfeth.h>

#include "netd_private.h"
#include <netd/netd_debug.h>

/* Enable tracing only when it is globally enabled */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define ENABLE_WEB_TRACING 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE


static void timer_callback(void *data)
{

    void (*lwip_callback) (void) = data;
    NETD_DEBUG("timer_callback: triggering %p\n", lwip_callback);
//    wrapper_perform_lwip_work();
    lwip_callback();

//    NETD_DEBUG("timer_callback: terminated\n");
}


static void setup_dhcp_timer(struct netd_state *state)
{
    errval_t err;

    // Initialize Timer and LWIP
    NETD_DEBUG("setting up timeouts for lwip\n");

    /* DHCP fine timer */
    err = periodic_event_create(&state->dhcp_fine_timer, get_default_waitset(),
                                (DHCP_FINE_TIMER_MSECS * 1000),
                                MKCLOSURE(timer_callback, dhcp_fine_tmr));
    assert(err_is_ok(err));

    /* DHCP coarse timer */
    err = periodic_event_create(&state->dhcp_coarse_timer, get_default_waitset(),
                                (DHCP_COARSE_TIMER_MSECS * 1000),
                                MKCLOSURE(timer_callback, dhcp_coarse_tmr));
    assert(err_is_ok(err));
}

static void link_status_change(struct netif *nf)
{
    struct netd_state *state = nf->state;
    
    if (netif_is_up(nf)) {
        printf("netd: interface is now up\n");
    } else {
        printf("netd: interface is now down\n");
        return;
    }
    if (state->subsequent_call) {
        if (ip_addr_cmp(&state->local_ip, &nf->ip_addr) != 0) {
            printf
              ("netd: WARNING: IP has changed! Current address: %d.%d.%d.%d",
               ip4_addr1(&nf->ip_addr), ip4_addr2(&nf->ip_addr),
               ip4_addr3(&nf->ip_addr), ip4_addr4(&nf->ip_addr));
        }
    } else {
        // warning: some regression tests depend upon the format of this message
        printf("%s:##########################################\n",
                disp_name());
        printf("Interface up! IP address %d.%d.%d.%d\n",
               ip4_addr1(&nf->ip_addr), ip4_addr2(&nf->ip_addr),
               ip4_addr3(&nf->ip_addr), ip4_addr4(&nf->ip_addr));
        printf("NetMask %d.%d.%d.%d\n",
               ip4_addr1(&nf->netmask), ip4_addr2(&nf->netmask),
               ip4_addr3(&nf->netmask), ip4_addr4(&nf->netmask));
        printf("GateWay %d.%d.%d.%d\n",
               ip4_addr1(&nf->gw), ip4_addr2(&nf->gw),
               ip4_addr3(&nf->gw), ip4_addr4(&nf->gw));

        printf("for MAC = %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx\n",
                nf->hwaddr[0], nf->hwaddr[1], nf->hwaddr[2],
                nf->hwaddr[3], nf->hwaddr[4], nf->hwaddr[5]);
        printf("##########################################\n");


    } // end else:

    state->local_ip = nf->ip_addr;
    netif_set_default(nf);

    if (!state->subsequent_call) {
#if 0
        /* Now, the timers are not needed.  They should be closed. */
        /* I don't agree -- we need to keep renewing our lease -AB */
        periodic_event_cancel(&dhcp_fine_timer);
        periodic_event_cancel(&dhcp_coarse_timer);
#endif // 0

        if (state->do_dhcp) {
            state->dhcp_completed = true;
        }
    } // end if: first call

    state->subsequent_call = true;
}



static void get_ip_from_dhcp(struct netd_state *state, struct netif *nf_ptr)
{


    NETD_DEBUG("get_ip_from_dhcp: starting dhcp\n");
    setup_dhcp_timer(state);
    err_t err = dhcp_start(nf_ptr);

    assert(err == ERR_OK);
}

static void convert_str_to_ip_addr(char *addr_str,
        struct ip_addr *ip_addr_holder)
{
    // Preparing IP address for use
    assert(addr_str != NULL);
    struct in_addr addr;
    if (inet_aton(addr_str, &addr) == 0) {
        printf("Invalid IP addr: %s\n", addr_str);
        USER_PANIC("Invalid IP address %s", addr_str);
        return;
    }
    ip_addr_holder->addr = addr.s_addr;
} // end function: convert_str_to_ip_addr

void startlwip(struct netd_state *state, char *card_name, uint64_t queueid)
{

    NETD_DEBUG("NETD is taking control of the LWIP for card[%s][%"PRIu64"]\n",
            card_name, queueid);
    state->subsequent_call = false;
    state->local_ip.addr = 0x00; // BFDMUX_IP_ADDR_ANY
    
    // take ownership of lwip
    state->netif_ptr = owner_lwip_init(card_name, queueid);
    assert(state->netif_ptr != NULL);
    state->netif_ptr->state = state;
    netif_set_status_callback(state->netif_ptr, link_status_change);

#if 0
#if ENABLE_WEB_TRACING
        printf("Starting tracing\n");

//        errval_t err = trace_control(TRACE_EVENT(TRACE_SUBSYS_NET,
        errval_t errt = trace_control_fixed_events_counter(TRACE_EVENT(TRACE_SUBSYS_NET,
                    TRACE_EVENT_NET_START, 0),
                TRACE_EVENT(TRACE_SUBSYS_NET,
                    TRACE_EVENT_NET_STOP, 0), 0,
                //2000);
                2000);
        set_cond_termination(trace_conditional_termination);

        if(err_is_fail(errt)) {
            USER_PANIC_ERR(errt, "trace_control failed");
        }
        trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_START, 0);
#else // ENABLE_WEB_TRACING
        printf("Tracing not enabled\n");
#endif // ENABLE_WEB_TRACING
#endif // 0
    if (state->do_dhcp) {
        get_ip_from_dhcp(state, state->netif_ptr);

        NETD_DEBUG("Waiting for DHCP to complete\n");
        struct waitset *ws = get_default_waitset();
        while (!state->dhcp_completed) {
            errval_t err = event_dispatch(ws);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch (waiting for dhcp)");
                break;
            }
        } // end while
        return;
    } // end if: do_dhcp

    // end else: static ip configuration
    // directly set IP address
    printf("Configuring interface with static values\n");
    printf("ip[%s], nm[%s], gw[%s]\n", state->ip_addr_str, state->netmask_str,
                state->gateway_str);
    struct ip_addr ipaddr, gw, netmask;
    convert_str_to_ip_addr(state->ip_addr_str, &ipaddr);
    convert_str_to_ip_addr(state->netmask_str, &netmask);
    convert_str_to_ip_addr(state->gateway_str, &gw);


    netif_set_ipaddr(state->netif_ptr, &ipaddr);
    netif_set_gw(state->netif_ptr, &gw);
    netif_set_netmask(state->netif_ptr, &netmask);
    netif_set_up(state->netif_ptr);

} // end function startlwip
