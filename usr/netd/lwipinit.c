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

// keeping it for DHCP timers
#include <barrelfish/deferred.h>

#include "netd_debug.h"
#include "netd.h"


static struct periodic_event dhcp_fine_timer; // fine-grain timer for DHCP
static struct periodic_event dhcp_coarse_timer; // coarse-grain timer for DHCP

// local ip address.  Used to keep track of changing IP addresses
static struct ip_addr local_ip = {
    .addr = 0x00,       // BFDMUX_IP_ADDR_ANY
};

// state variable indicating if dhcp is done or not
static bool dhcp_completed = false;

static void timer_callback(void *data)
{

//    NETD_DEBUG("timer_callback: called\n");
    void (*lwip_callback) (void) = data;

    lwip_callback();

//    NETD_DEBUG("timer_callback: terminated\n");
}


static void setup_dhcp_timer(void)
{
    errval_t err;

    // Initialize Timer and LWIP
    NETD_DEBUG("setting up timeouts for lwip\n");

    /* DHCP fine timer */
    err = periodic_event_create(&dhcp_fine_timer, get_default_waitset(),
                                (DHCP_FINE_TIMER_MSECS * 1000),
                                MKCLOSURE(timer_callback, dhcp_fine_tmr));
    assert(err_is_ok(err));

    /* DHCP coarse timer */
    err = periodic_event_create(&dhcp_coarse_timer, get_default_waitset(),
                                (DHCP_COARSE_TIMER_MSECS * 1000),
                                MKCLOSURE(timer_callback, dhcp_coarse_tmr));
    assert(err_is_ok(err));
}

static void link_status_change(struct netif *nf)
{
    static bool subsequent_call;

    if (netif_is_up(nf)) {
        printf("netd: interface is now up\n");
    } else {
        printf("netd: interface is now down\n");
        return;
    }

    if (subsequent_call) {
        if (ip_addr_cmp(&local_ip, &nf->ip_addr) != 0) {
            printf
              ("netd: WARNING: IP has changed! Current address: %d.%d.%d.%d",
               ip4_addr1(&nf->ip_addr), ip4_addr2(&nf->ip_addr),
               ip4_addr3(&nf->ip_addr), ip4_addr4(&nf->ip_addr));
        }
    } else {
        // warning: some regression tests depend upon the format of this message
        printf("##########################################\n");
        printf("Interface up! IP address %d.%d.%d.%d\n",
               ip4_addr1(&nf->ip_addr), ip4_addr2(&nf->ip_addr),
               ip4_addr3(&nf->ip_addr), ip4_addr4(&nf->ip_addr));
    }

    local_ip = nf->ip_addr;
    netif_set_default(nf);

    if (!subsequent_call) {
#if 0
        /* Now, the timers are not needed.  They should be closed. */
        /* I don't agree -- we need to keep renewing our lease -AB */
        periodic_event_cancel(&dhcp_fine_timer);
        periodic_event_cancel(&dhcp_coarse_timer);
#endif // 0

        dhcp_completed = true;
    }

    subsequent_call = true;
}



static void get_ip_from_dhcp(struct netif *nf_ptr)
{

    netif_set_status_callback(nf_ptr, link_status_change);
    NETD_DEBUG("get_ip_from_dhcp: starting dhcp\n");
    setup_dhcp_timer();
    err_t err = dhcp_start(nf_ptr);

    assert(err == ERR_OK);
}

void startlwip(char *card_name, uint64_t queueid)
{

    NETD_DEBUG("NETD is taking control of the LWIP for card[%s][%"PRIu64"]\n",
            card_name, queueid);
    // take ownership of lwip
    netif_ptr = owner_lwip_init(card_name, queueid);
    assert(netif_ptr != NULL);
    get_ip_from_dhcp(netif_ptr);

    NETD_DEBUG("Waiting for DHCP to complete\n");
    struct waitset *ws = get_default_waitset();
    while (!dhcp_completed) {
        errval_t err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch (waiting for dhcp)");
            break;
        }
    } // end while

    NETD_DEBUG("registering net_ARP service\n");
    // FIXME: register ARP service
} // end function startlwip
