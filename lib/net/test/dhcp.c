/**
 * \file ping.c
 * \brief 
 */


/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>

#include <lwip/ip.h>
#include <lwip/udp.h>
#include <lwip/pbuf.h>

#include <lwip/mem.h>
#include <lwip/raw.h>
#include <lwip/icmp.h>
#include <lwip/netif.h>
#include <lwip/sys.h>
#include <lwip/timeouts.h>
#include <lwip/ip_addr.h>
#include <lwip/prot/ip4.h>

#include <net/net.h>
#include <net/dhcp.h>

#include <octopus/octopus.h>
#include <octopus/getset.h>

ip_addr_t ip;
ip_addr_t nm;
ip_addr_t gw;

uint8_t counter = 0;

static int client_main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("DHCP client main.\n");

    err = dhcpd_query(0);
    assert(err_is_ok(err));

    debug_printf("DHCP client query init\n");

    while(1) {
        event_dispatch(get_default_waitset());
    }
}

#define DHCP_RECORD_FIELDS "{ ip: %d, gw: %d, netmask: %d }"
#define DHCP_RECORD_FORMAT "net.ipconfig " DHCP_RECORD_FIELDS

static void timer_callback(void *data)
{
    errval_t err;

    if (++counter == 0) {
        ++counter;
    }
    IP_ADDR4(&ip, 192,168,1,counter);

    debug_printf("DHCP server timer. set ip to %s\n", ip4addr_ntoa(&ip));

    err = oct_set(DHCP_RECORD_FORMAT,ip.addr, gw.addr, nm.addr);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to set the DHCP record\n");
    }
}

static int server_main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("DHCP server main.\n");

    IP_ADDR4(&ip, 192,168,1,0);
    IP_ADDR4(&nm, 255,255,255,0);
    IP_ADDR4(&gw, 192,168,1,0);

    struct periodic_event ptimer;

    /* DHCP fine timer */
    err = periodic_event_create(&ptimer, get_default_waitset(), (5000 * 1000),
                                MKCLOSURE(timer_callback, NULL));
    assert(err_is_ok(err));

    while(1) {
        event_dispatch(get_default_waitset());
    }
}

int main(int argc, char *argv[])
{
    debug_printf("DHCP started.\n");

    oct_init();

    /* parse ip */

    if (argc == 2) {
        if (strncmp("server", argv[1], strlen("server")) == 0) {
            return server_main(argc, argv);
        } else if (strncmp("client", argv[1], strlen("client")) == 0) {
            return client_main(argc, argv);
        } else {
            USER_PANIC("invalid argument supplied: %s\n", argv[1])
        }
    }

    return client_main(argc, argv);
}


