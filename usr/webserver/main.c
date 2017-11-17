/**
 * \file
 * \brief LWIP web server
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <stdio.h>
#include <net_sockets/net_sockets.h>
#include <netif/etharp.h>
#include <netif/bfeth.h>
#include <netbench/netbench.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

// #include <lwip/dhcp.h>
// #include <netif/etharp.h>
// #include <lwip/init.h>
// #include <lwip/tcp.h>

#include "webserver_network.h"
#include "webserver_debug.h"

static struct in_addr serverip;
static const char *serverpath;

/* Enable tracing only when it is globally enabled */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define ENABLE_WEB_TRACING 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE


int main(int argc, char**argv)
{
    errval_t err;

    // Parse args
    if (argc != 3) {
        printf("Usage: %s CardName NFSIP NFSpath\n", argv[0]);
        return 1;
    }
//    char *card_name = argv[1];

    if (inet_aton(argv[1], &serverip) == 0) {
        printf("Invalid IP addr: %s\n", argv[1]);
        return 1;
    }
    serverpath = argv[2];

    // Boot up
    DEBUGPRINT("init start\n");

    DEBUGPRINT("lwip_demo: lwip setup\n");
    printf("webserver:%u: initializing networking \n", disp_get_core_id());
    err = net_sockets_init();
    if (err_is_fail(err)) {
        USER_PANIC("Failed init %s \n", err_getstring(err));
    }
    printf("webserver:%u: networking initialized\n", disp_get_core_id());

//    lwip_benchmark_control(1, BMS_START_REQUEST, 0, 0);
    http_server_init(serverip, serverpath);

    DEBUGPRINT("Init finished.\n");

    uint32_t eventcount = 0;
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
        eventcount++;
#if ENABLE_WEB_TRACING
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_WEBEVENTLOOP, eventcount);
#endif // ENABLE_WEB_TRACING

    } // end while: infinite

}
