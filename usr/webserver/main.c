/**
 * \file
 * \brief LWIP web server
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
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
#include <lwip/netif.h>
#include <lwip/dhcp.h>
#include <netif/etharp.h>
#include <lwip/init.h>
#include <lwip/tcp.h>
#include <netif/bfeth.h>

#include "webserver_network.h"
#include "webserver_debug.h"

static struct ip_addr serverip;
static const char *serverpath;

int main(int argc, char**argv)
{
    errval_t err;

    // Parse args
    if (argc != 3) {
        printf("Usage: %s NFSIP NFSpath\n", argv[0]);
        return 1;
    }

    struct in_addr server1;
    if (inet_aton(argv[1], &server1) == 0) {
        printf("Invalid IP addr: %s\n", argv[1]);
        return 1;
    }
    serverip.addr = server1.s_addr; // XXX
    serverpath = argv[2];

    // Boot up
    SERVER_DEBUG("init start\n");

    SERVER_DEBUG("lwip_demo: lwip setup\n");
    lwip_init("e1000");

    http_server_init(serverip, serverpath);

    SERVER_DEBUG("Init finished.\n");

    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

}
