/**
 * \file  block_server_client.c
 * \brief block server client domain
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h>
#include <bench/bench.h>

/* includes for ip address and lwip wrappers */
#include <ipv4/lwip/inet.h>
#include <lwip/init.h>

/* bulk transfer facilities */
#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_allocator.h>

/* local includes of block server client */
#include "block_server.h"
#include "local_server.h"
#include "network_common.h"
#include "network_client.h"

#if (BLOCK_ENABLE_NETWORKING == 0)
#include "block_storage.h"
#endif // (RUN_NET == 0)

/**
 * \brief Contains the connection parameters to the network block server
 */
static struct block_net_service block_server;

/* ======================= run net test facility =========================== */

static void message_handler_loop(void)
{
    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (true) {
        err = event_dispatch_non_block(ws);
        if (err != LIB_ERR_NO_EVENT) {
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch");
                break;
            }
        }

        wrapper_perform_lwip_work();
        /*err = event_dispatch(ws);
         if (err_is_fail(err)) {
         DEBUG_ERR(err, "in event_dispatch");
         break;
         }*/
    }
}


/**
 * \brief Main function for the network block server client.
 *
 */
int main(int argc, char *argv[])
{
    debug_printf("Block service network client started.\n");

    errval_t err;

    /*
     * Connect to the network block server
     */

#if BLOCK_ENABLE_NETWORKING
    if (argc != 2) {
        USER_PANIC("usage: %s [IP]", argv[0]);
    }

    struct ip_addr server_ip;
    struct in_addr addr;
    if (!inet_aton(argv[1], &addr)) {
        USER_PANIC("Could not convert the ip address");
    }

    /* XXX: does not work in ipv6 */
    server_ip.addr = addr.s_addr;

    err = block_net_connect(&block_server, &server_ip, BLOCK_NET_PORT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not connect to the block server");
        exit(EXIT_FAILURE);
    }

#if BLOCK_BENCH_ENABLE
    run_test(&block_server.tx_chan, &block_server.rx_chan, &block_server);
    debug_printf("test executed\n");
    while (1)
        ;
    assert(!"should not reach that point");
    return EXIT_SUCCESS;
#endif
#endif
    /*
     * TODO: Setup block caching
     */

    /*
     * Initialize the core local server (flounder interface)
     */
#if BLOCK_ENABLE_NETWORKING
    uint32_t flags = SERVICE_FLAG_CLIENT;
#else
    uint32_t flags = SERVICE_FLAG_DEFAULT;

    err = block_storage_init(BLOCK_COUNT, BLOCK_SIZE);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the block service.\n");
        exit(EXIT_FAILURE);
    }
#endif // RUN_NET

    err = block_local_init(&block_server, flags);
    if (err_is_fail(err)) {
        /* disconnect from the network server */
        block_net_disconnect(&block_server);
        USER_PANIC_ERR(err, "could not initialize local service");
        exit(EXIT_FAILURE);
    }

    err = block_local_start();
    if (err_is_fail(err)) {
        block_net_disconnect(&block_server);
        USER_PANIC_ERR(err, "could not start the local server.\n");
        exit(EXIT_FAILURE);
    }

    message_handler_loop();
}

