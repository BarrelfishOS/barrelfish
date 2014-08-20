/**
 * \file
 * \brief block_server client process.
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

#include <barrelfish/barrelfish.h>

#include "block_server.h"
#include "local_server.h"
#include "network_client.h"

static char *ip = "127.0.0.1";

/**
 * \brief Contains the connection parameters to the network block server
 */
static struct block_net_server block_server;

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

    block_server.ip = ip;
    block_server.port = BLOCK_NET_PORT;

    return SYS_ERR_OK;

    err = block_net_connect(&block_server);
    if (err_is_fail(err)) {
        printf("ERROR: Could not connect to block server: %s, (%i)\n",
                        block_server.ip, block_server.port);
        return 1;
    }

    /*
     * TODO: Setup block caching
     */

    /*
     * Initialize the core local server (flounder interface)
     */
    err = block_local_init(SERVICE_FLAG_CLIENT);
    if (err_is_fail(err)) {
        /* disconnect from the network server */
        block_net_disconnect(&block_server);
        printf("ERROR: Could not initialize local server.\n");
        return 1;
    }

    err = block_local_start();
    if (err_is_fail(err)) {
        block_net_disconnect(&block_server);
        printf("ERROR: Could not start the local server.\n");
        return 1;
    }
}

