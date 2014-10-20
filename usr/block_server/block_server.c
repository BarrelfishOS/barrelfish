/**
 * \file
 * \brief block_server process.
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

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_allocator.h>

#include "block_server.h"
#include "local_server.h"
#include "network_server.h"
#include "block_storage.h"




/**
 *
 *
 */
int main(int argc, char *argv[])
{
    debug_printf("block server started.\n");

    errval_t err;



    /* initialize the block store */
    err = block_storage_init(BLOCK_COUNT, BLOCK_SIZE);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the block service.\n");
        exit(EXIT_FAILURE);
    }


#if BLOCK_ENABLE_NETWORKING
    /* initialize the network service */
    err = block_net_init(BLOCK_NET_PORT);
    if (err_is_fail(err)) {
        block_storage_dealloc();
        USER_PANIC_ERR(err, "could not initialize the network service.\n");
        exit(EXIT_FAILURE);
    }

    /* start the network service */
    debug_printf(" > Network initialization done. Starting Server.\n");
    err = block_net_start();

    assert(!"should not reach that point");
#else
    /* Initialize the core local flounder interface  */
    struct block_net_service local_service;
    err = block_local_init(&local_service, SERVICE_FLAG_DEFAULT);
    if (err_is_fail(err)) {
        block_storage_dealloc();
        USER_PANIC_ERR(err, "could not initialize the flounder service.\n");
        exit(EXIT_FAILURE);
    }

    /* start the floudner service */
    debug_printf(" > Local initialization done. Starting Server.\n");
    err = block_local_start();

    while (1) {
        messages_wait_and_handle_next();
    }
#endif

}

