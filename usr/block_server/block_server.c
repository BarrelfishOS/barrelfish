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


static void run_test(void)
{
    struct bulk_allocator alloc;
#define NUM_BUFS 10
#define BUF_SIZE 4096
    errval_t err;
    err = bulk_alloc_init(&alloc, NUM_BUFS, BUF_SIZE, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Bulk alloc failed");
    }

    printf("Pool allocated @ %p\n", (void *)alloc.pool->base_address);

    struct bulk_buffer *buf;
    for (int i = 0; i < NUM_BUFS; ++i) {
        buf = bulk_alloc_new_buffer(&alloc);
        if (buf) {
            printf("Buffer %i @ %p\n", i, buf->address);
            memset(buf->address, i, BUF_SIZE);
            char *e = buf->address;
            for (int j=0; j < BUF_SIZE; ++j) {
                if (e[j] != i) {
                    printf("ERROR: buffer has not the intended content");
                }
            }
        } else {
            printf("No new buffer %i\n", i);
        }
    }

    bulk_alloc_return_buffer(&alloc, buf);
    bulk_alloc_free(&alloc);

    printf("\n\n=============================================\n");
    abort();
}

/**
 *
 *
 */
int main(int argc, char *argv[])
{
    printf("\n\n==============================================\n");
    debug_printf("block server started.\n");

    errval_t err;

    run_test();

    return SYS_ERR_OK;

    /* initialize the block store */
    err = block_storage_init(BLOCK_COUNT, BLOCK_SIZE);
    if (err_is_fail(err)) {
        printf("ERROR: could not initialize the block store\n");
        return 1;
    }

    /* Initialize the core local flounder interface  */
    /* XXX: Do we need this? */
    err = block_local_init(SERVICE_FLAG_DEFAULT);
    if (err_is_fail(err)) {
        block_storage_dealloc();
        printf("ERROR: could not initialize the flounder service.\n");
        return 1;
    }

    /* initialize the network service */
    err = block_net_init(BLOCK_NET_PORT);
    if (err_is_fail(err)) {
        block_storage_dealloc();
        printf("ERROR: could not initialize the network service.\n");
        return 1;
    }

    /* start the network service */
    err = block_net_start();

    /* start the floudner service */
    err = block_local_start();

}

