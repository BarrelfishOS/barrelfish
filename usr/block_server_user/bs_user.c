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

#include <if/block_service_defs.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_allocator.h>

#include "bs_connector.h"
#include "benchmark.h"

struct bs_connection service;

/**
 * \brief Main function for the network block server client.
 *
 */
int main(int argc, char *argv[])
{
    debug_printf("block service user started...\n");

    errval_t err;

    struct bulk_channel_callbacks *rx_cb = bench_get_rx_cb();
    struct bulk_channel_callbacks *tx_cb = bench_get_tx_cb();
    err = bs_service_connect(&service, rx_cb, tx_cb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not connect to the block service.\n");
        exit(EXIT_FAILURE);
    }

    err = bench_init(&service);
    assert(!err_is_fail(err));

    err = bench_run();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failure while running benchmark\n");
        exit(EXIT_FAILURE);
    }

    while(true);

}

