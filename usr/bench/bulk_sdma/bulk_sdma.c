/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <barrelfish/barrelfish.h>

#include <thc/thc.h>

#include <if/omap_sdma_defs.h>
#include <if/omap_sdma_thc.h>

#include "bulk_sim.h"

static struct omap_sdma_thc_client_binding_t *cl;

void bulk_sdma_setup(void)
{
    errval_t err;
    struct omap_sdma_binding *b;

    err = omap_sdma_thc_connect_by_name("sdma",
                                      get_default_waitset(),
                                      IDC_BIND_FLAGS_DEFAULT,
                                      &b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not bind SDMA service");
    }

    cl = malloc(sizeof(struct omap_sdma_thc_client_binding_t));
    assert(cl != NULL);

    err = omap_sdma_thc_init_client(cl, b, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not init SDMA client");
    }
}

#define MAX_YCOUNT    ((1<<16) - 1)
#define MAX_XCOUNT    ((1<<24) - 1)

void bulk_sdma_transfer(struct capref dst, struct capref src, size_t len)
{
    omap_sdma_addr_2d_t src_addr = {
        .cap = src,
        .x_start = 0,
        .y_start = 0,
        .x_modify = 1,
        .y_modify = 1,
    };

    omap_sdma_addr_2d_t dst_addr = {
        .cap = dst,
        .x_start = 0,
        .y_start = 0,
        .x_modify = 1,
        .y_modify = 1,
    };

    omap_sdma_count_2d_t count_2d = {
        .pixel_size = omap_sdma_DATA_TYPE_8BIT,
        .x_count = len,
        .y_count = 1,
    };

    while (count_2d.x_count > MAX_XCOUNT) {
        if ((count_2d.x_count % 2) != 0) {
            USER_PANIC("Payload size too big AND not a power of two.");
        }

        count_2d.x_count /= 2;
        count_2d.y_count++;

        assert(count_2d.y_count <= MAX_YCOUNT);
    }

    errval_t err;
    cl->call_fifo.mem_copy_2d(cl, dst_addr, src_addr, count_2d, false, 0, &err);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "SDMA transfer failed");
    }
}
