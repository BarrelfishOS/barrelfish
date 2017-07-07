/**
 * \file
 * \brief Implementation of ata_rw28.if interface (to enable working vfs_fat)
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/omap_sdma_defs.h>

#include "sdma.h"

static errval_t mem_copy_handler(struct omap_sdma_binding *sv, struct capref dst, struct capref src, errval_t* err) {
    struct sdma_driver_state* ds = sv->st;
    *err = mem_copy(ds, dst, src);
    return SYS_ERR_OK;
}

static errval_t mem_fill_handler(struct omap_sdma_binding *sv, struct capref dst, uint8_t color, errval_t* err) {
    struct sdma_driver_state* ds = sv->st;
    *err = mem_fill(ds, dst, color);
    return SYS_ERR_OK;
}

static errval_t mem_copy_2d_handler(struct omap_sdma_binding *sv,
                             omap_sdma_addr_2d_t dst, omap_sdma_addr_2d_t src, omap_sdma_count_2d_t count,
                             bool transparent, uint32_t color, errval_t* err) {
    struct sdma_driver_state* ds = sv->st;
    *err = mem_copy_2d(ds, dst, src, count, transparent, color);
    return SYS_ERR_OK;
}

static errval_t mem_fill_2d_handler(struct omap_sdma_binding *sv,
                             omap_sdma_addr_2d_t dst, uint32_t color, omap_sdma_count_2d_t count,
                             errval_t* err) {
    struct sdma_driver_state* ds = sv->st;
    *err = mem_fill_2d(ds, dst, count, color);
    return SYS_ERR_OK;
}

static struct export_state {
    struct omap_sdma_binding* b;
    bool is_done;
    errval_t err;
    iref_t iref;
} service_export;

static void export_cb(void *st, errval_t err, iref_t iref)
{
    service_export.is_done = true;
    service_export.err = err;
    service_export.iref = iref;

    if (err_is_ok(err)) {
        SDMA_DEBUG("Exported ddomain service with iref: %"PRIu32"\n", iref);
        err = nameservice_register("sdma", iref);
        assert(err_is_ok(err));
    }
}

static const struct omap_sdma_rpc_rx_vtbl rx_vtbl = {
    .mem_copy_call = mem_copy_handler,
    .mem_fill_call = mem_fill_handler,
    .mem_copy_2d_call = mem_copy_2d_handler,
    .mem_fill_2d_call = mem_fill_2d_handler,
};

static errval_t client_connect(void *st, struct omap_sdma_binding *b)
{
    service_export.b = b;
    b->rpc_rx_vtbl = rx_vtbl;
    b->st = st;
    return SYS_ERR_OK;
}

void sdma_init_service(struct sdma_driver_state* st, iref_t* iref)
{
    errval_t err;
    SDMA_DEBUG("%s:%d: Starting server\n", __FUNCTION__, __LINE__);
    err = omap_sdma_export(st, export_cb, client_connect, get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }

    while(!service_export.is_done) {
        messages_wait_and_handle_next();
    }
    *iref = service_export.iref;
    SDMA_DEBUG("Service sdma exported.\n");
}
