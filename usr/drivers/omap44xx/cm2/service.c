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

#include "cm2.h"

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/cm2_defs.h>

static errval_t cm2_enable_i2c_handler(struct cm2_binding *sv, uint32_t index)
{
    struct cm2_driver_state* ds = sv->st;
    cm2_enable_i2c(ds, index);
    return SYS_ERR_OK;
}

static errval_t cm2_get_hsmmc1_base_clock_handler(struct cm2_binding *sv, uint32_t* clock)
{
    struct cm2_driver_state* ds = sv->st;
    *clock = cm2_get_hsmmc1_base_clock(ds);
    return SYS_ERR_OK;
}

static errval_t cm2_enable_hsmmc1_handler(struct cm2_binding *sv)
{
    struct cm2_driver_state* ds = sv->st;
    cm2_enable_hsmmc1(ds);
    return SYS_ERR_OK;
}

static struct export_state {
    struct cm2_binding* b;
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
        CM2_DEBUG("Exported ddomain service with iref: %"PRIu32"\n", iref);
        err = nameservice_register("cm2", iref);
        assert(err_is_ok(err));
    }
}
static const struct cm2_rpc_rx_vtbl rx_vtbl = {
    .enable_i2c_call = cm2_enable_i2c_handler,
    .get_hsmmc1_base_clock_call = cm2_get_hsmmc1_base_clock_handler,
    .enable_hsmmc1_call = cm2_enable_hsmmc1_handler
};

static errval_t client_connect(void *st, struct cm2_binding *b)
{
    service_export.b = b;
    b->rpc_rx_vtbl = rx_vtbl;
    b->st = st;
    return SYS_ERR_OK;
}

void cm2_init_service(struct cm2_driver_state* st, iref_t* iref)
{
    errval_t err;
    CM2_DEBUG("%s:%d: Starting server\n", __FUNCTION__, __LINE__);
    err = cm2_export(st, export_cb, client_connect, get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }

    while(!service_export.is_done) {
        messages_wait_and_handle_next();
    }

    *iref = service_export.iref;
    CM2_DEBUG("Service cm2 exported.\n");
}
