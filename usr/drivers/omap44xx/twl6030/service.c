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

#include "twl6030.h"

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/twl6030_defs.h>



static errval_t vmmc_csel_off(struct twl6030_binding *sv)
{
    struct twl6030_driver_state* ds = sv->st;
    ti_twl6030_vmmc_off(ds->twl);
    return SYS_ERR_OK;
}

static errval_t vmmc_on_handler(struct twl6030_binding *sv)
{
    struct twl6030_driver_state* ds = sv->st;
    ti_twl6030_vmmc_on(ds->twl);
    return SYS_ERR_OK;
}

static errval_t vmmc_vsel_handler(struct twl6030_binding *sv, uint32_t vsel)
{
    struct twl6030_driver_state* ds = sv->st;
    return ti_twl6030_set_vmmc_vsel(ds->twl, vsel);
}

static struct export_state {
    struct twl6030_binding* b;
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
        TWL_DEBUG("Exported ddomain service with iref: %"PRIu32"\n", iref);
        err = nameservice_register("twl6030", iref);
        assert(err_is_ok(err));
    }
}
static const struct twl6030_rpc_rx_vtbl rx_vtbl = {
    .vmmc_on_call = vmmc_on_handler,
    .vmmc_off_call = vmmc_csel_off,
    .vmmc_vsel_call = vmmc_vsel_handler,
};

static errval_t client_connect(void *st, struct twl6030_binding *b)
{
    service_export.b = b;
    b->rpc_rx_vtbl = rx_vtbl;
    b->st = st;
    return SYS_ERR_OK;
}

void twl6030_init_service(struct twl6030_driver_state* st, iref_t* iref)
{
    errval_t err;
    TWL_DEBUG("%s:%d: Starting server\n", __FUNCTION__, __LINE__);
    err = twl6030_export(st, export_cb, client_connect, get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }

    while(!service_export.is_done) {
        messages_wait_and_handle_next();
    }
    *iref = service_export.iref;
    TWL_DEBUG("Service twl6030 exported.\n");
}
