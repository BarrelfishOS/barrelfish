/**
 * \file
 * \brief This code shall be used in Kaluga for receiving replies of various
 * driver domains and sending requests to driver domains.
 *
 * Note the client/service distinction can be confusing here. Altough, Kaluga
 * exports the service (for bootstrapping reasons and Barrelfish limitations
 * -> can't just ship an endpoint). We call this the client even tough
 * it exports the service. The ddomain_service.c which runs as part of
 * the driver domain connects to 'this file' at runtime.
 */

/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <driverkit/driverkit.h>
#include <if/ddomain_defs.h>

#include "debug.h"

#define SERVICE_NAME "ddomain_controller"

static struct export_state {
    struct ddomain_binding* b;
    bool is_done;
    errval_t err;
} rpc_export;

static void rpc_export_cb(void *st, errval_t err, iref_t iref)
{
    rpc_export.is_done = true;
    rpc_export.err = err;

    if (err_is_ok(err)) {
        DRIVERKIT_DEBUG("Exported ddomain service with iref: %"PRIu32"\n", iref);
        err = nameservice_register(SERVICE_NAME, iref);
        assert(err_is_ok(err));
    }
}

static void ddomain_create_response_handler(struct ddomain_binding* binding, iref_t dev, iref_t control, errval_t err) {
    DRIVERKIT_DEBUG("Driver domain created driver rechable at [%"PRIuIREF", %"PRIuIREF"]\n", dev, control);
}

static void ddomain_destroy_response_handler(struct ddomain_binding* binding, errval_t err) {
    DRIVERKIT_DEBUG("Driver destroyed [%s]\n", err_getstring(err));
}

static const struct ddomain_rx_vtbl rpc_rx_vtbl = {
    .create_response = ddomain_create_response_handler,
    .destroy_response = ddomain_destroy_response_handler,
};

static errval_t rpc_connect_cb(void *st, struct ddomain_binding *b)
{
    DRIVERKIT_DEBUG("Got a new connection from driver domain.\n");
    rpc_export.b = b;

    b->rx_vtbl = rpc_rx_vtbl;

    // Set up continuation queue
    b->st = NULL;
    return SYS_ERR_OK;
}

errval_t ddomain_controller_init(void)
{
    rpc_export.err = SYS_ERR_OK;
    rpc_export.is_done = false;

    errval_t err = ddomain_export(&rpc_export, rpc_export_cb, rpc_connect_cb,
                                   get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        return err;
    }

    // XXX: broken
    while (!rpc_export.is_done) {
        messages_wait_and_handle_next();
    }

    /*
    // Hack for testing: wait for 1st controller to connect
    while(rpc_export.b == NULL) {
        messages_wait_and_handle_next();
    }

    struct capref c;
    err = slot_alloc(&c);
    assert(err_is_ok(err));
    err = frame_alloc(&c, 4096, NULL);
    assert(err_is_ok(err));
    (rpc_export.b)->tx_vtbl.create((rpc_export.b), NOP_CONT, "uart", 5, "uart_instance", 14, c, 0x0);
    // end of hack for testing
    */

    return rpc_export.err;
}
