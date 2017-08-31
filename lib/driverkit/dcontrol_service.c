/**
 * \brief Implements the message interface for a device driver instance
 * to act on requests from a controller (probably Kaluga).
 *
 * Essentially these are message handler stubs that forward requests
 * for attach/detach/set_sleep_level etc. to the corresponding
 * driver instance (struct bfdriver_instance) within a driver domain.
 */
/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <driverkit/driverkit.h>

#include <if/dcontrol_defs.h>
#include "debug.h"

/**
 * Binding state.
 */
static struct bind_state {
    struct dcontrol_binding* binding;
    bool is_done;
    errval_t err;
    iref_t control_iref;
} rpc_export;

/**
 * Tells instance to reattach to device, replies with error.
 */
static void dcontrol_default_attach_handler(struct dcontrol_binding* binding) {
    struct bfdriver_instance* bfi = (struct bfdriver_instance*) binding->st;
    assert(bfi != NULL);
    assert(bfi->driver != NULL);
    assert(bfi->driver->attach != NULL);

    errval_t out_err = bfi->driver->attach(bfi);

    errval_t err = binding->tx_vtbl.attach_response(binding, NOP_CONT, out_err);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Sending reply failed.");
    }
}

/**
 * Tells instance to detach from device, replies with error.
 */
static void dcontrol_default_detach_handler(struct dcontrol_binding* binding) {
    struct bfdriver_instance* bfi = (struct bfdriver_instance*) binding->st;
    assert(bfi != NULL);
    assert(bfi->driver != NULL);
    assert(bfi->driver->detach != NULL);


    errval_t out_err = bfi->driver->detach(bfi);

    errval_t err = binding->tx_vtbl.detach_response(binding, NOP_CONT, out_err);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Sending reply failed.");
    }
}

/**
 * Sets sleep level on the instance, replies with error.
 */
static void dcontrol_default_set_sleep_level_handler(struct dcontrol_binding* binding, uint32_t level) {
    struct bfdriver_instance* bfi = (struct bfdriver_instance*) binding->st;
    assert(bfi != NULL);
    assert(bfi->driver != NULL);
    assert(bfi->driver->set_sleep_level != NULL);

    errval_t out_err = bfi->driver->set_sleep_level(bfi, level);

    errval_t err = binding->tx_vtbl.set_sleep_level_response(binding, NOP_CONT, out_err);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Sending reply failed.");
    }
}

/**
 * Virtual table for controller stubs.
 */
static const struct dcontrol_rx_vtbl rpc_rx_vtbl = {
    .attach_call = dcontrol_default_attach_handler,
    .detach_call = dcontrol_default_detach_handler,
    .set_sleep_level_call = dcontrol_default_set_sleep_level_handler,
};

/**
 * Called when the service has successfully exported itself.
 *
 * Does two things:
 *   - Registers the service as $instance_name.control in the nameserver
 *   - Stores iref in rpc_export
 *
 * \param st   st Points to the driver instance that this service belongs to.
 * \param err  Error of the export.
 * \param iref Interface service reference.
 */
static void rpc_export_cb(void *st, errval_t err, iref_t iref) {
    assert(st != NULL);
    struct bfdriver_instance* bfi = (struct bfdriver_instance*) st;
    assert(bfi->name != NULL);

    if (err_is_fail(err)) {
        DRIVERKIT_DEBUG("Exporting control interface for %s failed.\n", bfi->name);
        err = err_push(err, DRIVERKIT_ERR_CONTROL_IFACE_EXPORT);
    }
    else {
        char* service_name = NULL;
        int r = asprintf(&service_name, "%s.control", bfi->name);
        assert (r > 0);
        err = nameservice_register(service_name, iref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can't register control interface with nameserver.\n");
            err = err_push(err, DRIVERKIT_ERR_CONTROL_IFACE_EXPORT);
        }
        else {
            DRIVERKIT_DEBUG("Control interface for %s exported at [%s, %"PRIuIREF"]\n", bfi->name, service_name, iref);
        }
        free(service_name);
    }

    rpc_export.control_iref = iref;
    rpc_export.err = err;
    rpc_export.is_done = true;
}

/**
 * Called when someone connects to the control interface of the driver instance.
 *
 * Stores a pointer to the driver instance in the binding state
 * and sets up handler functions.
 *
 * \param  st The driver instance (struct bfdriver_instance)
 * \param  b  The newly created binding
 * \retval SYS_ERR_OK Accepts the connection.
 */
static errval_t rpc_connect_cb(void *st, struct dcontrol_binding *b)
{
    struct bfdriver_instance* bfi = (struct bfdriver_instance*) st;
    assert(bfi != NULL);
    
    DRIVERKIT_DEBUG("Got a new connection for controlling driver instance %s.\n", bfi->name);
    b->rx_vtbl = rpc_rx_vtbl;
    b->st = st;

    return SYS_ERR_OK;
}

/**
 * Export the control interface for a given driver instance.
 * Stores the control iref in the bfi struct.
 *
 * \param[in]  bfi              The driver instance to associate with the service.
 * \param[in]  waitset          The waitset to use for this serice (if NULL, default waitset is used).
 * \retval SYS_ERR_OK           Service successfully exported.
 */
errval_t dcontrol_service_init(struct bfdriver_instance* bfi, struct waitset* ws)
{
    rpc_export.err = SYS_ERR_OK;
    rpc_export.is_done = false;
    rpc_export.control_iref = 0;

    struct waitset* service_ws = (ws == NULL) ? get_default_waitset() : ws;
    errval_t err = dcontrol_export(bfi, rpc_export_cb, rpc_connect_cb,
                                   service_ws, IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    // XXX: broken
    while (!rpc_export.is_done) {
        messages_wait_and_handle_next();
    }

    bfi->control = rpc_export.control_iref;
    return rpc_export.err;
}
