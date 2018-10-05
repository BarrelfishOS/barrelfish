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

struct dcontrol_binding* b;
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
 * Get endpoint of this instance, replies with error.
 */
static void dcontrol_default_get_ep_handler(struct dcontrol_binding* binding, bool lmp) {
    struct bfdriver_instance* bfi = (struct bfdriver_instance*) binding->st;
    assert(bfi != NULL);
    assert(bfi->driver != NULL);
    assert(bfi->driver->set_sleep_level != NULL);
    errval_t err, out_err;

    struct capref cap;
    out_err = slot_alloc(&cap);
    if (err_is_fail(out_err)) {
        goto send_reply;
    }    

    out_err = bfi->driver->get_ep(bfi, lmp, &cap);

send_reply:
    err = binding->tx_vtbl.get_ep_response(binding, NOP_CONT, cap, out_err);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Sending reply failed.");
    }
}

/**
 * Virtual table for controller stubs.
 */
static struct dcontrol_rx_vtbl rpc_rx_vtbl = {
    .attach_call = dcontrol_default_attach_handler,
    .detach_call = dcontrol_default_detach_handler,
    .set_sleep_level_call = dcontrol_default_set_sleep_level_handler,
    .get_ep_call = dcontrol_default_get_ep_handler,
};

/**
 * Export the control interface for a given driver instance.
 * Stores the control iref in the bfi struct.
 *
 * \param[in]  bfi              The driver instance to associate with the service.
 * \param[in]  waitset          The waitset to use for this serice (if NULL, default waitset is used).
 * \param[in]  lmp              Endpoint type
 * \param[out]  ret_cap         Created endpoint through wich the interface is reachable
 * \retval SYS_ERR_OK           Service successfully exported.
 */
errval_t dcontrol_service_init(struct bfdriver_instance* bfi, struct waitset* ws, 
                                bool lmp, struct capref* ret_cap)
{
    struct waitset* service_ws = (ws == NULL) ? get_default_waitset() : ws;
    
    errval_t err = dcontrol_create_endpoint(lmp? IDC_ENDPOINT_LMP: IDC_ENDPOINT_UMP, 
                                            &rpc_rx_vtbl, bfi, service_ws, 
                                            IDC_ENDPOINT_FLAGS_DUMMY, 
                                            &b, *ret_cap);
    if (err_is_fail(err)) {   
        return err;
    }

    return SYS_ERR_OK;
}
