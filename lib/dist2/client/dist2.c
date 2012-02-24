/**
 * \file
 * \brief Initialization functions for the dist2 client library.
 *
 * We use two bindings: One for communication with the server using RPC calls,
 * and one for asynchronous events coming from the server.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/threads.h>
#include <barrelfish/nameservice_client.h>

#include <if/monitor_defs.h>
#include <dist2/init.h>
#include <thc/thc.h>

#include "handler.h"
#include "common.h"

static struct dist_state {
    struct dist2_binding* binding;
    struct dist2_thc_client_binding_t thc_client;
    struct waitset ws;
    errval_t err;
    bool is_done;
} rpc, event;

static iref_t service_iref = 0;
static uint64_t client_identifier = 0;

struct dist2_binding* dist_get_event_binding(void)
{
    assert(event.binding != NULL);
    return event.binding;
}

struct dist2_thc_client_binding_t* dist_get_thc_client(void)
{
    //assert(rpc.rpc_client != NULL);
    return &rpc.thc_client;
}

static void identify_response_handler(struct dist2_binding* b)
{
    event.is_done = true;
}

static struct dist2_rx_vtbl rx_vtbl = {
        .identify_response = identify_response_handler,
        .subscription = subscription_handler,
        .trigger = trigger_handler
};

/*
static int event_handler_thread(void* st)
{
    errval_t err = SYS_ERR_OK;
    struct dist2_binding* b = dist_get_event_binding();

    b->change_waitset(b, &event.ws);

    uint64_t id = (uint64_t) st;
    err = b->tx_vtbl.identify_call(b, NOP_CONT, id, dist2_BINDING_EVENT);
    assert(err_is_ok(err));

    // TODO abort condition
    while (1) {
        err = event_dispatch(&event.ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err,
                    "error in event_dispatch for dist2 event binding");
        }
    }

    return SYS_ERR_OK;
}*/

static void event_bind_cb(void *st, errval_t err, struct dist2_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "dist_event bind failed");
        goto out;
    }

    event.binding = b;
    event.binding->rx_vtbl = rx_vtbl;

out:
    assert(!event.is_done);
    event.is_done = true;
    event.err = err;
}

static void get_name_iref_reply(struct monitor_binding *mb, iref_t iref,
                                uintptr_t state)
{
    struct dist_state* ds = (struct dist_state*)state;
    service_iref = iref;
    ds->err = (iref != 0) ? SYS_ERR_OK : LIB_ERR_GET_NAME_IREF;
    ds->is_done = true;
}

static errval_t init_binding(struct dist_state* state,
        dist2_bind_continuation_fn bind_fn)
{
    errval_t err = SYS_ERR_OK;
    assert(service_iref != 0);

    state->is_done = false;
    err = dist2_bind(service_iref, bind_fn, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, FLOUNDER_ERR_BIND);
    }

    //  Wait for callback to complete
    while (!state->is_done) {
        messages_wait_and_handle_next();
    }

    return state->err;
}

static errval_t get_service_iref(void)
{
    errval_t err = SYS_ERR_OK;
    if (service_iref > 0) {
        // we already have the iref
        return err;
    }

    struct monitor_binding *mb = get_monitor_binding();

    rpc.is_done = false;

    mb->rx_vtbl.get_name_iref_reply = get_name_iref_reply;
    err = mb->tx_vtbl.get_name_iref_request(mb, NOP_CONT, (uintptr_t)&rpc);
    if (err_is_fail(err)) {
        return err;
    }

    while (!rpc.is_done) {
        messages_wait_and_handle_next();
    }

    if (err_is_fail(rpc.err)) {
        return rpc.err;
    }

    return err;

}

errval_t dist_thc_init(void)
{
    errval_t err = SYS_ERR_OK;

    err = get_service_iref();
    if (err_is_fail(err)) {
        return err;
    }

    assert(service_iref != 0);
    err = dist2_thc_connect(service_iref,
            get_default_waitset(), IDC_BIND_FLAGS_DEFAULT, &(rpc.binding));
    if (err_is_fail(err)) {
        return err;
    }

    assert(rpc.binding != NULL);
    err = dist2_thc_init_client(&rpc.thc_client, rpc.binding, rpc.binding);
    if (err_is_fail(err)) {
        return err;
    }

    // TODO: Hack. Tell the server that these bindings belong together
    // We can't use the same binding in 2 different threads with
    // rpc and non-rpc calls.
    dist2_thc_client_binding_t* cl = dist_get_thc_client();
    err = cl->call_seq.get_identifier(cl, &client_identifier);
    if (err_is_fail(err)) {
        return err;
    }

    // Register rpc binding using identifier
    err = cl->call_seq.identify(cl, client_identifier, dist2_BINDING_RPC);

    return err;
}

/**
 * \brief Initializes the dist2 client library.
 *
 * Note the dist rpc binding is most likely already initialized
 * by libbarrelfish (used for nameservice). This function
 * will set up the event thread to handle asynchronous events.
 *
 * \note Call this before you call any dist2 functions that require
 * asynchronous callbacks (subscribe, get/set/del with triggers).
 */
errval_t dist_init(void)
{
    errval_t err = dist_thc_init();
    if (err_is_fail(err)) {
        return err;
    }

    err = init_binding(&event, event_bind_cb);
    if (err_is_fail(err)) {
        return err;
    }

    // Register event binding
    event.is_done = false;
    event.binding->tx_vtbl.identify_call(event.binding, NOP_CONT,
            client_identifier, dist2_BINDING_EVENT);
    while (!event.is_done) {
        messages_wait_and_handle_next();
    }

    return err;
}
