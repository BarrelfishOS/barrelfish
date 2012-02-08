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

#include "handler.h"
#include "common.h"

static struct dist_state {
    struct dist2_binding* binding;
    struct dist2_rpc_client* rpc_client;
    struct waitset ws;
    iref_t iref;
    errval_t err;
    bool is_done;
} rpc, event;

static struct thread_sem ts;

struct dist2_binding* get_dist_event_binding(void)
{
    assert(event.binding != NULL);
    return event.binding;
}

struct dist2_rpc_client* get_dist_rpc_client(void)
{
    //assert(rpc.rpc_client != NULL);
    return rpc.rpc_client;
}
/*
static void identify_response_handler(struct dist2_binding* b)
{
    thread_sem_post(&ts);
}

static struct dist2_rx_vtbl rx_vtbl = {
        .identify_response = identify_response_handler,
        .subscribed_message = subscribed_message_handler,
        .trigger = trigger_handler
};

static int event_handler_thread(void* st)
{
    errval_t err = SYS_ERR_OK;
    struct dist2_binding* b = get_dist_event_binding();

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
}

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
}*/

static void rpc_bind_cb(void *st, errval_t err, struct dist2_binding* b)
{
    if (err_is_ok(err)) {
        rpc.rpc_client = malloc(sizeof(struct dist2_rpc_client));
        assert(rpc.rpc_client != NULL);

        err = dist2_rpc_client_init(rpc.rpc_client, b);
        if (err_is_fail(err)) {
            free(rpc.rpc_client);
        }
    } // else: Do nothing

    printf("cb done!\n");

    assert(!rpc.is_done);
    rpc.is_done = true;
    rpc.err = err;
}

static void get_name_iref_reply(struct monitor_binding *mb, iref_t iref,
                                uintptr_t state)
{
    struct dist_state* ds = (struct dist_state*)state;
    ds->iref = iref;
    ds->is_done = true;


    if (iref == 0) {
        ds->err = LIB_ERR_GET_NAME_IREF;
    }
    else {
        ds->err = SYS_ERR_OK;
    }
}

static errval_t init_binding(struct dist_state* state,
        dist2_bind_continuation_fn bind_fn)
{
    errval_t err = SYS_ERR_OK;
    struct monitor_binding *mb = get_monitor_binding();

    state->is_done = false;
    mb->rx_vtbl.get_name_iref_reply = get_name_iref_reply;
    err = mb->tx_vtbl.get_name_iref_request(mb, NOP_CONT, (uintptr_t)state);
    if (err_is_fail(err)) {
        return err;
    }
    while (!state->is_done) {
        messages_wait_and_handle_next();
    }
    if (err_is_fail(state->err)) {
        return state->err;
    }

    state->is_done = false;
    err = dist2_bind(state->iref, bind_fn, NULL, get_default_waitset(),
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

/**
 * \brief Initializes the dist2 client library.
 *
 * Sets up the rpc and event binding for communication
 * with the server. Events on the event binding are handled
 * on a separate thread.
 *
 * \note Call this before you call any other dist2 functions.
 */
errval_t dist_init(void)
{
    //thread_mutex_init(&trigger_mutex);

    errval_t err = SYS_ERR_OK;
    thread_sem_init(&ts, 0);

    err = init_binding(&rpc, rpc_bind_cb);
    if (err_is_fail(err)) {
        return err;
    }

    /*err = init_binding(&event, event_bind_cb);
    if (err_is_fail(err)) {
        return err;
    }*/

    // TODO: Hack. Tell the server that these bindings belong together
    // We can't use the same binding in 2 different threads with
    // rpc and non-rpc calls.

    // Get identifier from server
    struct dist2_rpc_client* dist_rpc = get_dist_rpc_client();
    uint64_t id = 0;
    err = dist_rpc->vtbl.get_identifier(dist_rpc, &id);
    if (err_is_fail(err)) {
        return err;
    }

    // Spawn event handler thread (handles asynchronous messages from server)
    /*struct thread* t = thread_create(event_handler_thread, (void*) id);
    assert(t != NULL);*/

    // Register rpc binding using identifier
    err = dist_rpc->vtbl.identify(dist_rpc, id, dist2_BINDING_RPC);

    dist_pubsub_init();

    // Wait until event binding has registered itself
    //thread_sem_wait(&ts);
    return err;
}
