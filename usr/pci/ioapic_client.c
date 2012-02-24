/**
 * \file
 * \brief I/O APIC RPC Client
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/ioapic_rpcclient_defs.h>

#include "ioapic_client.h"

struct ioapic_connection {
    bool is_done;
    errval_t err;
} state;

static struct ioapic_rpc_client* rpc_client;

struct ioapic_rpc_client* get_ioapic_rpc_client(void)
{
    assert(rpc_client != NULL);
    return rpc_client;
}

static void rpc_bind_cb(void *st, errval_t err, struct ioapic_binding* b)
{
    if (err_is_ok(err)) {
        rpc_client = malloc(sizeof(struct ioapic_rpc_client));
        assert(rpc_client != NULL);

        err = ioapic_rpc_client_init(rpc_client, b);
        if (err_is_fail(err)) {
            free(rpc_client);
        }
    } // else: Do nothing

    assert(!state.is_done);
    state.is_done = true;
    state.err = err;
}

errval_t connect_to_ioapic(void) {
    errval_t err;
    iref_t iref;

    err = nameservice_blocking_lookup("ioapic", &iref);
    if (err_is_fail(err)) {
        return err;
    }

    state.is_done = false;
    err = ioapic_bind(iref, rpc_bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, FLOUNDER_ERR_BIND);
    }

    //  Wait for callback to complete
    while (!state.is_done) {
        messages_wait_and_handle_next();
    }

    return state.err;

}
