/**
 * \file
 * \brief ACPI RPC Client
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

#include <if/acpi_defs.h>
#include <if/acpi_rpcclient_defs.h>

#include "acpi_client.h"

struct acpi_connection {
    bool is_done;
    errval_t err;
} state;

static struct acpi_rpc_client* rpc_client;

struct acpi_rpc_client* get_acpi_rpc_client(void)
{
    assert(rpc_client != NULL);
    return rpc_client;
}

static void rpc_bind_cb(void *st, errval_t err, struct acpi_binding* b)
{
    if (err_is_ok(err)) {
        rpc_client = malloc(sizeof(struct acpi_rpc_client));
        assert(rpc_client != NULL);

        err = acpi_rpc_client_init(rpc_client, b);
        if (err_is_fail(err)) {
            free(rpc_client);
        }
    } // else: Do nothing

    assert(!state.is_done);
    state.is_done = true;
    state.err = err;
}

errval_t connect_to_acpi(void) {
    errval_t err;
    iref_t iref;

    err = nameservice_blocking_lookup("acpi", &iref);
    if (err_is_fail(err)) {
        return err_push(err, CHIPS_ERR_GET_SERVICE_REFERENCE);
    }

    state.is_done = false;
    err = acpi_bind(iref, rpc_bind_cb, NULL, get_default_waitset(),
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
