/**
 * \brief Implements the handler function for a driver domain
 * to act upon requests from a device manager (i.e., Kaluga).
 *
 * The stubs will create, destroy driver instances using functions
 * mainly found in `modules.c` of this library.
 */

/*
 * Copyright (c) 2016, ETH Zurich.
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

#include <if/ddomain_defs.h>
#include "debug.h"

/**
 * State for connection.
 */
static struct bind_state {
    struct ddomain_binding* binding;
    bool is_done;
    errval_t err;
} rpc_bind;

/**
 * Check if the argument is non-null and of non zero length
 */
static bool arg_valid(const char * a){
    return a != NULL && strlen(a) > 0;
}

/**
 * Act upon request to create a driver instance.
 *
 * \param binding Controller binding
 * \param cls     What class to instantiate?
 * \param cls_len Ignored.
 * \param name    What name the driver instance should have.
 * \param nlen    Ignored.
 * \param cap     Capabilities for the driver instance.
 * \param flags   Flags for the driver instance.
 */
static void create_handler(struct ddomain_binding* binding, const char* cls, size_t cls_len,
                           const char* name, size_t nlen,
                           const char* a1, size_t a1len, const char* a2, size_t a2len,
                           const char* a3, size_t a3len, const char* a4, size_t a4len,
                           struct capref cap1, struct capref cap2, struct capref cap3,
                           struct capref cap4, struct capref cap5,  struct capref cap6,
                           uint64_t flags) {
    errval_t err;
    DRIVERKIT_DEBUG("Driver domain got create message from kaluga for cls=%s,"
            "name=%s\n", cls, name);

    iref_t dev = 0, ctrl = 0;

    static size_t NR_CAPS  = 6;
    static size_t NR_ARGS = 4;

    // This array is owned by the driver after create:
    struct capref* cap_array = calloc(sizeof(struct capref), NR_CAPS);
    cap_array[0] = cap1;
    cap_array[1] = cap2;
    cap_array[2] = cap3;
    cap_array[3] = cap4;
    cap_array[4] = cap5;
    cap_array[5] = cap6;

    struct capref cnodecap;
    err = slot_alloc_root(&cnodecap);
    assert(err_is_ok(err));
    err = cap_copy(cnodecap, cap_array[0]);
    struct capref cap0_0 = {
            .slot = 0,
            .cnode = build_cnoderef(cnodecap, CNODE_TYPE_OTHER)
    }; 
    char debug_msg[100];
    debug_print_cap_at_capref(debug_msg, sizeof(debug_msg), cap0_0);
    DRIVERKIT_DEBUG("Received cap0_0=%s\n", debug_msg);

    char** args_array = calloc(sizeof(char*), 4);
    args_array[0] = arg_valid(a1) ? strdup(a1) : NULL;
    args_array[1] = arg_valid(a2) ? strdup(a2) : NULL;
    args_array[2] = arg_valid(a3) ? strdup(a3) : NULL;
    args_array[3] = arg_valid(a4) ? strdup(a4) : NULL;

    int args_len;
    for(args_len=0; args_len<NR_ARGS; args_len++) {
        if(args_array[args_len] == NULL) break;
    }

    DRIVERKIT_DEBUG("Instantiate driver\n");
    err = driverkit_create_driver(cls, name, cap_array, NR_CAPS, args_array, args_len, flags, &dev, &ctrl);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Instantiating driver failed, report this back to Kaluga."
                "name=%s, cls=%s\n", name, cls);
    }

    DRIVERKIT_DEBUG("sending create response to kaluga\n");
    err = ddomain_create_response__tx(binding, NOP_CONT, dev, ctrl, err);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Sending reply failed.\n");
    }
}

/**
 * Destroy an existing driver instance.
 *
 * \param binding Controller binding.
 * \param name    Name of the driver instance.
 * \param len     Ignored
 */
static void destroy_handler(struct ddomain_binding* binding, const char* name, size_t len) {
    DRIVERKIT_DEBUG("Driver domain got destroy message for instance %s\n", name);
    errval_t err = driverkit_destroy(name);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Destroying driver failed, report this back to Kaluga.");
    }

    err = binding->tx_vtbl.destroy_response(binding, NOP_CONT, err);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Sending reply failed.");
    }
}

/**
 * Stubs table for functions to call on driver instance.
 */
static const struct ddomain_rx_vtbl rpc_rx_vtbl = {
    .create_call = create_handler,
    .destroy_call = destroy_handler,
};

/**
 * Called if connection to the manager has completed.
 *
 * \param st  NULL
 * \param err Connection initiated successfully?
 * \param b   Created binding, this is stored in rpc_bind.
 */
static void rpc_bind_cb(void *st, errval_t err, struct ddomain_binding *b)
{
    b->st = NULL;
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "oct_event bind failed");
        goto out;
    }

    DRIVERKIT_DEBUG("Driver domain has connected to ddomain controller service.\n");
    rpc_bind.binding = b;
    rpc_bind.binding->rx_vtbl = rpc_rx_vtbl;

out:
    assert(!rpc_bind.is_done);
    rpc_bind.is_done = true;
    rpc_bind.err = err;
}

/**
 * Connects to the driver domain manager.
 *
 * \param  connect_to iref where to connect.
 * \retval SYS_ERR_OK Connected to the driver manager.
 */
errval_t ddomain_communication_init(iref_t connect_to, uint64_t ident)
{
    rpc_bind.err = SYS_ERR_OK;
    rpc_bind.is_done = false;

    errval_t err = ddomain_bind(connect_to, rpc_bind_cb, NULL, get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }
    DRIVERKIT_DEBUG("%s:%s:%d: Trying to connect to kaluga...\n", __FILE__, __FUNCTION__, __LINE__);
    // XXX: broken
    while (!rpc_bind.is_done) {
        messages_wait_and_handle_next();
    }

    DRIVERKIT_DEBUG("%s:%s:%d: Send identify %"PRIu64"\n", __FILE__, __FUNCTION__, __LINE__, ident);
    errval_t send_err = rpc_bind.binding->tx_vtbl.identify(rpc_bind.binding, NOP_CONT, ident);
    assert(err_is_ok(send_err));

    return rpc_bind.err;
}
