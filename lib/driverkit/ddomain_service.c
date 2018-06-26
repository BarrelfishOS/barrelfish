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

    iref_t dev = 0;
    
    struct bfdriver_instance* inst = calloc(1, sizeof(struct bfdriver_instance));
    if (inst == NULL) {
        err = LIB_ERR_MALLOC_FAIL;
        goto send_reply;
    }

    inst->capc = 0;
    if (!capref_is_null(cap1)) {
        inst->caps[inst->capc++] = cap1;
    }
    if (!capref_is_null(cap2)) {
        inst->caps[inst->capc++] = cap2;
    }
    if (!capref_is_null(cap3)) {
        inst->caps[inst->capc++] = cap3;
    }
    if (!capref_is_null(cap4)) {
        inst->caps[inst->capc++] = cap4;
    }
    if (!capref_is_null(cap5)) {
        inst->caps[inst->capc++] = cap5;
    }
    if (!capref_is_null(cap5)) {
        inst->caps[inst->capc++] = cap6;
    }

    inst->argcn_cap = NULL_CAP;
    inst->argcn = NULL_CNODE;

    /* Copy the arguments to our own memory */
    inst->argc = 0;
    if (a1len) {
        strncpy(inst->_argv[0], a1, sizeof(inst->_argv[0]));
        inst->argv[inst->argc++] = inst->_argv[0];
    } else {
        inst->argv[0] = NULL;
    }
    if (a2len) {
        strncpy(inst->_argv[1], a2, sizeof(inst->_argv[1]));
        inst->argv[inst->argc++] = inst->_argv[1];
    } else {
        inst->argv[1] = NULL;
    }
    if (a3len) {
        strncpy(inst->_argv[2], a3, sizeof(inst->_argv[2]));
        inst->argv[inst->argc++] = inst->_argv[2];
    } else {
        inst->argv[2] = NULL;
    }
    if (a4len) {
        strncpy(inst->_argv[3], a4, sizeof(inst->_argv[3]));
        inst->argv[inst->argc++] = inst->_argv[3];
    } else {
        inst->argv[3] = NULL;
    }

    strncpy(inst->name, name, sizeof(inst->name));

    err = slot_alloc(&inst->ctrl);
    if (err_is_fail(err)){
        DEBUG_ERR(err, "Instantiating driver failed, report this back to Kaluga."
                "name=%s, cls=%s\n", name, cls);
        free(inst);
        goto send_reply;
    }

    DRIVERKIT_DEBUG("Instantiate driver\n");
    err = driverkit_create_driver(cls, inst, flags, &dev, &inst->ctrl);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Instantiating driver failed, report this back to Kaluga."
                "name=%s, cls=%s\n", name, cls);
        cap_destroy(inst->ctrl);
        free(inst);
    }

    send_reply:

    DRIVERKIT_DEBUG("sending create response to kaluga\n");
    err = ddomain_create_response__tx(binding, NOP_CONT, dev, inst->ctrl, err);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Sending reply failed.\n");
        /* TODO: handle error */
    }
}

static void create_with_argcn_handler(struct ddomain_binding* binding,
                                      const char* cls, size_t cls_len,
                                      const char* name, size_t nlen,
                                      const char* a1, size_t a1len,
                                      const char* a2, size_t a2len,
                                      const char* a3, size_t a3len,
                                      const char* a4, size_t a4len,
                                      struct capref argcn,
                                      uint64_t flags)
{
    errval_t err;
    DRIVERKIT_DEBUG("Driver domain got create message from kaluga for cls=%s,"
                            "name=%s\n", cls, name);

    iref_t dev = 0;

    struct bfdriver_instance* inst = calloc(1, sizeof(struct bfdriver_instance));
    if (inst == NULL) {
        err = LIB_ERR_MALLOC_FAIL;
        goto send_reply;
    }

    err = slot_alloc_root(&inst->argcn_cap);
    if (err_is_fail(err)) {
        free(inst);
        goto send_reply;
    }

    err = cap_copy(inst->argcn_cap, argcn);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cannot copy argcn cap to root node\n");
        slot_free(inst->argcn_cap);
        free(inst);
        goto send_reply;
    }
    inst->capc = 0;
    inst->argcn = build_cnoderef(inst->argcn_cap, CNODE_TYPE_OTHER);

    /* Copy the arguments to our own memory */
    inst->argc = 0;
    if (a1len) {
        strncpy(inst->_argv[0], a1, sizeof(inst->_argv[0]));
        inst->argv[inst->argc++] = inst->_argv[0];
    } else {
        inst->argv[0] = NULL;
    }
    if (a2len) {
        strncpy(inst->_argv[1], a2, sizeof(inst->_argv[1]));
        inst->argv[inst->argc++] = inst->_argv[1];
    } else {
        inst->argv[1] = NULL;
    }
    if (a3len) {
        strncpy(inst->_argv[2], a3, sizeof(inst->_argv[2]));
        inst->argv[inst->argc++] = inst->_argv[2];
    } else {
        inst->argv[2] = NULL;
    }
    if (a3len) {
        strncpy(inst->_argv[3], a4, sizeof(inst->_argv[3]));
        inst->argv[inst->argc++] = inst->_argv[3];
    } else {
        inst->argv[3] = NULL;
    }

    struct capref ctrl;
    err = slot_alloc(&inst->ctrl);
    if (err_is_fail(err)){
        DEBUG_ERR(err, "Instantiating driver failed, report this back to Kaluga."
                "name=%s, cls=%s\n", name, cls);
        cap_destroy(inst->argcn_cap);
        free(inst);
        goto send_reply;
    }

    ctrl = inst->ctrl;

    DRIVERKIT_DEBUG("Instantiate driver\n");
    err = driverkit_create_driver(cls, inst, flags, &dev, &inst->ctrl);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Instantiating driver failed, report this back to Kaluga."
                "name=%s, cls=%s\n", name, cls);
        cap_destroy(inst->argcn_cap);
        slot_free(inst->ctrl);
        free(inst);
        ctrl = NULL_CAP;
    }

    send_reply:

    if (err)
    DRIVERKIT_DEBUG("sending create response to kaluga\n");
    err = ddomain_create_with_argcn_response__tx(binding, NOP_CONT, dev, ctrl,
                                                 err);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Sending reply failed.\n");
        cap_destroy(inst->argcn_cap);
        slot_free(inst->ctrl);
        free(inst);
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
    .create_with_argcn_call = create_with_argcn_handler,
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
