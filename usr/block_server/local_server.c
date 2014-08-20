/**
 * \file
 * \brief Network server thread of the bulk server
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/block_service_defs.h>

#include "local_server.h"

/* ---------------------------  Server State  ---------------------------- */
static uint8_t server_state = SERVICE_STATE_UNINITIALIZED;

static iref_t server_iref = 0;

static uint8_t is_client = 0;

/* -------------------------  Request Callbacks  ------------------------- */

/**
 * \brief callback for read requests on the flounder channel
 */
static void rx_read_request(struct block_service_binding *binding,
                            uint32_t start_block, uint32_t count)
{
    /*
     * if is server, then serve the request locally
     * else forward it to the network server
     */
    if (is_client) {

    }
}

static void rx_write_request(struct block_service_binding *binding,
                             uint32_t start_block, uint32_t count)
{
    /*
     * if is server, then serve the request locally
     * else forward it to the network server
     */
    if (is_client) {

    }
}

/* ---------------------  Connection Initialization  --------------------- */

/**
 * \brief accepts new connections to the local  via the flounder interface
 */
static errval_t block_local_accept_cb(void *st, struct block_service_binding *b)
{
    // do the channel initialization
    b->rx_vtbl.read = rx_read_request;
    b->rx_vtbl.write = rx_write_request;

    /* TODO: Do we need some state to setup here? */

    assert(!"NYI: block_local_accept_cb");
    return SYS_ERR_OK;
}

/* ----------------------  Channel Initialization  ----------------------- */

/**
 * \brief callback for interfrace export
 */
static void block_local_export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        /* TODO: Error handling */
        server_state = SERVICE_STATE_FAILURE;
    }

    server_iref = iref;

    server_state = SERVICE_STATE_EXPORTED;
}

/**
 * \brief initializes the machine local block server
 */
errval_t block_local_init(uint32_t flags)
{
    errval_t err;

    is_client = (flags & SERVICE_FLAG_CLIENT) && 1;

    // initialize worker thread pool

    // export the interface
    err = block_service_export(NULL, block_local_export_cb,
                    block_local_accept_cb, get_default_waitset(),
                    IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        /* TODO: Error handling */
    }
    // start new thread for accepting flounder connections

    assert(!"NYI: block_local_init");
    return SYS_ERR_OK;
}

/* -------------------------  Server Management  ------------------------- */

/**
 * \brief starts the machine local server of block service to accept requests
 *
 * This function should not return until stopped.
 */
errval_t block_local_start(void)
{
    // start listen to connections on the flounder channel

    if (server_state != SERVICE_STATE_EXPORTED) {
        /* TODO: ERROR */
    }

    /* TODO: give the service a name
     *  -> Must it have a system wide unique name i.e. two services
     *     with the same name running on two different hosts?
     */
    errval_t err = nameservice_register("TODO: NAME", server_iref);
    if (err_is_fail(err)) {
        /* TODO: Error handling */
    }

    /*
     * TODO: message main loop
     */
    assert(!"NYI: block_local_start");
    return SYS_ERR_OK;
}
/**
 * \brief stops the request handling of the machine local block service requests
 */
errval_t block_local_stop(void)
{
    // set the stop flag.

    // tear down all bulk channels

    // stop the flounder service

    // free up resources

    assert(!"NYI: block_local_stop");
    return SYS_ERR_OK;
}

