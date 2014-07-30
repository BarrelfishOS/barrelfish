/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <flounder/flounder_txqueue.h>
#include <if/dma_defs.h>

#include <dma_internal.h>
#include <dma/dma_service.h>

#include <debug.h>

/**
 * DMA service state that will be assigned to the
 */
struct dma_svc_st
{
    void *usr_st;
    struct tx_queue queue;
    struct dma_binding *binding;
};

/**
 * enumration of all possible states of the service exportation process
 */
enum dma_svc_state
{
    DMA_SVC_STATE_INVALID,
    DMA_SVC_STATE_EXPORTING,
    DMA_SVC_STATE_EXPORT_OK,
    DMA_SVC_STATE_EXPORT_FAIL,
    DMA_SVC_STATE_NS_REGISTERING,
    DMA_SVC_STATE_NS_REGISTER_OK,
    DMA_SVC_STATE_NS_REGISTER_FAIL,
    DMA_SVC_STATE_RUNNING
};

/// represents the current state of the exporting process
static enum dma_svc_state dma_svc_state = DMA_SVC_STATE_EXPORTING;

/// error while exporting
static errval_t dma_svc_err;

/// our own iref of the exported service
static iref_t dma_svc_iref;

/// registered callbacks
struct dma_service_cb *event_handlers;

/*
 * ----------------------------------------------------------------------------
 * Reply State Cache
 * ----------------------------------------------------------------------------
 */

struct dma_svc_reply_st
{
    struct txq_msg_st common;
    /* union of arguments */
    union
    {
        /* request handling */
        struct
        {
            dma_id_t id;            ///<
        } request;
    } args;
};

/*
 * ----------------------------------------------------------------------------
 * Memory:  registration
 * ----------------------------------------------------------------------------
 */

static errval_t dma_register_response_tx(struct txq_msg_st *msg_st)
{
    struct dma_binding *b = msg_st->queue->binding;

    return dma_register_response__tx(b, TXQCONT(msg_st), msg_st->err);
}

static void dma_register_call_rx(struct dma_binding *_binding,
                                 struct capref memory)
{
    dma_svc_handle_t svc_handle = _binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_handle->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = dma_register_response_tx;

    if (event_handlers->addregion) {
        event_handlers->addregion(svc_handle, memory);
    } else {
        msg_st->err = DMA_ERR_SVC_REJECT;
    }

    txq_send(msg_st);
}

/*
 * ----------------------------------------------------------------------------
 * Memory: de-registration
 * ----------------------------------------------------------------------------
 */

static errval_t dma_deregister_response_tx(struct txq_msg_st *msg_st)
{
    struct dma_binding *b = msg_st->queue->binding;

    return dma_deregister_response__tx(b, TXQCONT(msg_st), msg_st->err);
}

static void dma_deregister_call_rx(struct dma_binding *_binding,
                                   struct capref memory)
{
    dma_svc_handle_t svc_handle = _binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_handle->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = dma_deregister_response_tx;

    if (event_handlers->removeregion) {
        event_handlers->removeregion(svc_handle, memory);
    } else {
        msg_st->err = DMA_ERR_SVC_REJECT;
    }

    txq_send(msg_st);
}

/*
 * ----------------------------------------------------------------------------
 * Transfer Control: START
 * ----------------------------------------------------------------------------
 */

static errval_t dma_memcpy_response_tx(struct txq_msg_st *msg_st)
{
    struct dma_svc_reply_st *st = (struct dma_svc_reply_st *) msg_st;
    struct dma_binding *b = msg_st->queue->binding;

    return dma_memcpy_response__tx(b, TXQCONT(msg_st), msg_st->err,
                                   st->args.request.id);
}

static void dma_memcpy_call_rx(struct dma_binding *_binding,
                               uint64_t src,
                               uint64_t dst,
                               uint64_t length)
{

    DMASVC_DEBUG("memcopy request [0x%016lx]->[0x%016lx] of size 0x%lx\n", src, dst,
                 length);

    dma_svc_handle_t svc_handle = _binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_handle->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->send = dma_memcpy_response_tx;

    struct dma_svc_reply_st *reply = (struct dma_svc_reply_st *) msg_st;

    if (event_handlers->memcpy) {
        msg_st->err = event_handlers->memcpy(svc_handle, dst, src, length,
                                             &reply->args.request.id);
    } else {
        msg_st->err = DMA_ERR_SVC_REJECT;
    }

    txq_send(msg_st);
}

struct dma_rx_vtbl dma_rx_vtbl = {
    .register_call = dma_register_call_rx,
    .deregister_call = dma_deregister_call_rx,
    .memcpy_call = dma_memcpy_call_rx,
};

/*
 * ----------------------------------------------------------------------------
 * Transmission of done notifications
 * ----------------------------------------------------------------------------
 */

static errval_t dma_done_tx(struct txq_msg_st *msg_st)
{
    struct dma_svc_reply_st *st = (struct dma_svc_reply_st *) msg_st;
    struct dma_binding *b = msg_st->queue->binding;

    return dma_done__tx(b, TXQCONT(msg_st), st->args.request.id, msg_st->err);
}

/*
 * ----------------------------------------------------------------------------
 * Service export and connect handling
 * ----------------------------------------------------------------------------
 */

struct export_arg
{
    errval_t err;
    void *user_st;
};


static errval_t svc_connect_cb(void *st,
                               struct dma_binding *binding)
{
    errval_t err;

    DMASVC_DEBUG("New connection to the DMA service\n");

    if (event_handlers->connect == NULL) {
        /* the service is not interested in new connections (anymore) */
        return DMA_ERR_SVC_REJECT;
    }

    struct dma_svc_st *state = malloc(sizeof(*state));
    if (state == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    txq_init(&state->queue, binding, binding->waitset,
             (txq_register_fn_t) binding->register_send,
             sizeof(struct dma_svc_reply_st));

    err = event_handlers->connect(st, &state->usr_st);
    if (err_is_fail(err)) {
        /* reject the connection */
        DMASVC_DEBUG("application rejected the connection: %s\n",
                     err_getstring(err));
        free(state);
        return DMA_ERR_SVC_REJECT;
    }

    state->binding = binding;
    binding->st = state;
    binding->rx_vtbl = dma_rx_vtbl;

    return SYS_ERR_OK;
}

static void svc_export_cb(void *st,
                          errval_t err,
                          iref_t iref)
{
    dma_svc_err = err;

    if (err_is_fail(err)) {
        dma_svc_state = DMA_SVC_STATE_EXPORT_FAIL;
        return;
    }

    dma_svc_iref = iref;
    dma_svc_state = DMA_SVC_STATE_EXPORT_OK;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 * \brief initializes the DMA service and registers with the DMA manager
 *
 * \param cb        Callback function pointers
 * \param arg       Argument passed to the connect callback
 * \param svc_iref  Returns the exported iref
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_service_init(struct dma_service_cb *cb,
                          void *arg,
                          iref_t *svc_iref)
{
    errval_t err;

    DMASVC_DEBUG("Initializing DMA service...\n");


    err = dma_export(arg, svc_export_cb, svc_connect_cb,
                     get_default_waitset(),
                     IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (dma_svc_state == DMA_SVC_STATE_EXPORTING) {
        messages_wait_and_handle_next();
    }

    if (dma_svc_state == DMA_SVC_STATE_EXPORT_FAIL) {
        return dma_svc_err;
    }

    dma_svc_state = DMA_SVC_STATE_RUNNING;
    event_handlers = cb;

    if (svc_iref) {
        *svc_iref = dma_svc_iref;
    }

    DMASVC_DEBUG("DMA service up and running.\n");

    return SYS_ERR_OK;
}

/**
 * \brief initializes the DMA service and exports it to the nameservice
 *
 * \param svc_name  The name of the service for nameservice registration
 * \param cb        Callback function pointers
 * \param arg       Argument passed to the connect callback
 * \param svc_iref  Returns the exported iref
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_service_init_with_name(char *svc_name,
                                    struct dma_service_cb *cb,
                                    void *arg,
                                    iref_t *svc_iref)
{
    errval_t err;

    DMASVC_DEBUG("Initializing DMA service...\n");

    err = dma_export(arg, svc_export_cb, svc_connect_cb,
                     get_default_waitset(),
                     IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (dma_svc_state == DMA_SVC_STATE_EXPORTING) {
        messages_wait_and_handle_next();
    }

    if (dma_svc_state == DMA_SVC_STATE_EXPORT_FAIL) {
        return dma_svc_err;
    }

    dma_svc_state = DMA_SVC_STATE_NS_REGISTERING;

    DMASVC_DEBUG("Registering service [%s] with iref [0x%"PRIxIREF"]\n", svc_name,
                 dma_svc_iref);

    err = nameservice_register(svc_name, dma_svc_iref);
    if (err_is_fail(err)) {
        dma_svc_state = DMA_SVC_STATE_NS_REGISTER_FAIL;
        return err;
    }

    event_handlers = cb;
    *svc_iref = dma_svc_iref;

    DMASVC_DEBUG("DMA service up and running.\n");

    dma_svc_state = DMA_SVC_STATE_RUNNING;

    return SYS_ERR_OK;
}

/**
 * \brief sends a done notification about the transfer that has completed
 *
 * \param binding   DMA binding
 * \param err       Outcome of the transfer
 * \param id        The id of the completed transfer
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_service_send_done(dma_svc_handle_t svc_handle,
                               errval_t err,
                               dma_req_id_t id)
{
    struct txq_msg_st *msg_st = txq_msg_st_alloc(&svc_handle->queue);
    if (msg_st == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    msg_st->err = err;
    msg_st->send = dma_done_tx;

    struct dma_svc_reply_st *reply = (struct dma_svc_reply_st *) msg_st;

    reply->args.request.id = id;

    txq_send(msg_st);

    return SYS_ERR_OK;
}
