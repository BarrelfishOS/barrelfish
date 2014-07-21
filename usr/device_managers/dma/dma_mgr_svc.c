/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 *
 *
 */
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <dma/dma_manager_client.h>

#include <if/dma_mgr_defs.h>

#include "dma_mgr.h"
#include "debug.h"

/**
 * enumration of all possible states of the service exportation process
 */
enum svc_state
{
    SVC_STATE_EXPORTING,
    SVC_STATE_EXPORT_OK,
    SVC_STATE_EXPORT_FAIL,
    SVC_STATE_NS_REGISTERING,
    SVC_STATE_NS_REGISTER_OK,
    SVC_STATE_NS_REGISTER_FAIL,
    SVC_STATE_RUNNING
};

/// represents the current state of the exporting process
static enum svc_state dma_svc_state = SVC_STATE_EXPORTING;

/// our own iref of the exported service
static iref_t svc_iref;

/*
 * ----------------------------------------------------------------------------
 * Reply State Cache
 * ----------------------------------------------------------------------------
 */
struct svc_reply_st
{
    void (*op)(void *arg);          ///<
    struct svc_reply_st *next;      ///<
    struct dma_mgr_binding *b;      ///<
    errval_t err;                   ///<

    /* union of arguments */
    union
    {
        struct dma_mgr_driver_info *lookup;
    } args;
};

static struct svc_reply_st *dma_reply_st_cache = NULL;

static struct svc_reply_st *reply_st_alloc(void)
{
    if (dma_reply_st_cache) {
        struct svc_reply_st *st = dma_reply_st_cache;
        dma_reply_st_cache = dma_reply_st_cache->next;
        return st;
    }
    return calloc(1, sizeof(struct svc_reply_st));
}

static inline void reply_st_free(struct svc_reply_st *st)
{
    st->next = dma_reply_st_cache;
    dma_reply_st_cache = st;
}

/*
 * ----------------------------------------------------------------------------
 * Reply send queue
 * ----------------------------------------------------------------------------
 */

static struct
{
    struct svc_reply_st *head;
    struct svc_reply_st *tail;
} svc_reply_txq;

static void send_reply_cont(void *a)
{
    struct svc_reply_st *st = (struct svc_reply_st *) a;

    /* we are likely to be able to send the reply so send it */
    assert(svc_reply_txq.head == st);

    svc_reply_txq.head->op(st);
}

static void send_reply_done(void *a)
{
    struct svc_reply_st *st = (struct svc_reply_st *) a;

    /* callback when the message has been sent */
    if (svc_reply_txq.head == NULL) {
        reply_st_free((struct svc_reply_st *) a);
        return;
    }
    assert(svc_reply_txq.head == st);

    svc_reply_txq.head = svc_reply_txq.head->next;
    if (svc_reply_txq.head == NULL) {
        svc_reply_txq.tail = NULL;
    } else {
        struct svc_reply_st *next = svc_reply_txq.head;
        /* it should not matter if we already registered */
        svc_reply_txq.head->b->register_send(svc_reply_txq.head->b,
                                             get_default_waitset(),
                                             MKCONT(send_reply_cont, next));
    }
    reply_st_free(st);
}

static errval_t send_reply_enqueue(struct svc_reply_st *st)
{
    st->next = NULL;

    if (svc_reply_txq.tail == NULL) {
        svc_reply_txq.head = st;
    } else {
        svc_reply_txq.tail->next = st;
    }
    svc_reply_txq.tail = st;

    /* register if queue was empty i.e. head == tail */
    if (svc_reply_txq.tail == svc_reply_txq.head) {
        return st->b->register_send(st->b, get_default_waitset(),
                                    MKCONT(send_reply_cont, NULL));
    }

    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Driver Registration
 * ----------------------------------------------------------------------------
 */

static void svc_register_response_tx(void *a)
{
    errval_t err;

    struct svc_reply_st *st = a;

    err = dma_mgr_register_driver_response__tx(st->b, MKCONT(send_reply_done, a),
                                               st->err);
    switch (err_no(err)) {
        case SYS_ERR_OK:
            break;
        case FLOUNDER_ERR_TX_BUSY:
            err = send_reply_enqueue(st);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not send reply");
            }
            break;
        default:
            USER_PANIC_ERR(err, "could not send reply");
            break;
    }
}

static void svc_register_call_rx(struct dma_mgr_binding *_binding,
                                 uint64_t mem_low,
                                 uint64_t mem_high,
                                 uint8_t numa_node,
                                 uint8_t type,
                                 iref_t iref)
{
    SVC_DEBUG("register call: [%016lx, %016lx]\n", mem_low, mem_high);

    struct svc_reply_st *state = reply_st_alloc();
    assert(state);

    state->b = _binding;
    state->op = svc_register_response_tx;

    state->err = driver_store_insert(mem_low, mem_high, numa_node, type, iref);

    svc_register_response_tx(state);
}

/*
 * ----------------------------------------------------------------------------
 * Query for DMA driver service
 * ----------------------------------------------------------------------------
 */

static void svc_lookup_response_tx(void *a)
{
    errval_t err;

    struct svc_reply_st *st = a;

    struct dma_mgr_driver_info *di = st->args.lookup;

    if (err_is_fail(st->err)) {
        err = dma_mgr_lookup_driver_response__tx(st->b, MKCONT(send_reply_done, a),
                                                 st->err, 0, 0, 0, 0, 0);
    } else {
        err = dma_mgr_lookup_driver_response__tx(st->b, MKCONT(send_reply_done, a),
                                                 st->err, di->mem_low, di->mem_high,
                                                 di->numa_node, di->type, di->iref);
    }

    switch (err_no(err)) {
        case SYS_ERR_OK:
            break;
        case FLOUNDER_ERR_TX_BUSY:
            err = send_reply_enqueue(st);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not send reply");
            }
            break;
        default:
            USER_PANIC_ERR(err, "could not send reply");
            break;
    }
}

static void svc_lookup_call_rx(struct dma_mgr_binding *_binding,
                               uint64_t addr,
                               uint64_t size,
                               uint8_t numa_node)
{
    SVC_DEBUG("lookup call: [%016lx, %016lx]\n", addr, size);

    struct svc_reply_st *st = reply_st_alloc();
    assert(st);

    st->b = _binding;
    st->op = svc_lookup_response_tx;

    st->err = driver_store_lookup(addr, size, numa_node, &st->args.lookup);

    svc_lookup_response_tx(st);
}

struct dma_mgr_rx_vtbl svc_rx_vtbl = {
    .register_driver_call = svc_register_call_rx,
    .lookup_driver_call = svc_lookup_call_rx,
};

/*
 * ----------------------------------------------------------------------------
 * Service export and connect handling
 * ----------------------------------------------------------------------------
 */

static errval_t svc_connect_cb(void *st,
                               struct dma_mgr_binding *binding)
{
    SVC_DEBUG("New connection to the DMA service\n");

    binding->rx_vtbl = svc_rx_vtbl;

    return SYS_ERR_OK;
}

static void svc_export_cb(void *st,
                          errval_t err,
                          iref_t iref)
{
    *((errval_t *) st) = err;

    if (err_is_fail(err)) {
        dma_svc_state = SVC_STATE_EXPORT_FAIL;
        return;
    }

    svc_iref = iref;
    dma_svc_state = SVC_STATE_EXPORT_OK;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 * \brief initializes the DMA manager service service
 *
 * \param svc_name  The name of the service for nameservice registration
 * \param cb        Callback function pointers
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_mgr_svc_start(void)
{
    errval_t err, export_err;

    SVC_DEBUG("Initializing DMA service...\n");

    svc_reply_txq.head = NULL;
    svc_reply_txq.tail = NULL;

    err = dma_mgr_export(&export_err, svc_export_cb, svc_connect_cb,
                         get_default_waitset(),
                         IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (dma_svc_state == SVC_STATE_EXPORTING) {
        messages_wait_and_handle_next();
    }

    if (dma_svc_state == SVC_STATE_EXPORT_FAIL) {
        return export_err;
    }

    dma_svc_state = SVC_STATE_NS_REGISTERING;

    SVC_DEBUG("Registering service [%s] with iref [0x%"PRIxIREF"]\n",
              DMA_MGR_SVC_NAME, svc_iref);

    err = nameservice_register(DMA_MGR_SVC_NAME, svc_iref);
    if (err_is_fail(err)) {
        dma_svc_state = SVC_STATE_NS_REGISTER_FAIL;
        return err;
    }

    dma_svc_state = SVC_STATE_RUNNING;

    SVC_DEBUG("DMA service up and running.\n");

    return SYS_ERR_OK;
}

