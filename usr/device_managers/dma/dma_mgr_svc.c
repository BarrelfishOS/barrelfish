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
#include <flounder/flounder_txqueue.h>
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
    struct txq_msg_st common;

    /* union of arguments */
    union
    {
        struct dma_mgr_driver_info *lookup;
    } args;
};

/*
 * ----------------------------------------------------------------------------
 * Driver Registration
 * ----------------------------------------------------------------------------
 */

static errval_t svc_register_response_tx(struct txq_msg_st *msg_st)
{

    return dma_mgr_register_driver_response__tx(msg_st->queue->binding,
                                                TXQCONT(msg_st), msg_st->err);

}

static void svc_register_call_rx(struct dma_mgr_binding *_binding,
                                 uint64_t mem_low,
                                 uint64_t mem_high,
                                 uint8_t numa_node,
                                 uint8_t type,
                                 iref_t iref)
{
    errval_t err;

    SVC_DEBUG("register call: [%016lx, %016lx]\n", mem_low, mem_high);

    struct tx_queue *txq = _binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(txq);
    if (msg_st == NULL) {
        USER_PANIC("could not allocate reply state\n");
    }

    msg_st->send = svc_register_response_tx;

    msg_st->err = driver_store_insert(mem_low, mem_high, numa_node, type, iref);

    if (err_is_ok(msg_st->err)) {
        char buf[30];
        snprintf(buf, 30, "%s_%u_%u", DMA_MGR_REGISTERED_DRIVER, type, numa_node);
        SVC_DEBUG("registering with nameservice {%s}\n", buf);
        err = nameservice_register(buf, 0x123);
        if (err_is_fail(err)) {
            SVC_DEBUG("registering failed: %s\n", err_getstring(err));
        }
    }

    txq_send(msg_st);
}

/*
 * ----------------------------------------------------------------------------
 * Query for DMA driver service
 * ----------------------------------------------------------------------------
 */

static errval_t svc_lookup_response_tx(struct txq_msg_st *msg_st)
{
    struct svc_reply_st *st = (struct svc_reply_st *) msg_st;
    struct dma_mgr_driver_info *di = st->args.lookup;

    if (err_is_fail(msg_st->err)) {
        return dma_mgr_lookup_driver_response__tx(msg_st->queue->binding,
                                                  TXQCONT(msg_st), msg_st->err, 0, 0,
                                                  0, 0, 0);
    } else {
        assert(di);
        return dma_mgr_lookup_driver_response__tx(msg_st->queue->binding,
                                                  TXQCONT(msg_st), msg_st->err,
                                                  di->mem_low, di->mem_high,
                                                  di->numa_node, di->type, di->iref);
    }
}

static errval_t svc_lookup_by_iref_response_tx(struct txq_msg_st *msg_st)
{
    struct svc_reply_st *st = (struct svc_reply_st *) msg_st;

    struct dma_mgr_driver_info *di = st->args.lookup;

    if (err_is_fail(msg_st->err)) {
        return dma_mgr_lookup_driver_by_iref_response__tx(msg_st->queue->binding,
                                                          TXQCONT(msg_st),
                                                          msg_st->err, 0, 0, 0, 0);
    } else {
        assert(di);
        return dma_mgr_lookup_driver_by_iref_response__tx(msg_st->queue->binding,
                                                          TXQCONT(msg_st),
                                                          msg_st->err, di->mem_low,
                                                          di->mem_high,
                                                          di->numa_node, di->type);
    }
}

static void svc_lookup_call_rx(struct dma_mgr_binding *_binding,
                               uint64_t addr,
                               uint64_t size,
                               uint8_t numa_node)
{
    SVC_DEBUG("lookup call: [%016lx, %016lx]\n", addr, size);

    struct tx_queue *txq = _binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(txq);
    if (msg_st == NULL) {
        USER_PANIC("could not allocate reply state\n");
    }

    msg_st->send = svc_lookup_response_tx;

    struct svc_reply_st *st = (struct svc_reply_st *) msg_st;

    msg_st->err = driver_store_lookup(addr, size, numa_node, &st->args.lookup);

    txq_send(msg_st);
}

static void svc_lookup_by_iref_call_rx(struct dma_mgr_binding *_binding,
                                       iref_t iref)
{
    SVC_DEBUG("lookup call by iref: %"PRIxIREF"\n", iref);

    struct tx_queue *txq = _binding->st;

    struct txq_msg_st *msg_st = txq_msg_st_alloc(txq);
    if (msg_st == NULL) {
        USER_PANIC("could not allocate reply state\n");
    }

    msg_st->send = svc_lookup_by_iref_response_tx;

    struct svc_reply_st *st = (struct svc_reply_st *) msg_st;

    msg_st->err = driver_store_lookup_by_iref(iref, &st->args.lookup);

    txq_send(msg_st);
}

struct dma_mgr_rx_vtbl svc_rx_vtbl = {
    .register_driver_call = svc_register_call_rx,
    .lookup_driver_call = svc_lookup_call_rx,
    .lookup_driver_by_iref_call = svc_lookup_by_iref_call_rx
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

    struct tx_queue *txq = calloc(1, sizeof(*txq));
    if (txq == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    txq_init(txq, binding, binding->waitset,
             (txq_register_fn_t) binding->register_send,
             sizeof(struct svc_reply_st));

    binding->st = txq;

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

