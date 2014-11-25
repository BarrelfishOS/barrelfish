/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <flounder/flounder_txqueue.h>

#include <if/dma_defs.h>

#include <client/dma_client_internal.h>
#include <client/dma_client_device_internal.h>
#include <client/dma_client_channel_internal.h>
#include <client/dma_client_request_internal.h>

#include <debug.h>

struct dma_client_channel
{
    struct dma_channel common;
    struct dma_binding *binding;     ///< the DMA service binding
    dma_client_st_t client_st;       ///< state of the service binding
    errval_t error;                  ///< stores the connection error
    struct tx_queue txq;             ///< Flounder TX Queue
    uint8_t wait_reply;              ///< RPC is in progress
    struct dma_request *rpc;         ///< currently pending requests
    dma_req_id_t last_done_id;       ///<
    struct waitset *ws;              ///< waitset to be polled
};

struct svc_msg_st
{
    struct txq_msg_st common;

    /* union of arguments */
    union
    {
        struct capref cap;
        struct dma_request *req;
    } args;
};

/*
 * ---------------------------------------------------------------------------
 * RPC management
 * ---------------------------------------------------------------------------
 */

/**
 * \brief starts a new RPC to the DMA service
 *
 * \param chan  the DMA channel to start the RPC on
 *
 * \returns 1 if the RPC transaction could be started
 *          0 if there was already a transaction in process
 */
static inline uint8_t rpc_start(struct dma_client_channel *chan)
{
    if (!chan->wait_reply) {
        chan->wait_reply = 0x1;
        return 1;
    }
    return 0;
}

/**
 * \brief waits until the started transaction is finished
 *
 * \param chan  the DMA channel
 */
static inline void rpc_wait_done(struct dma_client_channel *chan)
{
    while (chan->wait_reply) {
        messages_wait_and_handle_next();
    }
}

/**
 * \brief signals the completion of the RPC
 *
 * \param chan  the DMA channel
 */
static inline void rpc_done(struct dma_client_channel *chan)
{
    chan->wait_reply = 0x0;
}

/*
 *
 */
static errval_t dma_client_channel_poll(struct dma_channel *chan)
{
    struct dma_client_channel *cc = (struct dma_client_channel *)chan;

    return event_dispatch(cc->ws);
}

/*
 * ----------------------------------------------------------------------------
 * Message Receive Handlers
 * ----------------------------------------------------------------------------
 */

static void done_msg_rx(struct dma_binding *_binding,
                        dma_id_t id,
                        dma_errval_t msgerr)
{
    struct dma_client_channel *chan = _binding->st;

    CLIENTCHAN_DEBUG("done_msg_rx: %lx %s\n", chan->common.id, id,
                     err_getstring(msgerr));

    struct dma_request *req = dma_channel_deq_request_by_id(&chan->common, id);
    /*
     * XXX: workaround. it may be the case that the done message was handled
     *      earlier than than the RPC reply.
     */
    if (req == NULL) {
        assert(chan->last_done_id == 0);
        assert(chan->rpc);
        chan->last_done_id = id;
        chan->error = msgerr;
        if (err_is_fail(msgerr)) {
            chan->rpc->state = DMA_REQ_ST_ERR;
        } else {
            chan->rpc->state = DMA_REQ_ST_DONE;
        }
        return;
    }

    if (req->setup.done_cb) {
        req->setup.done_cb(msgerr, id, req->setup.cb_arg);
    }

    dma_client_request_free((struct dma_client_request *) req);
}

static void register_response_rx(struct dma_binding *_binding,
                                 dma_errval_t msgerr)
{
    struct dma_client_channel *chan = _binding->st;

    CLIENTCHAN_DEBUG("register_response_rx: %s\n", chan->common.id,
                     err_getstring(msgerr));

    chan->error = msgerr;
    if (err_is_fail(msgerr)) {
        chan->rpc->state = DMA_REQ_ST_ERR;
    } else {
        chan->rpc->state = DMA_REQ_ST_DONE;
    }
    rpc_done(chan);
}

static void deregister_response_rx(struct dma_binding *_binding,
                                   dma_errval_t msgerr)
{
    struct dma_client_channel *chan = _binding->st;
    chan->error = msgerr;

    CLIENTCHAN_DEBUG("deregister_response_rx: %s\n", chan->common.id,
                     err_getstring(msgerr));

    if (err_is_fail(msgerr)) {
        chan->rpc->state = DMA_REQ_ST_ERR;
    } else {
        chan->rpc->state = DMA_REQ_ST_DONE;
    }
    rpc_done(chan);
}

static void memcpy_response_rx(struct dma_binding *_binding,
                               dma_errval_t err,
                               dma_id_t id)
{

    struct dma_client_channel *chan = _binding->st;
    chan->error = err;

    CLIENTCHAN_DEBUG("memcpy_response_rx: id=%016lx, err: %s\n",
                     chan->common.id, id, err_getstring(err));

    if (err_is_fail(err)) {
        chan->rpc->state = DMA_REQ_ST_ERR;
    } else {
        chan->rpc->state = DMA_REQ_ST_SUBMITTED;
    }
    chan->rpc->id = id;
    rpc_done(chan);
}

static struct dma_rx_vtbl dma_rxvtbl = {
    .register_response = register_response_rx,
    .deregister_response = deregister_response_rx,
    .memcpy_response = memcpy_response_rx,
    .done = done_msg_rx
};

/*
 * ----------------------------------------------------------------------------
 * Message Send Handlers
 * ----------------------------------------------------------------------------
 */

static errval_t dma_client_channel_register_call_tx(struct txq_msg_st *msg_st)
{
    struct svc_msg_st *st = (struct svc_msg_st *) msg_st;

    return dma_register_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                 st->args.cap);
}

static errval_t dma_client_channel_deregister_call_tx(struct txq_msg_st *msg_st)
{
    struct svc_msg_st *st = (struct svc_msg_st *) msg_st;

    return dma_deregister_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                   st->args.cap);
}

static errval_t dma_client_channel_memcpy_call_tx(struct txq_msg_st *msg_st)
{
    struct svc_msg_st *st = (struct svc_msg_st *) msg_st;
    struct dma_req_setup *setup = &st->args.req->setup;

    return dma_memcpy_call__tx(msg_st->queue->binding, TXQCONT(msg_st),
                               setup->args.memcpy.src, setup->args.memcpy.dst,
                               setup->args.memcpy.bytes);
}

/*
 * ---------------------------------------------------------------------------
 * Service Binding
 * ---------------------------------------------------------------------------
 */

static void chan_init_bind_cb(void *st,
                              errval_t err,
                              struct dma_binding *b)
{
    struct dma_client_channel *chan = st;

    if (err_is_fail(err)) {
        CLIENTCHAN_DEBUG("chan_init_bind_cb: connection to service failed.\n",
                         chan->common.id);
        chan->client_st = DMA_CLIENT_STATE_BIND_FAIL;
        return;
    }

    txq_init(&chan->txq, b, b->waitset, (txq_register_fn_t) b->register_send,
             sizeof(struct svc_msg_st));

    chan->binding = b;
    chan->client_st = DMA_CLIENT_STATE_BIND_OK;

    b->st = chan;
    b->rx_vtbl = dma_rxvtbl;

    CLIENTCHAN_DEBUG("bind done. connection to service established.\n",
                     chan->common.id);
}

/*
 * ============================================================================
 * Library Internal Interface
 * ============================================================================
 */

/**
 * \brief initializes and allocates resources for a new channel DMA channel
 *        belonging to a device
 *
 * \param dev           DMA client device
 * \param id            id of this channel
 * \param driver_iref   iref to the driver service
 * \param ret_chan      returned channel pointer
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_channel_init(struct dma_client_device *dev,
                                 uint8_t id,
                                 iref_t driver_iref,
                                 struct dma_client_channel **ret_chan)
{
    errval_t err;

    struct dma_client_channel *chan = calloc(1, sizeof(*chan));
    if (chan == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct waitset *ws = get_default_waitset();

    struct dma_device *dma_dev = (struct dma_device *) dev;

    chan->common.id = dma_channel_id_build(dma_device_get_id(dma_dev), id);
    chan->common.state = DMA_CHAN_ST_RUNNING;
    chan->common.device = dma_dev;
    chan->common.f.memcpy = dma_client_request_memcpy_chan;
    chan->common.f.poll = dma_client_channel_poll;
    chan->ws = ws;

    chan->client_st = DMA_CLIENT_STATE_BINDING;

    CLIENTCHAN_DEBUG("initializing channel with iref %"PRIxIREF". binding...\n",
                     chan->common.id, driver_iref);

    err = dma_bind(driver_iref, chan_init_bind_cb, chan, ws, IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        free(chan);
        return err;
    }

    while (chan->client_st == DMA_CLIENT_STATE_BINDING) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            return err;
        }
    }

    if (chan->client_st == DMA_CLIENT_STATE_BIND_FAIL) {
        return chan->error;
    }

    chan->client_st = DMA_CLIENT_STATE_CONNECTED;

    *ret_chan = chan;

    return SYS_ERR_OK;
}

/**
 * \brief enqueues a request onto the DMA client channel and sends it to the
 *        DMA driver service
 *
 * \param chan  DMA client channel
 * \param req   DMA client request
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t dma_client_channel_submit_request(struct dma_channel *chan,
                                           struct dma_request *req)
{
    assert(chan->device->type == DMA_DEV_TYPE_CLIENT);

    struct dma_client_channel *cl_chan = (struct dma_client_channel *) chan;
    struct dma_client_request *cl_req = (struct dma_client_request *) req;

    if (cl_chan->binding == NULL) {
        return DMA_ERR_SVC_NO_CONNECTION;
    }

    if (!rpc_start(cl_chan)) {
        return DMA_ERR_SVC_BUSY;
    }

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&cl_chan->txq);
    if (msg_st == NULL) {
        rpc_done(cl_chan);
        return LIB_ERR_MALLOC_FAIL;
    }

    struct svc_msg_st *svc_st = (struct svc_msg_st *) msg_st;

    cl_chan->rpc = req;

    switch (req->type) {
        case DMA_REQ_TYPE_MEMCPY:
            CLIENTCHAN_DEBUG("submitting new memcpy request\n", chan->id);
            msg_st->send = dma_client_channel_memcpy_call_tx;
            svc_st->args.req = req;
            break;
        case DMA_REQ_TYPE_MEM_REGISTER:
            CLIENTCHAN_DEBUG("submitting new memory register request\n", chan->id);
            msg_st->send = dma_client_channel_register_call_tx;
            svc_st->args.cap = cl_req->cap;
            break;
        case DMA_REQ_TYPE_MEM_REMOVE:
            CLIENTCHAN_DEBUG("submitting new memory remove request\n", chan->id);
            msg_st->send = dma_client_channel_deregister_call_tx;
            svc_st->args.cap = cl_req->cap;
            break;
        default:
            CLIENTCHAN_DEBUG("unsupported request\n", chan->id);
            txq_msg_st_free(msg_st);
            rpc_done(cl_chan);
            return DMA_ERR_REQUEST_UNSUPPORTED;
            break;
    }

    txq_send(msg_st);

    rpc_wait_done(cl_chan);

    if (req->state == DMA_REQ_ST_ERR) {
        CLIENTCHAN_DEBUG("request ended up in error state: %s", chan->id,
                         err_getstring(cl_chan->error));
        return cl_chan->error;
    }

    switch (req->type) {
        case DMA_REQ_TYPE_MEMCPY:
            if (cl_chan->last_done_id == req->id) {
                CLIENTCHAN_DEBUG("request has already been completed", chan->id);
                cl_chan->last_done_id = 0x0;
                req->state = DMA_REQ_ST_DONE;
                return SYS_ERR_OK;
            } else {
                req->state = DMA_REQ_ST_SUBMITTED;
            }
            CLIENTCHAN_DEBUG("request %016lx is enqueued on list\n", chan->id,
                             req->id);
            dma_channel_enq_request_tail(chan, req);
            break;
        default:
            return cl_chan->error;
            break;
    }

    return SYS_ERR_OK;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */
