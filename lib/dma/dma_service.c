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

#include <if/dma_defs.h>

#include <dma/dma.h>
#include <dma/dma_service.h>

#include <debug.h>

/**
 * DMA service state that will be assigned to the
 */
struct dma_svc_st
{
    struct dma_mem_mgr *mem_mgr;
    void *usr_st;
};

/**
 * enumration of all possible states of the service exportation process
 */
enum dma_svc_state
{
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

/// our own iref of the exported service
static iref_t dma_svc_iref;

/// registered callbacks
struct dma_service_cb *event_handlers;

/*
 * ----------------------------------------------------------------------------
 * Reply Queue
 * ----------------------------------------------------------------------------
 */
#ifdef XDMA_USE_QUEUE
#define XDMA_REPLY_QUEUE_SIZE 32

struct queue_entry
{
    void *st;
    void (*op)(void *st);
    struct queue_entry *next;
};

struct xdma_reply_queue
{
    struct queue_entry *head;
    struct queue_entry *tail;
};

struct xdma_reply_queue send_queue;

struct queue_entry *send_queue_entries;

struct queue_entry *send_queue_free;

static errval_t xdma_reply_queue_send(void (*op)(void *st),
                void *st)
{
    assert(op);
    if (send_queue.head == NULL) {
        op(st);
        return SYS_ERR_OK;
    }
    if (send_queue_free == NULL) {
        return FLOUNDER_ERR_TX_BUSY;
    }
    struct queue_entry *e = send_queue_free;
    send_queue_free = e->next;
    e->op = op;
    e->st = st;
    e->next = NULL;
    send_queue.tail->next = e;
    send_queue.tail = e;
    return SYS_ERR_OK;
}

static errval_t xdma_reply_queue_insert(void (*op)(void *st),
                void *st)
{
    if (send_queue_free == NULL) {
        return FLOUNDER_ERR_TX_BUSY;
    }
    struct queue_entry *e = send_queue_free;
    send_queue_free = e->next;
    e->op = op;
    e->st = st;
    e->next = NULL;
    if (send_queue.head == NULL) {
        send_queue.head = e;
        send_queue.tail = e;
    } else {
        send_queue.tail->next = e;
        send_queue.tail = e;
    }

    return SYS_ERR_OK;
}

static void xdma_reply_queue_exec(void)
{
    if (send_queue.head == NULL) {
        return;
    }

    struct queue_entry *e = send_queue.head;
    if (e == send_queue.tail) {
        send_queue.tail = NULL;
        send_queue.head = NULL;
    } else {
        assert(e->next);
        send_queue.head = e->next;
    }

    e->op(e->st);

    e->next = send_queue_free;
    send_queue_free = e;
}

static void xdma_reply_queue_next(void *a)
{
    free(a);
    xdma_reply_queue_exec();
}
#endif

/*
 * ----------------------------------------------------------------------------
 * Reply State Cache
 * ----------------------------------------------------------------------------
 */
struct dma_svc_reply_st
{
    void (*op)(void *arg);          ///<
    struct dma_svc_reply_st *next;  ///<
    struct dma_binding *b;          ///<
    errval_t err;                   ///<

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

static struct dma_svc_reply_st *dma_reply_st_cache = NULL;

static struct dma_svc_reply_st *reply_st_alloc(void)
{
    if (dma_reply_st_cache) {
        struct dma_svc_reply_st *st = dma_reply_st_cache;
        dma_reply_st_cache = dma_reply_st_cache->next;
        return st;
    }
    return calloc(1, sizeof(struct dma_svc_reply_st));
}

static inline void reply_st_free(struct dma_svc_reply_st *st)
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
    struct dma_svc_reply_st *head;
    struct dma_svc_reply_st *tail;
} dma_reply_txq;

static void send_reply_cont(void *a)
{
    struct dma_svc_reply_st *st = (struct dma_svc_reply_st *) a;

    /* we are likely to be able to send the reply so send it */
    assert(dma_reply_txq.head == st);

    dma_reply_txq.head->op(st);
}

static void send_reply_done(void *a)
{
    struct dma_svc_reply_st *st = (struct dma_svc_reply_st *) a;

    /* callback when the message has been sent */
    if (dma_reply_txq.head == NULL) {
        reply_st_free((struct dma_svc_reply_st *) a);
        return;
    }
    assert(dma_reply_txq.head == st);

    dma_reply_txq.head = dma_reply_txq.head->next;
    if (dma_reply_txq.head == NULL) {
        dma_reply_txq.tail = NULL;
    } else {
        struct dma_svc_reply_st *next = dma_reply_txq.head;
        /* it should not matter if we already registered */
        dma_reply_txq.head->b->register_send(dma_reply_txq.head->b,
                                             get_default_waitset(),
                                             MKCONT(send_reply_cont, next));
    }
    reply_st_free(st);
}

static errval_t send_reply_enqueue(struct dma_svc_reply_st *st)
{
    st->next = NULL;

    if (dma_reply_txq.tail == NULL) {
        dma_reply_txq.head = st;
    } else {
        dma_reply_txq.tail->next = st;
    }
    dma_reply_txq.tail = st;

    /* register if queue was empty i.e. head == tail */
    if (dma_reply_txq.tail == dma_reply_txq.head) {
        return st->b->register_send(st->b, get_default_waitset(),
                                    MKCONT(send_reply_cont, NULL));
    }

    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Memory:  registration
 * ----------------------------------------------------------------------------
 */

static void dma_register_response_tx(void *a)
{
    errval_t err;

    struct dma_svc_reply_st *st = a;

    err = dma_register_response__tx(st->b, MKCONT(send_reply_done, a), st->err);
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

static void dma_register_call_rx(struct dma_binding *_binding,
                                 struct capref memory)
{
    errval_t err;

    struct dma_svc_st *st = _binding->st;

    err = event_handlers->addregion(st->usr_st, memory);

    struct dma_svc_reply_st *state = reply_st_alloc();
    if (state == NULL) {
        /* todo: error handling */
    }

    state->b = _binding;
    state->err = err;

    dma_register_response_tx(st);
}

/*
 * ----------------------------------------------------------------------------
 * Memory: de-registration
 * ----------------------------------------------------------------------------
 */

static void dma_deregister_response_tx(void *a)
{
    errval_t err;

    struct dma_svc_reply_st *st = a;

    err = dma_deregister_response__tx(st->b, MKCONT(send_reply_done, a), st->err);
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

static void dma_deregister_call_rx(struct dma_binding *_binding,
                                   struct capref memory)
{
    errval_t err;

    err = event_handlers->removeregion(_binding, memory);

    struct dma_svc_reply_st *st = reply_st_alloc();
    assert(st);
    st->b = _binding;
    st->err = err;

    dma_deregister_response_tx(st);
}

/*
 * ----------------------------------------------------------------------------
 * Transfer Control: START
 * ----------------------------------------------------------------------------
 */

static void dma_memcpy_response_tx(void *a)
{
    errval_t err;

    struct dma_svc_reply_st *st = a;

    err = dma_memcpy_response__tx(st->b, MKCONT(send_reply_done, a), st->err,
                                  st->args.request.id);
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

static void dma_memcpy_call_rx(struct dma_binding *_binding,
                               uint64_t src,
                               uint64_t dst,
                               uint64_t length)
{

    DMASVC_DEBUG("memcopy request [0x%016lx]->[0x%016lx] of size 0x%lx\n", src,
                 dst, length);

    struct dma_svc_reply_st *reply = reply_st_alloc();
    if (reply == NULL) {
        USER_PANIC("ran out of reply state resources\n");
    }

    reply->b = _binding;
    reply->op = dma_memcpy_response_tx;

    if (event_handlers->memcpy) {
        reply->err = event_handlers->memcpy(_binding, dst, src, length,
                                            &reply->args.request.id);
    } else {
        reply->err = DMA_ERR_SVC_REJECT;
    }

    dma_memcpy_response_tx(reply);
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

static void dma_done_tx(void *a)
{
    errval_t err;

    struct dma_svc_reply_st *st = a;

    err = dma_done__tx(st->b, MKCONT(send_reply_done, a), st->args.request.id,
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

/*
 * ----------------------------------------------------------------------------
 * Service export and connect handling
 * ----------------------------------------------------------------------------
 */

static errval_t svc_connect_cb(void *st,
                               struct dma_binding *binding)
{
    errval_t err;

    DMASVC_DEBUG("New connection to the DMA service\n");

    if (event_handlers->connect == NULL) {
        /* the service is not interested in new connections (anymore) */
        return DMA_ERR_SVC_REJECT;
    }

    struct dma_svc_st *state = malloc(sizeof(*st));
    if (state == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    err = event_handlers->connect(&state->usr_st);
    if (err_is_fail(err)) {
        /* reject the connection */
        DMASVC_DEBUG("application rejected the connection: %s\n",
                     err_getstring(err));
        return DMA_ERR_SVC_REJECT;
    }

    binding->st = state;
    binding->rx_vtbl = dma_rx_vtbl;

    return SYS_ERR_OK;
}

static void svc_export_cb(void *st,
                          errval_t err,
                          iref_t iref)
{
    *((errval_t *) st) = err;

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
 * \brief initializes the DMA service
 *
 * \param svc_name  The name of the service for nameservice registration
 * \param cb        Callback function pointers
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_service_init(const char *svc_name,
                          struct dma_service_cb *cb)
{
    errval_t err, export_err;

    DMASVC_DEBUG("Initializing DMA service...\n");

    dma_reply_txq.head = NULL;
    dma_reply_txq.tail = NULL;


    err = dma_export(&export_err, svc_export_cb, svc_connect_cb,
                     get_default_waitset(),
                     IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (dma_svc_state == DMA_SVC_STATE_EXPORTING) {
        messages_wait_and_handle_next();
    }

    if (dma_svc_state == DMA_SVC_STATE_EXPORT_FAIL) {
        return export_err;
    }

    dma_svc_state = DMA_SVC_STATE_NS_REGISTERING;

    DMASVC_DEBUG("Registering service [%s] with iref [0x%"PRIxIREF"]\n", svc_name,
                 dma_svc_iref);

    err = nameservice_register(svc_name, dma_svc_iref);
    if (err_is_fail(err)) {
        dma_svc_state = DMA_SVC_STATE_NS_REGISTER_FAIL;
        return err;
    }

    dma_svc_state = DMA_SVC_STATE_RUNNING;
    event_handlers = cb;

    DMASVC_DEBUG("DMA service up and running.\n");

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
errval_t dma_service_send_done(struct dma_binding *binding,
                               errval_t err,
                               dma_req_id_t id)
{
    struct dma_svc_reply_st *msgst = reply_st_alloc();
    if (msgst == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    msgst->b = binding;
    msgst->args.request.id = id;
    msgst->err = err;

    dma_done_tx(msgst);

    return SYS_ERR_OK;
}
