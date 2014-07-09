/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/xeon_phi_dma_defs.h>

#include "xeon_phi_internal.h"
#include "dma.h"
#include "dma_mem.h"
#include "dma_channel.h"
#include "debug.h"

/**
 * enumration of all possible states of the service exportation process
 */
enum xpm_svc_state
{
    XPM_SVC_STATE_EXPORTING,
    XPM_SVC_STATE_EXPORT_OK,
    XPM_SVC_STATE_EXPORT_FAIL,
    XPM_SVC_STATE_NS_REGISTERING,
    XPM_SVC_STATE_NS_REGISTER_OK,
    XPM_SVC_STATE_NS_REGISTER_FAIL,
    XPM_SVC_STATE_RUNNING
};

/// represents the current state of the exporting process
static enum xpm_svc_state svc_state = XPM_SVC_STATE_EXPORTING;

/// our own iref of the exported service
static iref_t dma_iref;

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
 * Memory:  registration
 * ----------------------------------------------------------------------------
 */

struct dma_reg_resp_st
{
    struct xeon_phi_dma_binding *b;
    errval_t err;
};

static void dma_register_response_tx(void *a)
{
    errval_t err;

    struct dma_reg_resp_st *st = a;

    struct event_closure txcont = MKCONT(free, a);

    err = xeon_phi_dma_register_response__tx(st->b, txcont, st->err);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(dma_register_response_tx, a);
            err = st->b->register_send(st->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not send reply");
            }
        }
    }
}

static void dma_register_call_rx(struct xeon_phi_dma_binding *_binding,
                                 struct capref memory)
{
    errval_t err;

    err = xdma_mem_register(_binding, memory);

    struct dma_reg_resp_st *st = malloc(sizeof(struct dma_reg_resp_st));
    assert(st);
    st->b = _binding;
    st->err = err;

    dma_register_response_tx(st);
}

/*
 * ----------------------------------------------------------------------------
 * Memory: de-registration
 * ----------------------------------------------------------------------------
 */

struct dma_dereg_resp_st
{
    struct xeon_phi_dma_binding *b;
    errval_t err;
};

static void dma_deregister_response_tx(void *a)
{
    errval_t err;

    struct dma_dereg_resp_st *st = a;

    struct event_closure txcont = MKCONT(free, a);

    err = xeon_phi_dma_deregister_response__tx(st->b, txcont, st->err);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(dma_deregister_response_tx, a);
            err = st->b->register_send(st->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not send reply");
            }
        }
    }
}

static void dma_deregister_call_rx(struct xeon_phi_dma_binding *_binding,
                                   struct capref memory)
{
    errval_t err;

    err = xdma_mem_deregister(_binding, memory);

    struct dma_dereg_resp_st *st = malloc(sizeof(struct dma_dereg_resp_st));
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
struct dma_exec_resp_st
{
    struct xeon_phi_dma_binding *b;
    xeon_phi_dma_id_t id;
    errval_t err;
    uint8_t sent;
};

struct dma_exec_resp_st exec_resp_err;


static void dma_exec_response_sent(void *a)
{
    struct dma_exec_resp_st *st = a;
    st->sent = 0x1;
}

static void dma_exec_response_tx(void *a)
{
    errval_t err;

    struct dma_exec_resp_st *st = a;

    struct event_closure txcont = MKCONT(dma_exec_response_sent, a);

    err = xeon_phi_dma_exec_response__tx(st->b, txcont, st->err, st->id);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(dma_exec_response_tx, a);
            XDMA_DEBUG("dma_exec_response_tx: register sending...\n");
            err = st->b->register_send(st->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not send reply");
            }
        }
        return;
    }
}

static void dma_exec_call_rx(struct xeon_phi_dma_binding *_binding,
                             uint64_t src,
                             uint64_t dst,
                             uint64_t length)
{
    XDMAV_DEBUG("memcopy request [0x%016lx]->[0x%016lx] of size 0x%lx\n",
                           src, dst, length);

    struct dma_exec_resp_st st;

    st.b = _binding;
    st.sent = 0x0;
    lpaddr_t dma_src = xdma_mem_verify(_binding, src, length);
    lpaddr_t dma_dst = xdma_mem_verify(_binding, dst, length);
    if (!dma_src || !dma_dst) {
        st.err = XEON_PHI_ERR_DMA_MEM_REGISTERED;
        st.id = 0;
#ifdef XDEBUG_DMA
        if (!dma_src) {
            XDMA_DEBUG("Memory range not registered: [0x%016lx] [0x%016lx]\n",
                       src, src+length);
        }
        if (!dma_dst) {
            XDMA_DEBUG("Memory range not registered: [0x%016lx] [0x%016lx]\n",
                       dst, dst+length);
        }
#endif

        dma_exec_response_tx(&st);
        return;
    }

    /*
     * DMA transfers from host to host are not supported.
     */
    if (dma_src > XEON_PHI_SYSMEM_BASE && dma_dst > XEON_PHI_SYSMEM_BASE) {
        st.err = XEON_PHI_ERR_DMA_NOT_SUPPORTED;
        st.id = 0;
        dma_exec_response_tx(&st);
        return;
    }

    struct dma_req_setup setup = {
        .type = XDMA_REQ_TYPE_MEMCPY,
        .st = _binding,
        .cb = dma_service_send_done,
    };
    setup.info.mem.src = dma_src;
    setup.info.mem.dst = dma_dst;
    setup.info.mem.bytes = length;
    setup.info.mem.dma_id = &st.id;

    struct xeon_phi *phi = xdma_mem_get_phi(_binding);

    st.err = dma_do_request(phi, &setup);

    dma_exec_response_tx(&st);

    /*
     * XXX: we must wait until the message has been sent, otherwise we may
     *      trigger sending the done message when we poll in the main message
     *      loop. This causes the client library to receive a done message
     *      of an invalid id.
     */
    volatile uint8_t *sent_flag = &st.sent;
    while(!(*sent_flag)) {
        messages_wait_and_handle_next();
    }
}

/*
 * ----------------------------------------------------------------------------
 * Transfer Control: STOP
 * ----------------------------------------------------------------------------
 */

struct dma_stop_resp_st
{
    struct xeon_phi_dma_binding *b;
    errval_t err;
};

static void dma_stop_response_tx(void *a)
{
    errval_t err;

    struct dma_exec_resp_st *st = a;

    struct event_closure txcont = MKCONT(free, a);

    err = xeon_phi_dma_stop_response__tx(st->b, txcont, st->err);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(dma_stop_response_tx, a);
            err = st->b->register_send(st->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not send reply");
            }

        }
    }
}

static void dma_stop_call_rx(struct xeon_phi_dma_binding *_binding,
                             xeon_phi_dma_id_t id)
{

    debug_printf(" DMA | dma_stop_call_rx: NOT YET IMPLEMENTED!!!!\n");

    struct dma_stop_resp_st *st = malloc(sizeof(struct dma_stop_resp_st));
    assert(st);
    st->b = _binding;
    st->err = SYS_ERR_OK;

    dma_stop_response_tx(st);
}

struct xeon_phi_dma_rx_vtbl dma_rx_vtbl = {
    .register_call = dma_register_call_rx,
    .deregister_call = dma_deregister_call_rx,
    .exec_call = dma_exec_call_rx,
    .stop_call = dma_stop_call_rx
};

/*
 * ----------------------------------------------------------------------------
 * Flounder callbacks for export and connect events
 * ----------------------------------------------------------------------------
 */

/**
 * \brief Xeon Phi DMA service connect handler
 *
 * \param st pointer to struct xeon_phi
 * \param b  Flounder binding
 */
static errval_t svc_connect_cb(void *st,
                               struct xeon_phi_dma_binding *b)
{
    errval_t err;

    XDMA_DEBUG("New connection to the DMA service.\n");
    b->rx_vtbl = dma_rx_vtbl;

    struct xeon_phi *phi = st;
    err = xdma_mem_init(b, phi);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * \brief
 */
static void svc_export_cb(void *st,
                          errval_t err,
                          iref_t iref)
{
    if (err_is_fail(err)) {
        svc_state = XPM_SVC_STATE_EXPORT_FAIL;
        return;
    }

    dma_iref = iref;

    svc_state = XPM_SVC_STATE_EXPORT_OK;
}

errval_t dma_service_init(struct xeon_phi *phi)
{
    errval_t err;

    XDMA_DEBUG("Initializing DMA service\n");

    struct waitset *ws = get_default_waitset();

    err = xeon_phi_dma_export(phi,
                              svc_export_cb,
                              svc_connect_cb,
                              ws,
                              IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    XDMAV_DEBUG("Waiting for export...\n");
    while (svc_state == XPM_SVC_STATE_EXPORTING) {
        messages_wait_and_handle_next();
    }

    if (svc_state == XPM_SVC_STATE_EXPORT_FAIL) {
        return FLOUNDER_ERR_BIND;
    }

    svc_state = XPM_SVC_STATE_NS_REGISTERING;

    char buf[50];
#ifdef __k1om__
    snprintf(buf, 50, "%s.%u", XEON_PHI_DMA_SERVICE_NAME, 0);
#else
    snprintf(buf, 50, "%s.%u", XEON_PHI_DMA_SERVICE_NAME, phi->id);
#endif

    XDMA_DEBUG("Registering iref [%u] with name [%s]\n", dma_iref, buf);
    err = nameservice_register(buf, dma_iref);
    if (err_is_fail(err)) {
        svc_state = XPM_SVC_STATE_NS_REGISTER_FAIL;
        return err;
    }

    svc_state = XPM_SVC_STATE_RUNNING;

    return SYS_ERR_OK;

}

/*
 * ---------------------------------------------------------------------------
 * Send the done notification
 * ---------------------------------------------------------------------------
 */
struct dma_done_st
{
    struct xeon_phi_dma_binding *b;
    xeon_phi_dma_id_t id;
    errval_t err;
};

static void dma_done_tx(void *a)
{
    errval_t err;

    struct dma_done_st *st = a;

    struct event_closure txcont = MKCONT(free, a);

    err = xeon_phi_dma_done__tx(st->b, txcont, st->id, st->err);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(dma_done_tx, a);
            err = st->b->register_send(st->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not send reply");
            }
        }
        return;
    }
}

errval_t dma_service_send_done(void *a,
                               errval_t err,
                               xeon_phi_dma_id_t id)
{
    struct xeon_phi_dma_binding *b = a;
    assert(b);

    struct dma_done_st *msgst = malloc(sizeof(struct dma_done_st));
    if (msgst == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    msgst->b = b;
    msgst->id = id;
    msgst->err = err;

    dma_done_tx(msgst);

    return SYS_ERR_OK;
}
