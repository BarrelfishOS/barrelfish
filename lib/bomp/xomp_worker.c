/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <barrelfish/barrelfish.h>
#include <flounder/flounder_txqueue.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>

#include <xomp/xomp.h>

#include <if/xomp_defs.h>

#include "libbomp.h"
#include "backend.h"
#include "xomp_debug.h"

/// XOMP control channel to the master
static struct xomp_binding *xbinding;

/// flag indicating if the client is bound to the master
static volatile uint8_t is_bound = 0x0;

/// messaging frame capability
static struct capref msgframe;

/// where the messaging frame is mapped
static void *msgbuf;

/// pointer to the thread local storage
static void *tls;

/// service iref of the master (if local worker)
static iref_t svc_iref;

/// our own worker id
static xomp_wid_t worker_id;

/// Flounder TX messaging queue
struct tx_queue txq;

/**
 * \brief Flounder TX queue message state representation
 */
struct xomp_msg_st {
    struct txq_msg_st common;

    /* union of arguments */
    union {
        struct {
            uint64_t arg;
            uint64_t id;
        } done_notify;
    } args;
};

/*
 * ----------------------------------------------------------------------------
 * Xeon Phi Channel callbacks
 * ----------------------------------------------------------------------------
 */
static errval_t msg_open_cb(xphi_dom_id_t domain,
                            uint64_t usrdata,
                            struct capref frame,
                            uint8_t type)
{
    errval_t err;

    XWI_DEBUG("msg_open_cb: from domid:%lx, usrdata:%lx\n", domain, usrdata);

    uint32_t map_flags = 0x0;
    lvaddr_t addr = 0x0;

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    switch ((xomp_frame_type_t) type) {
        case XOMP_FRAME_TYPE_MSG:
            map_flags = VREGION_FLAGS_READ_WRITE;
            break;
        case XOMP_FRAME_TYPE_SHARED_RW:
            addr = (lvaddr_t) usrdata;
            map_flags = VREGION_FLAGS_READ_WRITE;
            break;
        case XOMP_FRAME_TYPE_SHARED_RO:
            map_flags = VREGION_FLAGS_READ;
            break;
        default:
            USER_PANIC("unknown type: %u", type)
            break;
    }
    if (addr) {
        err = vspace_map_one_frame_fixed_attr(addr, (1UL << id.bits), frame,
                                              map_flags, NULL, NULL);
    } else {
        err = vspace_map_one_frame_attr((void **) &addr, (1UL << id.bits), frame,
                                        map_flags, NULL, NULL);
    }
    if (err_is_fail(err)) {
        return err;
    }

    XWI_DEBUG("msg_open_cb: frame [%016lx] mapped @ [%016lx, %016lx]\n", id.base,
              addr, addr + (1UL << id.bits));

    if ((xomp_frame_type_t) type == XOMP_FRAME_TYPE_MSG) {
        USER_PANIC("NYI: initializing messaging");
    }

    return SYS_ERR_OK;
}

#ifdef __k1om__
static struct xeon_phi_callbacks callbacks = {
    .open = msg_open_cb
};
#endif

/*
 * ----------------------------------------------------------------------------
 * XOMP channel send handlers
 * ----------------------------------------------------------------------------
 */

static errval_t add_memory_response_tx(struct txq_msg_st *msg_st)
{
    return xomp_add_memory_response__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                        msg_st->err);
}

static errval_t done_notify_tx(struct txq_msg_st *msg_st)
{
    struct xomp_msg_st *st = (struct xomp_msg_st *) msg_st;

    return xomp_done_notify__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                st->args.done_notify.id, msg_st->err);
}

static errval_t done_with_arg_tx(struct txq_msg_st *msg_st)
{
    struct xomp_msg_st *st = (struct xomp_msg_st *) msg_st;

    return xomp_done_with_arg__tx(msg_st->queue->binding, TXQCONT(msg_st),
                                  st->args.done_notify.id,
                                  st->args.done_notify.arg, msg_st->err);
}


/*
 * ----------------------------------------------------------------------------
 * XOMP channel receive handlers
 * ----------------------------------------------------------------------------
 */

static void add_memory_call_rx(struct xomp_binding *b,
                               struct capref frame,
                               uint64_t addr,
                               uint8_t type)
{
    XWI_DEBUG("add_memory_call_rx: addr:%lx, tyep: %u\n", addr, type);
    struct txq_msg_st *msg_st = txq_msg_st_alloc(&txq);
    assert(msg_st != NULL);

    msg_st->err = msg_open_cb(0x0, addr, frame, type);
    msg_st->send = add_memory_response_tx;
    msg_st->cleanup = NULL;
    txq_send(msg_st);
}

static void do_work_rx(struct xomp_binding *b,
                       uint64_t fn,
                       uint64_t arg,
                       uint64_t id,
                       uint64_t flags)
{
    XWP_DEBUG("do_work_rx: fn:%lx, id:%lx\n", fn, id);

    struct txq_msg_st *msg_st = txq_msg_st_alloc(&txq);
    assert(msg_st != NULL);

    msg_st->err = SYS_ERR_OK;

    struct bomp_work *work = tls;
    work->data = work + 1;

    XWP_DEBUG("do_work_rx: threadid = %u, nthreads = %u\n", work->thread_id,
              work->num_threads);

    g_thread_numbers = work->num_threads;

    struct xomp_msg_st *st = (struct xomp_msg_st *) msg_st;
    st->args.done_notify.id = id;

    if (arg) {
        msg_st->send = done_with_arg_tx;
        st->args.done_notify.arg = arg;
    } else {
        msg_st->send = done_notify_tx;
    }

    xomp_worker_fn_t fnct = (xomp_worker_fn_t) fn;
    XWP_DEBUG("do_work_rx: calling fnct %p with argument %p\n", fnct, work->data);
    fnct(work->data);


    txq_send(msg_st);
}

static struct xomp_rx_vtbl rx_vtbl = {
    .add_memory_call = add_memory_call_rx,
    .do_work = do_work_rx
};

/*
 * ----------------------------------------------------------------------------
 * XOMP channel connect handler
 * ----------------------------------------------------------------------------
 */

/**
 * \brief XOMP channel connect callback called by the Flounder backend
 *
 * \param st    Supplied worker state
 * \param err   outcome of the connect attempt
 * \param xb    XOMP Flounder binding
 */
static void master_bind_cb(void *st,
                           errval_t err,
                           struct xomp_binding *xb)
{
    is_bound = 0x1;

    XWI_DEBUG("bound to master: %s\n", err_getstring(err));

    txq_init(&txq, xb, xb->waitset, (txq_register_fn_t) xb->register_send,
             sizeof(struct xomp_msg_st));

    xb->rx_vtbl = rx_vtbl;
    xbinding = xb;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 * \brief parses the command line arguments to extract
 *
 * \param argc  argument count
 * \param argv  argument values
 * \param wid   returns the XOMP worker ID
 *
 * \returns SYS_ERR_OK iff the command line arguments were parsed succecssfully
 *          XOMP_ERR_INVALID_WORKER_ARGS if there were no XOMP worker argumetnts
 *          errval on error
 *
 */
errval_t xomp_worker_parse_cmdline(uint8_t argc,
                                   char *argv[],
                                   xomp_wid_t *wid)
{
    XWI_DEBUG("xomp_worker_parse_cmdline\n");

    xomp_wid_t retwid = 0;
    uint8_t parsed = 0;
    uint8_t is_worker = 0x0;
    iref_t iref = 0x0;
    for (uint32_t i = 1; argv[i] != NULL; ++i) {
        if (strcmp(XOMP_WORKER_ARG, argv[i]) == 0) {
            parsed++;
            is_worker = 0x1;
        } else if (strncmp("--wid=", argv[i], 6) == 0) {
            retwid = strtol(argv[i] + 6, NULL, 16);
            parsed++;
        } else if (strncmp("--iref=", argv[i], 7) == 0) {
            iref = strtol(argv[i] + 7, NULL, 16);
            parsed++;
        }
    }

    if (!is_worker) {
        return XOMP_ERR_BAD_INVOCATION;
    }

    if (parsed < 2) {
        return XOMP_ERR_INVALID_WORKER_ARGS;
    }

    if (iref) {
        if (parsed != 3) {
            return XOMP_ERR_INVALID_WORKER_ARGS;
        }
        svc_iref = iref;
    }

    if (wid) {
        *wid = retwid;
    }

    return SYS_ERR_OK;
}

/**
 * \brief initializes the XOMP worker library
 *
 * \param wid   Xomp worker id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xomp_worker_init(xomp_wid_t wid)
{
    errval_t err;

    worker_id = wid;

    XWI_DEBUG("initializing worker {%016lx} iref:%u\n", worker_id, svc_iref);

    struct capref frame = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_ARGCN
    };

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err_push(err, XOMP_ERR_INVALID_MSG_FRAME);
    }

    size_t frame_size = 0;

    if (svc_iref) {
        frame_size = XOMP_TLS_SIZE;
    } else {
        frame_size = XOMP_FRAME_SIZE;
    }

    if ((1UL << id.bits) < XOMP_TLS_SIZE) {
        return XOMP_ERR_INVALID_MSG_FRAME;
    }

    msgframe = frame;

    err = vspace_map_one_frame(&msgbuf, frame_size, frame, NULL, NULL);
    if (err_is_fail(err)) {
        err_push(err, XOMP_ERR_WORKER_INIT_FAILED);
    }
    if (svc_iref) {
        tls = msgbuf;
    }
    else {
        tls = ((uint8_t *) msgbuf) + XOMP_MSG_FRAME_SIZE;
    }
    xomp_set_tls(tls);

    XWI_DEBUG("messaging frame mapped: [%016lx] @ [%016lx]\n", id.base,
              (lvaddr_t )msgbuf);

#ifdef __k1om__
    err = xeon_phi_client_init(disp_xeon_phi_id());
    if (err_is_fail(err)) {
        err_push(err, XOMP_ERR_WORKER_INIT_FAILED);
    }

    xeon_phi_client_set_callbacks(&callbacks);
#endif

    struct waitset *ws = get_default_waitset();

    if (svc_iref) {
        err = xomp_bind(svc_iref, master_bind_cb, NULL, ws,
        IDC_EXPORT_FLAGS_DEFAULT);
    } else {
        struct xomp_frameinfo fi = {
            .sendbase = id.base,
            .inbuf = ((uint8_t *) msgbuf) + XOMP_MSG_CHAN_SIZE,
            .inbufsize = XOMP_MSG_CHAN_SIZE,
            .outbuf = ((uint8_t *) msgbuf),
            .outbufsize = XOMP_MSG_CHAN_SIZE
        };
        err = xomp_connect(&fi, master_bind_cb, NULL, ws,
        IDC_EXPORT_FLAGS_DEFAULT);
    }

    if (err_is_fail(err)) {
        /* TODO: Clean up */
        return err_push(err, XOMP_ERR_WORKER_INIT_FAILED);
    }

    XWI_DEBUG("Waiting until bound to master...\n");

    while (!is_bound) {
        messages_wait_and_handle_next();
    }

    if (xbinding == NULL) {
        return XOMP_ERR_WORKER_INIT_FAILED;
    }

    return SYS_ERR_OK;

}
