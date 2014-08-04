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
#include <xomp/xomp_worker.h>

#include <if/xomp_defs.h>

#include "xomp_debug.h"

static struct xomp_binding *xbinding;

static volatile uint8_t is_bound = 0x0;

static struct capref msgframe;

static void *msgbuf;

static xomp_wid_t worker_id;

struct tx_queue txq;

struct xomp_msg_st
{
    struct txq_msg_st common;
    /* union of arguments */
    union
    {
        struct
        {
            uint64_t arg;
            uint64_t id;
        } done_notify;
    } args;
};

#ifdef __k1om__
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

    XWI_DEBUG("msg_open_cb: frame [%016lx] mapped @ [%016lx]\n", id.base, addr);

    if ((xomp_frame_type_t) type == XOMP_FRAME_TYPE_MSG) {
        USER_PANIC("NYI: initializing messaging");
    }

    return SYS_ERR_OK;
}

static struct xeon_phi_callbacks callbacks = {
    .open = msg_open_cb
};

#endif

/*
 * ----------------------------------------------------------------------------
 * XOMP channel send handlers
 * ----------------------------------------------------------------------------
 */

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
                                  st->args.done_notify.id, st->args.done_notify.arg,
                                  msg_st->err);
}

/*
 * ----------------------------------------------------------------------------
 * XOMP channel receive handlers
 * ----------------------------------------------------------------------------
 */
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

    if (arg) {
        msg_st->send = done_with_arg_tx;
    } else {
        msg_st->send = done_notify_tx;
    }

    uint32_t work_id = id & 0xF;

    uint32_t *data = (uint32_t *)arg;
    uint32_t work_size = *data;
    data += 2;
    if (work_id) {
        data += (work_size / 2);
    }
    for (uint32_t i = 0; i < (work_size / 2); ++i) {
        data[i] = data[i] + 1;
    }

    txq_send(msg_st);
}

static struct xomp_rx_vtbl rx_vtbl = {
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
    for (uint32_t i = 1; argv[i] != NULL; ++i) {
        if (strcmp("-xompworker", argv[i]) == 0) {
            parsed++;
        } else if (strncmp("-wid=", argv[i], 5) == 0) {
            retwid = strtol(argv[i] + 5, NULL, 16);
            parsed++;
        }
    }

    if (parsed != 2) {
        return XOMP_ERR_INVALID_WORKER_ARGS;
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

    XWI_DEBUG("initializing worker {%016lx}\n", worker_id);

    struct capref frame = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_ARGCN
    };

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err_push(err, XOMP_ERR_INVALID_MSG_FRAME);
        return err;
    }

    if ((1UL << id.bits) < XOMP_MSG_FRAME_SIZE) {
        return XOMP_ERR_INVALID_MSG_FRAME;
    }

    msgframe = frame;

    err = vspace_map_one_frame(&msgbuf, XOMP_MSG_FRAME_SIZE, frame, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    XWI_DEBUG("messaging frame mapped: [%016lx] @ [%016lx]\n", id.base,
              (lvaddr_t )msgbuf);

#ifdef __k1om__
    err = xeon_phi_client_init(disp_xeon_phi_id());
    if (err_is_fail(err)) {
        return err;
    }

    xeon_phi_client_set_callbacks(&callbacks);
#else
    USER_PANIC("workers should be run on the xeon phi\n");
#endif

    struct xomp_frameinfo fi = {
        .sendbase = id.base,
        .inbuf = ((uint8_t *) msgbuf) + XOMP_MSG_CHAN_SIZE,
        .inbufsize = XOMP_MSG_CHAN_SIZE,
        .outbuf = ((uint8_t *) msgbuf),
        .outbufsize = XOMP_MSG_CHAN_SIZE
    };

    struct waitset *ws = get_default_waitset();
    err = xomp_connect(&fi, master_bind_cb, NULL, ws, IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        /* TODO: Clean up */
        return err;
    }

    XWI_DEBUG("Waiting until bound to master...\n");

    while (!is_bound) {
        messages_wait_and_handle_next();
    }

    if (xbinding == NULL) {
        /* TODO: error code */
        return -1;
    }

    return SYS_ERR_OK;

}
