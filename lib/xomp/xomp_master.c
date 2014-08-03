/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <flounder/flounder_txqueue.h>
#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>
#include <xeon_phi/xeon_phi_domain.h>
#include <xomp/xomp.h>
#include <xomp/xomp_master.h>

#include <if/xomp_defs.h>

#include "xomp_debug.h"

typedef uint32_t xomp_task_id_t;

struct xomp_task
{
    xomp_task_id_t id;
    uint32_t nworkers;
    uint32_t done;
};

static volatile uint32_t done;

typedef enum xomp_worker_state
{
    XOMP_WORKER_ST_INVALID = 0,
    XOMP_WORKER_ST_FAILURE = 1,
    XOMP_WORKER_ST_SPAWNING = 2,
    XOMP_WORKER_ST_SPAWNED = 3,
    XOMP_WORKER_ST_READY = 4,
    XOMP_WORKER_ST_BUSY = 5
} xomp_worker_st_t;

struct xomp_worker
{
    xomp_wid_t id;
    struct tx_queue txq;
    struct xomp_binding *binding;
    struct capref msgframe;
    lpaddr_t msgbase;
    void *msgbuf;
    xomp_worker_st_t state;
    xphi_dom_id_t domainid;
};

/**
 *
 */
struct xomp_master
{
    uint32_t numworker;
    struct xomp_worker *workers;
};

static struct xomp_master xmaster;

static uint8_t num_phi = 0;

static uint8_t argnum;

static char **argval;

char worker_id_buf[23];

static char *worker_path;

struct xomp_msg_st
{
    struct txq_msg_st common;
    /* union of arguments */
    union
    {
        struct
        {
            uint64_t fn;
            uint64_t arg;
            uint64_t id;
            uint64_t flags;
        } do_work;
    } args;
};

/*
 * ----------------------------------------------------------------------------
 * XOMP channel send handlers
 * ----------------------------------------------------------------------------
 */
static errval_t do_work_tx(struct txq_msg_st *msg_st)
{
    struct xomp_msg_st *st = (struct xomp_msg_st *) msg_st;

    return xomp_do_work__tx(msg_st->queue->binding, TXQCONT(msg_st),
                            st->args.do_work.fn, st->args.do_work.arg,
                            st->args.do_work.id, st->args.do_work.flags);
}

/*
 * ----------------------------------------------------------------------------
 * XOMP channel receive handlers
 * ----------------------------------------------------------------------------
 */

static void done_with_arg_rx(struct xomp_binding *b,
                             uint64_t id,
                             uint64_t arg,
                             errval_t msgerr)
{
    XMP_DEBUG("done_with_arg_rx: arg:%lx, id:%lx\n", arg, id);
    done++;
}

static void done_notify_rx(struct xomp_binding *b,
                           uint64_t id,
                           errval_t msgerr)
{
    XMP_DEBUG("done_notify_rx: id:%lx\n", id);
    done++;
}

static struct xomp_rx_vtbl rx_vtbl = {
    .done_notify = done_notify_rx,
    .done_with_arg = done_with_arg_rx
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
static void worker_connect_cb(void *st,
                              errval_t err,
                              struct xomp_binding *xb)
{
    struct xomp_worker *worker = st;

    XMI_DEBUG("worker:%lx connected: %s\n", worker->id, err_getstring(err));

    if (err_is_fail(err)) {
        worker->state = XOMP_WORKER_ST_FAILURE;
    }

    xb->rx_vtbl = rx_vtbl;
    xb->st = worker;

    txq_init(&worker->txq, xb, xb->waitset, (txq_register_fn_t) xb->register_send,
             sizeof(struct xomp_msg_st));

    worker->binding = xb;
    worker->state = XOMP_WORKER_ST_SPAWNED;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 * \brief initializes the Xeon Phi openMP library
 *
 * \param nphi  Number of Xeon Phis to use
 * \param path  Path to the worker binary
 * \param argc  Argument count this program was started
 * \param argv  Argumnent values
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xomp_master_init(uint8_t nphi,
                          char *path,
                          uint8_t argc,
                          char *argv[])
{
    XMI_DEBUG("Initializing XOMP master with nphi:%u\n", nphi);
    num_phi = nphi;
    argnum = argc;
    argval = argv;
    argval[argnum++] = "-xompworker";
    worker_path = path;
    argval[argnum++] = worker_id_buf;
    argval[argnum] = NULL;
    return SYS_ERR_OK;
}

/**
 * \brief Spawns the worker threads on the Xeon Phi
 *
 * \param nworkers  Number of worker thread to be spawned per Xeon Phi
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xomp_master_spawn_workers(uint32_t nworkers)
{
    errval_t err;

    xmaster.numworker = nworkers;
    xmaster.workers = calloc(nworkers, sizeof(struct xomp_worker));
    if (xmaster.workers == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    uint32_t nphi = (nworkers / num_phi);
    if ((nphi * num_phi) < nworkers) {
        nphi++;
    }

    XMI_DEBUG("spawning %u workers (%u per Xeon Phi)\n", nworkers, nphi);

    xphi_id_t xid = 0;
    uint32_t on_phi = 0;
    for (uint32_t i = 0; i < nworkers; ++i) {
#ifdef __k1om__
        if (xid == disp_xeon_phi_id()) {
            xid++;
        }
#endif
        struct xomp_worker *worker = xmaster.workers + i;

        err = frame_alloc(&worker->msgframe, XOMP_MSG_FRAME_SIZE, NULL);
        if (err_is_fail(err)) {
            /* TODO: cleanup */
            return err_push(err, XOMP_ERR_SPAWN_WORKER_FAILED);
        }

        struct frame_identity id;
        err = invoke_frame_identify(worker->msgframe, &id);
        if (err_is_fail(err)) {
            /* TODO: cleanup */
            return err_push(err, XOMP_ERR_SPAWN_WORKER_FAILED);
        }

        worker->msgbase = id.base;
        worker->id = 0xffaa0000 + i; /* TODO: build a good id */

        err = vspace_map_one_frame(&worker->msgbuf, XOMP_MSG_FRAME_SIZE,
                                   worker->msgframe, NULL, NULL);
        if (err_is_fail(err)) {
            /* TODO: cleanup */
            return err_push(err, XOMP_ERR_SPAWN_WORKER_FAILED);
        }

        XMI_DEBUG("messaging frame mapped: [%016lx] @ [%016lx]\n",
                  worker->msgbase, (lvaddr_t )worker->msgbuf);

        struct xomp_frameinfo fi = {
            .sendbase = worker->msgbase + XOMP_MSG_CHAN_SIZE,
            .inbuf = worker->msgbuf,
            .inbufsize = XOMP_MSG_CHAN_SIZE,
            .outbuf = ((uint8_t *) worker->msgbuf) + XOMP_MSG_CHAN_SIZE,
            .outbufsize = XOMP_MSG_CHAN_SIZE
        };

        err = xomp_accept(&fi, worker, worker_connect_cb, get_default_waitset(),
                          IDC_EXPORT_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            /* TODO: Clean up */
            return err_push(err, XOMP_ERR_SPAWN_WORKER_FAILED);
        }

        worker->state = XOMP_WORKER_ST_SPAWNING;

        snprintf(worker_id_buf, sizeof(worker_id_buf), "-wid=%016"PRIx64,
                 worker->id);
        on_phi++;

        XMI_DEBUG("spawning {%s} on xid:%u, core:%u\n", worker_path, xid, on_phi);

        err = xeon_phi_client_spawn(xid, on_phi, worker_path, argval,
                                    worker->msgframe, &worker->domainid);
        if (err_is_fail(err)) {
            /* TODO: cleanup */
            return err_push(err, XOMP_ERR_SPAWN_WORKER_FAILED);
        }

        while (worker->state == XOMP_WORKER_ST_SPAWNING) {
            messages_wait_and_handle_next();
        }

        if (worker->state == XOMP_WORKER_ST_FAILURE) {
            return XOMP_ERR_SPAWN_WORKER_FAILED;
        }

        worker->state = XOMP_WORKER_ST_READY;

        if (on_phi == nphi) {
            xid++;
            on_phi = 0;
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Adds a memory region to be used for work
 *
 * \param frame Frame to be shared
 * \param info  information about the frame i.e. virtual address to map
 * \oaram type  Type of the frame
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xomp_master_add_memory(struct capref frame,
                                uint64_t info,
                                xomp_frame_type_t type)
{
    errval_t err;

    XMI_DEBUG("adding memory of type %u @ info: %016lx\n", type, info);

    for (uint32_t i = 0; i < xmaster.numworker; ++i) {
        struct xomp_worker *worker = &xmaster.workers[i];
        XMI_DEBUG(" worker: %lx\n", worker->id);

        if (worker->state < XOMP_WORKER_ST_READY) {
            XMI_DEBUG("skipping worker %lx, not ready\n", worker->id);
            continue;
        }

        xphi_id_t xid = xeon_phi_domain_get_xid(worker->domainid);
        err = xeon_phi_client_chan_open(xid, worker->domainid, info, frame, type);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to add the frame to the user. disabling it.\n");
            worker->state = XOMP_WORKER_ST_FAILURE;
            continue;
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief executes some work on each worker domains
 *
 * \param fn
 * \param arg
 * \param nthreads
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xomp_master_do_work(lvaddr_t fn,
                             lvaddr_t arg,
                             uint32_t nthreads)
{
    uint32_t current = 0;
    for (uint32_t i = 0; i < nthreads; ++i) {
        while(current < xmaster.numworker) {
            if (xmaster.workers[current].state == XOMP_WORKER_ST_READY) {
                break;
            }
            current++;
        }


        struct xomp_worker *worker = &xmaster.workers[current];
        XMI_DEBUG(" worker: %lx\n", worker->id);


        struct txq_msg_st *msg_st = txq_msg_st_alloc(&worker->txq);
        if (msg_st == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }

        msg_st->send = do_work_tx;
        msg_st->cleanup = NULL;

        struct xomp_msg_st *st = (struct xomp_msg_st *)msg_st;
        st->args.do_work.arg = arg;
        st->args.do_work.fn = fn;
        st->args.do_work.id = (0xdeadbeefUL << 32) | i;
        st->args.do_work.flags = 0;

        txq_send(msg_st);
        current++;
    }

    while (done < nthreads) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}
