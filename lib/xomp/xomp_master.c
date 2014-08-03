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
#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>
#include <xomp/xomp.h>
#include <xomp/xomp_master.h>

#include <if/xomp_defs.h>

#include "xomp_debug.h"


typedef enum xomp_worker_state
{
    XOMP_WORKER_ST_INVALID,
    XOMP_WORKER_ST_SPAWNING,
    XOMP_WORKER_ST_SPAWNED,
    XOMP_WORKER_ST_READY,
    XOMP_WORKER_ST_BUSY,
    XOMP_WORKER_ST_FAILURE
} xomp_worker_st_t;

struct xomp_worker
{
    xomp_wid_t id;
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

/*
 * ----------------------------------------------------------------------------
 * XOMP channel send handlers
 * ----------------------------------------------------------------------------
 */

/*
 * ----------------------------------------------------------------------------
 * XOMP channel receive handlers
 * ----------------------------------------------------------------------------
 */

static struct xomp_rx_vtbl rx_vtbl = {
    .do_work_response = NULL
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
        worker->id = i; /* TODO: build a good id */

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

        err = xomp_accept(&fi, worker, worker_connect_cb, get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            /* TODO: Clean up */
            return err_push(err, XOMP_ERR_SPAWN_WORKER_FAILED);
        }

        worker->state = XOMP_WORKER_ST_SPAWNING;

        snprintf(worker_id_buf, sizeof(worker_id_buf), "-wid=%016"PRIx64, worker->id);
        on_phi++;

        XMI_DEBUG("spawning {%s} on xid:%u, core:%u\n", worker_path, xid, on_phi);

        err = xeon_phi_client_spawn(xid, on_phi, worker_path, argval, worker->msgframe,
                                    &worker->domainid);
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
 * \param vbase virtual base address where the frame should be mapped
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xomp_master_add_memory(struct capref frame,
                                lvaddr_t vbase)
{
    assert(!"NYI: adding memory");
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
    assert(!"NYI: do work");
    return SYS_ERR_OK;
}
