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

#include "libbomp.h"
#include "backend.h"
#include "xomp_debug.h"

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
    void *tls;
};

/**
 *
 */
struct xomp_master
{
    uint32_t numworker;
    uint32_t next_worker;
    struct xomp_worker *workers;
};

static struct xomp_master xmaster;

static uint8_t num_phi = 0;

static uint8_t argnum;

static char **argval;

char worker_id_buf[23];

static char *worker_path;

static inline void xbomp_barrier_enter_no_wait(struct bomp_barrier *barrier)
{
    if (__sync_fetch_and_add(&barrier->counter, 1) == (barrier->max - 1)) {
        barrier->counter = 0;
        barrier->cycle = !barrier->cycle;
    }
}

static inline void xbomp_barrier_enter(struct bomp_barrier *barrier)
{
    errval_t err;
    int cycle = barrier->cycle;
    if (__sync_fetch_and_add(&barrier->counter, 1) == (barrier->max - 1)) {
        barrier->counter = 0;
        barrier->cycle = !barrier->cycle;
    } else {
        while (cycle == barrier->cycle) {
            err = event_dispatch_non_block(get_default_waitset());
            switch(err_no(err)) {
                case SYS_ERR_OK:
                    break;
                case LIB_ERR_NO_EVENT :
                    thread_yield();
                    break;
                default:
                    USER_PANIC_ERR(err, "in event dispatch");
            }
        }
    }
}

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
                             uint64_t tid,
                             uint64_t arg,
                             errval_t msgerr)
{
    XMP_DEBUG("done_with_arg_rx: arg:%lx, id:%lx\n", arg, tid);

    struct xomp_task *task = (struct xomp_task *) tid;

    xbomp_barrier_enter_no_wait(task->barrier);
    if (task->done == task->nworkers) {
        free(task);
    }
}

static void done_notify_rx(struct xomp_binding *b,
                           uint64_t tid,
                           errval_t msgerr)
{
    XMP_DEBUG("done_notify_rx: id:%lx\n", tid);

    struct xomp_task *task = (struct xomp_task *) tid;

    xbomp_barrier_enter_no_wait(task->barrier);

    task->done++;
    if (task->done == task->nworkers) {
        free(task);
    }
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

        err = frame_alloc(&worker->msgframe, XOMP_FRAME_SIZE, NULL);
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

        err = vspace_map_one_frame(&worker->msgbuf, XOMP_FRAME_SIZE,
                                   worker->msgframe, NULL, NULL);
        if (err_is_fail(err)) {
            /* TODO: cleanup */
            return err_push(err, XOMP_ERR_SPAWN_WORKER_FAILED);
        }

        worker->tls = ((uint8_t *) worker->msgbuf) + XOMP_MSG_FRAME_SIZE;

        XMI_DEBUG("messaging frame mapped: [%016lx] @ [%016lx]\n", worker->msgbase,
                  (lvaddr_t )worker->msgbuf);

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

        XMI_DEBUG("waiting for client to connect...\n");

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
 * \param task information about the task
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xomp_master_do_work(struct xomp_task *task)
{

    for (uint32_t i = 0; i < task->nworkers; ++i) {
        struct xomp_worker *worker = &xmaster.workers[xmaster.next_worker++];
        if (xmaster.next_worker == xmaster.numworker) {
            xmaster.next_worker = 0;
        }

        struct bomp_work *work = worker->tls;
        work->fn = task->fn;
        work->data = work + 1;
        work->barrier = NULL;
        work->thread_id = i+1;
        work->num_threads = task->total_threads;

        memset(work->data, 0, 64);
        XMI_DEBUG(" copying 64bytes from %p to %p\n", work->data, task->arg);
        memcpy(work->data, task->arg, 64);

        struct txq_msg_st *msg_st = txq_msg_st_alloc(&worker->txq);
        if (msg_st == NULL) {
            if (i == 0) {
                free(task);
            }
            return LIB_ERR_MALLOC_FAIL;
        }

        msg_st->send = do_work_tx;
        msg_st->cleanup = NULL;

        struct xomp_msg_st *st = (struct xomp_msg_st *) msg_st;
        st->args.do_work.arg = (uint64_t) work->data;  //(uint64_t) task->arg;
        st->args.do_work.fn = 0x400580;  //(uint64_t) task->fn;
        st->args.do_work.id = (uint64_t) task;
        st->args.do_work.flags = 0;

        txq_send(msg_st);
    }

    return SYS_ERR_OK;
}

static int count = 0;
volatile unsigned g_thread_numbers = 1;
static struct bomp_thread_local_data **g_array_thread_local_data;

void xomp_set_tls(void *xdata)
{
    struct bomp_thread_local_data *local;
    struct bomp_work *work_data = (struct bomp_work*) xdata;

    /* Populate the Thread Local Storage data */
    local = calloc(1, sizeof(struct bomp_thread_local_data));
    assert(local != NULL);
    local->thr = backend_get_thread();
    local->work = work_data;
    if (g_array_thread_local_data) {
        g_array_thread_local_data[work_data->thread_id] = local;
    }
    backend_set_tls(local);
}

#define THREAD_OFFSET   0
/* #define THREAD_OFFSET   12 */

static int bomp_thread_fn(void *xdata)
{
    struct bomp_work *work_data = xdata;

    backend_set_numa(work_data->thread_id);

    xomp_set_tls(work_data);
    work_data->fn(work_data->data);

    /* Wait for the Barrier */
    bomp_barrier_wait(work_data->barrier);
    return 0;
}

void bomp_start_processing(void (*fn) (void *),
                           void *data,
                           unsigned nthreads)
{
    XMP_DEBUG("start processing fn:%p, data:%p, threads:%u\n", fn, data, nthreads);

    errval_t err;

    /* Create Threads and ask them to process the function specified */
    /* Let them die as soon as they are done */

    struct bomp_work *xdata;
    struct bomp_barrier *barrier;

    uint32_t phi_threads = (nthreads) / (1 + num_phi);
    uint32_t host_threads = nthreads - (num_phi * phi_threads);

    XMP_DEBUG("thread configuration: host:%u, phi: %ux%u\n", host_threads, num_phi,
              phi_threads);

    g_thread_numbers = nthreads;

    char *memory = calloc(1, nthreads * sizeof(void *) + sizeof(*barrier)
                            + nthreads * sizeof(*xdata));
    assert(memory != NULL);

    g_array_thread_local_data = (struct bomp_thread_local_data **) memory;
    memory += nthreads * sizeof(struct bomp_thread_local_data *);

    /* Create a barier for the work that will be carried out by the threads */
    barrier = (struct bomp_barrier *) memory;
    memory += sizeof(struct bomp_barrier);
    bomp_barrier_init(barrier, nthreads);

    /* For main thread */
    xdata = (struct bomp_work *) memory;
    memory += sizeof(struct bomp_work);

    xdata->fn = fn;
    xdata->data = data;
    xdata->thread_id = 0;
    xdata->barrier = barrier;
    xomp_set_tls(xdata);

    struct xomp_task *task = calloc(1, sizeof(struct xomp_task));
    assert(task);
    task->arg = data;
    task->barrier = barrier;
    task->nworkers = (num_phi * phi_threads);
    task->total_threads = nthreads;
    task->fn = fn;
    task->done = 1;  // set the main thread to done

    XMP_DEBUG("distributing work to Xeon Phi\n");

    err = xomp_master_do_work(task);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to distribute workers\n");
    }

    XMP_DEBUG("distributing work to host threads\n");
    for (uint32_t i = 1; i < host_threads; i++) {
        xdata = (struct bomp_work *)memory;
        memory += sizeof(struct bomp_work);

        xdata->fn = fn;
        xdata->data = data;
        xdata->thread_id = (task->nworkers) + i;
        xdata->barrier = barrier;

        /* Create threads */
        backend_run_func_on(i, bomp_thread_fn, xdata);
    }

}

void bomp_end_processing(void)
{
    /* Cleaning of thread_local and work data structures */
    int i = 0;
    count++;

    xbomp_barrier_enter(g_array_thread_local_data[i]->work->barrier);

    /* Clear the barrier created */
    bomp_clear_barrier(g_array_thread_local_data[i]->work->barrier);

    free(g_array_thread_local_data);

    g_array_thread_local_data = NULL;
    g_thread_numbers = 1;
}
