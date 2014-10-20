/**
 * \file
 * \brief Implementation of the backend functions for domain exclusive address
 *        spaces
 *
 * This backend of libbomp deals with worker domains instead of threads.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <stdio.h>
#include <barrelfish/sys_debug.h>

#include <bomp_internal.h>
#include <xomp/xomp.h>
#include <xomp_debug.h>

static struct bomp_state *xomp_st;

/*
 * ----------------------------------------------------------------------------
 * XOMP barriers
 * ----------------------------------------------------------------------------
 */
/**
 * \brief enters the barrier for the master thread and waits until all
 *        workers also entered the barrier
 *
 * \param barrier   The barrier to enter
 */
static inline void xomp_barrier_enter(struct bomp_barrier *barrier)
{
    errval_t err;
    int cycle = barrier->cycle;

    if (__sync_fetch_and_add(&barrier->counter, 1) == (barrier->max - 1)) {
        barrier->counter = 0;
        barrier->cycle = !barrier->cycle;
        return;
    }

    while (cycle == barrier->cycle) {
        /* dispatch the events such that we recv the done messages */
        err = event_dispatch_non_block(get_default_waitset());
        switch (err_no(err)) {
            case SYS_ERR_OK:
                break;
            case LIB_ERR_NO_EVENT:
                thread_yield();
                break;
            default:
                USER_PANIC_ERR(err, "in event dispatch");
        }
    }
}


static void set_numa(unsigned id)
{
    /* nop */
}

static void xomp_synchronize(void)
{
    /* todo */
}

static void xomp_start_processing(void (*fn)(void *),
                                  void *data,
                                  unsigned nthreads)
{
    XMP_DEBUG("start processing fn:%p, data:%p, threads:%u\n", fn, data, nthreads);

    errval_t err;

    /* Create Threads and ask them to process the function specified */
    /* Let them die as soon as they are done */

    struct bomp_work *xdata;

    struct bomp_barrier *barrier;

    g_bomp_state->num_threads = nthreads;

    char *memory = calloc(1, sizeof(void *) + sizeof(*barrier) + sizeof(*xdata));
    assert(memory != NULL);

    g_bomp_state->tld = (struct bomp_thread_local_data **) memory;

    memory += sizeof(struct bomp_thread_local_data *);

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
    bomp_set_tls(xdata);

    struct xomp_task *task = calloc(1, sizeof(struct xomp_task));
    assert(task);
    task->arg = data;
    task->barrier = barrier;
    task->total_threads = nthreads;
    task->fn = fn;
    task->done = 1;  // set the main thread to done

    XMP_DEBUG("distributing work to Xeon Phi\n");

    err = xomp_master_do_work(task);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to distribute workers\n");
    }
}


static void xomp_end_processing(void)
{
    /* Cleaning of thread_local and work data structures */
    int i = 0;

    xomp_barrier_enter(g_bomp_state->tld[i]->work->barrier);

    /* Clear the barrier created */
    bomp_clear_barrier(g_bomp_state->tld[i]->work->barrier);

    free(g_bomp_state->tld);

    g_bomp_state->tld = NULL;
    g_bomp_state->num_threads = 1;
}


int bomp_xomp_init(void *arg)
{
    errval_t err;

    struct xomp_args *args = arg;

    xomp_st = calloc(1, sizeof(struct bomp_state ));
    if (xomp_st == NULL) {
        return -1;
    }
    xomp_st->backend_type = BOMP_BACKEND_XOMP;
    xomp_st->backend.get_thread = (backend_get_thread_fn_t)thread_self;
    xomp_st->backend.get_tls = thread_get_tls;
    xomp_st->backend.set_tls = thread_set_tls;
    xomp_st->backend.set_numa = set_numa;
    xomp_st->backend.thread_exit = thread_exit;
    xomp_st->backend.synchronize = xomp_synchronize;
    xomp_st->backend.start_processing = xomp_start_processing;
    xomp_st->backend.end_processing = xomp_end_processing;

    bomp_common_init(xomp_st);
    g_bomp_state = xomp_st;

    switch (args->type) {
        case XOMP_ARG_TYPE_WORKER:
            g_bomp_state = xomp_st;
            err = xomp_worker_init(args->args.worker.id);
            break;
        case XOMP_ARG_TYPE_UNIFORM:
        case XOMP_ARG_TYPE_DISTINCT:
            err = xomp_master_init(args);
            break;
        default:
            return XOMP_ERR_INVALID_ARGUMENTS;
            break;
    }

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Initializing library\n");
    }

    if (args->type == XOMP_ARG_TYPE_WORKER) {
        while(1) {
            messages_wait_and_handle_next();
        }
        return -1;
    }

    if (args->type == XOMP_ARG_TYPE_UNIFORM) {
        g_thread_limit = args->args.uniform.nthreads;
    } else {
        g_thread_limit = args->args.distinct.nthreads;
    }

    err = xomp_master_spawn_workers(g_thread_limit);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Master spawning workers\n");
    }

    omp_set_num_threads(g_thread_limit);

    return 0;
}
struct bomp_state * bomp_get_backend_state_xomp(void)
{
    return xomp_st;
}



