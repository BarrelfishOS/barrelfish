/**
 * \file
 * \brief API to use the bomp library
 */

/*
 * Copyright (c)2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <bomp_internal.h>

void bomp_start_processing(void (*fn)(void *),
                           void *data,
                           coreid_t tid_start,
                           coreid_t nthreads)
{
    struct bomp_tls *tls = thread_get_tls();

    debug_printf("bomp_start_processing(%p, %p, %u, %u)\n", fn, data, tid_start, nthreads);

    /* this function must only be called by the program and node masters */
    assert(tls->role == BOMP_THREAD_ROLE_MASTER || tls->role == BOMP_THREAD_ROLE_NODE);

    /* add one to the tid_start as this will be our ID */
    coreid_t tid_current = tid_start + 1;

    struct bomp_node *node;

    if (tls->role == BOMP_THREAD_ROLE_MASTER) {
        node = &tls->r.master.local;

        if (nthreads > (node->threads_max + 1)) {
            /* send the requests to the node masters */
            nthreads -= (node->threads_max + 1);
            for (nodeid_t i = 0; i < tls->r.master.num_nodes; ++i) {
                coreid_t num = bomp_node_exec(&tls->r.master.nodes[i], fn, data, tid_start, nthreads);
                assert(num <= nthreads);
                tls->r.master.nodes_active++;
                nthreads -= num;
                tid_current += num;
                if (nthreads == 0) {
                    break;
                }
            }
            nthreads += (node->threads_max);
        }
    } else if (tls->role == BOMP_THREAD_ROLE_NODE) {
        node = &tls->r.node;
    }

    debug_printf("nthreads=%u, max_threads=%u\n", nthreads, node->threads_max);

    assert((node->threads_max + 1)>= nthreads);

    struct omp_icv_task *icv = bomp_icv_get()->task;

    for (coreid_t i = 1; i < nthreads; ++i) {
        node->threads[i].icvt = icv;
        node->threads_active++;
        bomp_thread_exec(&node->threads[i], fn, data, tid_current);
        tid_current++;
    }

    /* set the local thread ID */
    tls->thread_id = 0;

    return;
#if 0
    /* Create Threads and ask them to process the function specified */
    /* Let them die as soon as they are done */
    unsigned i;
    struct bomp_work *xdata;
    struct bomp_barrier *barrier;

    g_bomp_state->num_threads = nthreads;

    char *memory = calloc(
                    1,
                    nthreads * sizeof(struct bomp_thread_local_data *)
                                    + sizeof(struct bomp_barrier)
                                    + nthreads * sizeof(struct bomp_work));
    assert(memory != NULL);

    g_bomp_state->tld = (struct bomp_thread_local_data **) memory;
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
    bomp_set_tls(xdata);

    for (i = 1; i < nthreads; i++) {
        xdata = (struct bomp_work *) memory;
        memory += sizeof(struct bomp_work);

        xdata->fn = fn;
        xdata->data = data;
        xdata->thread_id = i;
        xdata->barrier = barrier;

        /* Create threads */
        bomp_run_on(i * BOMP_DEFAULT_CORE_STRIDE + THREAD_OFFSET, bomp_thread_fn,
                    xdata);
    }
#endif
}

void bomp_end_processing(void)
{
    debug_printf("bomp_end_processing\n");
    struct bomp_tls *tls = thread_get_tls();
    struct waitset *ws = get_default_waitset();
    if (tls->role == BOMP_THREAD_ROLE_MASTER) {
        struct bomp_node *node = &tls->r.master.local;
        struct bomp_master *master = &tls->r.master;
        while(master->nodes_active != 1 || node->threads_active != 1) {
            event_dispatch(ws);
        }
    } else if (tls->role == BOMP_THREAD_ROLE_NODE) {
        struct bomp_node *node = &tls->r.node;
        while(node->threads_active != 0) {
            event_dispatch(ws);
        }
    }

    free(tls->icv.task);
    tls->icv.task = NULL;

    debug_printf("bomp_end_processing: done\n");
}
