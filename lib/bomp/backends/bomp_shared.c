/**
 * \file
 * \brief Implementation of backend functions on barrelfish
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
#include <barrelfish/dispatch.h>
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/sys_debug.h>
#include <barrelfish/resource_ctrl.h>
#include <bomp_internal.h>

// 1MB stack
#define STACK_BYTES (1UL << 20)

static struct bomp_state *bomp_st;

static rsrcid_t my_rsrc_id;

//static const char *my_manifest = "B 1\n"             // Normal phase
//                "G 80 160 80 480\n";// Gang phase

static void set_numa(unsigned id)
{
    /* nop */
}

static size_t thread_stack_size = 0;

static void bomp_run_on(int core_id,
                        void* cfunc,
                        void *arg)
{
    int actual_id = core_id + disp_get_core_id();
    thread_func_t func = (thread_func_t) cfunc;

    errval_t err = domain_thread_create_on_varstack(actual_id, func, arg,
                                                    thread_stack_size, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "domain_thread_create_on failed");
        printf("domain_thread_create_on failed on %d\n", actual_id);
        assert(err_is_ok(err));
    }
}

static int bomp_thread_fn(void *xdata)
{
    struct bomp_work *work_data = xdata;

    g_bomp_state->backend.set_numa(work_data->thread_id);

    bomp_set_tls(work_data);
    work_data->fn(work_data->data);
    /* Wait for the Barrier */
    bomp_barrier_wait(work_data->barrier);
    thread_detach(thread_self());
    thread_exit(0); // XXX: should return work_fn return value?
    return 0;
}

#define THREAD_OFFSET   0
/* #define THREAD_OFFSET   12 */

void bomp_start_processing(void (*fn)(void *),
                           void *data,
                           unsigned nthreads)
{
    assert(g_bomp_state);

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
}

void bomp_end_processing(void)
{
    /* Cleaning of thread_local and work data structures */
    int i = 0;

    bomp_barrier_wait(g_bomp_state->tld[i]->work->barrier);

    /* Clear the barrier created */
    bomp_clear_barrier(g_bomp_state->tld[i]->work->barrier);

    free(g_bomp_state->tld);
    g_bomp_state->tld = NULL;
    g_bomp_state->num_threads = 1;
}

static struct thread_sem init_sem = THREAD_SEM_INITIALIZER
;

static int remote_init(void *dumm)
{
//    errval_t err = rsrc_join(my_rsrc_id);
    //assert(err_is_ok(err));

    thread_sem_post(&init_sem);
    thread_detach(thread_self());
    return 0;
}

static int cores_initialized = 1;

static void domain_init_done(void *arg,
                             errval_t err)
{
    assert(err_is_ok(err));
    cores_initialized++;
}

static void bomp_span_domain(int nos_threads,
                             size_t stack_size)
{
    int my_core_id = disp_get_core_id();

    errval_t err;

    // Remember default stack size
    thread_stack_size = stack_size;

    // Submit manifest (derived from program)
//     = rsrc_manifest(my_manifest, &my_rsrc_id);
//
//    if (err_is_fail(err)) {
 //       DEBUG_ERR(err, "rsrc_manifest");
 //       abort();
 //   }

    /* Span domain to all cores */
    for (int i = 1; i < nos_threads; ++i) {
        //for (int i = my_core_id + BOMP_DEFAULT_CORE_STRIDE; i < nos_threads + my_core_id; i++) {
        coreid_t core = my_core_id + (i * BOMP_DEFAULT_CORE_STRIDE);
        err = domain_new_dispatcher(core, domain_init_done, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to span domain");
            printf("Failed to span domain to %d\n", i);
            assert(err_is_ok(err));
        }
    }

    while (cores_initialized < nos_threads) {
        thread_yield();
    }

    /* Run a remote init function on remote cores */
    //for (int i = my_core_id + 1; i < nos_threads + my_core_id; i++) {
    for (int i = 1; i < nos_threads; ++i) {
        coreid_t core = my_core_id + (i * BOMP_DEFAULT_CORE_STRIDE);
        err = domain_thread_create_on(core, remote_init, NULL, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "domain_thread_create_on failed");
            printf("domain_thread_create_on failed on %d\n", i);
            assert(err_is_ok(err));
        }
        thread_sem_wait(&init_sem);
    }
}

static void bomp_synchronize(void)
{
    /* if(GOMP_single_start()) { */
    errval_t err = rsrc_phase(my_rsrc_id, 1);
    assert(err_is_ok(err));
    /* } */
}

int bomp_bomp_init(uint32_t nthreads)
{
    return bomp_bomp_init_varstack(nthreads, STACK_BYTES);
}

int bomp_bomp_init_varstack(uint32_t nthreads, size_t stack_size)
{

    if (bomp_st != NULL) {
        debug_printf("bomp_bomp_init: already initialized\n");
        return 0;
    }

    debug_printf("bomp_bomp_init: nthreads=%u\n", nthreads);

    bomp_st = calloc(1, sizeof(struct bomp_state));
    if (bomp_st == NULL) {
        return -1;
    }

    // make sure stack size is multiple of page size
    stack_size = ROUND_UP(stack_size, BASE_PAGE_SIZE);

    g_thread_limit = nthreads;

    bomp_st->backend_type = BOMP_BACKEND_BOMP;
    bomp_st->backend.get_thread = (backend_get_thread_fn_t) thread_self;
    bomp_st->backend.get_tls = thread_get_tls;
    bomp_st->backend.set_tls = thread_set_tls;
    bomp_st->backend.set_numa = set_numa;
    bomp_st->backend.thread_exit = thread_exit;
    bomp_st->backend.synchronize = bomp_synchronize;
    bomp_st->backend.start_processing = bomp_start_processing;
    bomp_st->backend.end_processing = bomp_end_processing;
    bomp_common_init(bomp_st);
    g_bomp_state = bomp_st;
    bomp_span_domain(nthreads, stack_size);
    return 0;
}

int bomp_run_main(main_func_t mainfunc, void *mainarg, size_t stacksize)
{
    // we need to create a thread with a big enough stack on Barrelfish as the
    // default stack size for threads is 64kB which is nowhere near enough for
    // the fft code.
    int retval;
    struct thread *mt = thread_create_varstack(mainfunc, mainarg, stacksize);
    errval_t err = thread_join(mt, &retval);
    assert(err_is_ok(err));
    return retval;
}

struct bomp_state * bomp_get_backend_state_bomp(void)
{
    return bomp_st;
}
