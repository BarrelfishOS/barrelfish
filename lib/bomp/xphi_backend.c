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
#include <xomp/xomp.h>
#include "backend.h"
#include "omp.h"

#define DEBUG(x...) debug_printf("BACKEND: " x);



void GOMP_barrier(void);
bool GOMP_single_start(void);

void backend_set_numa(unsigned id)
{
    /* nop */
    DEBUG("backend_set_numa\n");
}

void *backend_get_tls(void)
{
    DEBUG("backend_get_tls\n");
    return thread_get_tls();
}

void backend_set_tls(void *data)
{
    DEBUG("backend_set_tls\n");
    thread_set_tls(data);
}

void *backend_get_thread(void)
{
    DEBUG("backend_get_thread\n");
    return thread_self();
}


/**
 * \brief is called when the bomp section is entered
 *
 *         -> this has to be fast
 */
void backend_run_func_on(int core_id,
                         void* cfunc,
                         void *arg)
{
    DEBUG("backend_run_func_on\n");
/*
    int actual_id = core_id + disp_get_core_id();
    thread_func_t func = (thread_func_t) cfunc;
    errval_t err = domain_thread_create_on_varstack(actual_id, func, arg,
                                                    thread_stack_size);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "domain_thread_create_on failed");
        printf("domain_thread_create_on failed on %d\n", actual_id);
        assert(err_is_ok(err));
    }
    */
}


void backend_span_domain_default(int nos_threads)
{
    DEBUG("backend_span_domain_default\n");
    backend_span_domain(nos_threads, THREADS_DEFAULT_STACK_BYTES);
}

static uint64_t create_time;



/**
 * \brief is called upon initialization of the program
 *
 *        -> bootstrap code
 */
void backend_span_domain(int nos_threads,
                         size_t stack_size)
{
    errval_t err;

    DEBUG("backend_span_domain\n");

#ifdef __x86_64__
    create_time = rdtsc();
#endif
    /* subtract one for the main thread */
    err = xomp_master_spawn_workers(nos_threads-1);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Master spawning workers\n");
    }

#ifdef __x86_64__
    create_time = rdtsc() - create_time;
#endif

}

void backend_create_time(int cores)
{
    printf("Createtime %d %" PRIu64 "\n", cores, create_time);
}

void backend_init(void *arg)
{
    DEBUG("backend_init\n");

    errval_t err;

    struct xomp_master_args *xarg = arg;

    err =  xomp_master_init(xarg->num_phi, xarg->path, xarg->argc, xarg->argv);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "xomp master init failed\n");
    }
}

void backend_thread_exit(void)
{
    DEBUG("backend_thread_exit\n");
    thread_exit();
}

struct thread *backend_thread_create_varstack(bomp_thread_func_t start_func,
                                              void *arg,
                                              size_t stacksize)
{
    DEBUG("backend_thread_create_varstack\n");
/*
    struct thread *t = thread_create_varstack(start_func, arg, stacksize);
    errval_t err = thread_detach(t);
    assert(err_is_ok(err));
    return t;
    */
    return NULL;
}

void bomp_synchronize(void)
{
    DEBUG("bomp_synchronize\n");
#if 0
    /* if(GOMP_single_start()) { */
    errval_t err = rsrc_phase(my_rsrc_id, 1);
    assert(err_is_ok(err));
    /* } */
#endif
}
