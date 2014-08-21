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

static uint8_t num_phi = 0;

void GOMP_barrier(void);
bool GOMP_single_start(void);

void backend_set_numa(unsigned id)
{
    /* nop */
}

void *backend_get_tls(void)
{
    return thread_get_tls();
}

void backend_set_tls(void *data)
{
    thread_set_tls(data);
}

void *backend_get_thread(void)
{
    return thread_self();
}

void backend_span_domain_default(int nos_threads)
{
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

#ifdef __x86_64__
    create_time = rdtsc();
#endif

    err = xomp_master_spawn_workers(nos_threads);
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
    errval_t err;

    struct xomp_master_args *xarg = arg;

    num_phi = xarg->num_phi;

    err = xomp_master_init(xarg);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "xomp master init failed\n");
    }
}

void backend_thread_exit(void)
{
    thread_exit();
}

struct thread *backend_thread_create_varstack(bomp_thread_func_t start_func,
                                              void *arg,
                                              size_t stacksize)
{
    struct thread *t = thread_create_varstack(start_func, arg, stacksize);
    errval_t err = thread_detach(t);
    assert(err_is_ok(err));
    return t;

}

void bomp_synchronize(void)
{
    /* if(GOMP_single_start()) { */
    //errval_t err = rsrc_phase(my_rsrc_id, 1);
   // assert(err_is_ok(err));
    /* } */
}
