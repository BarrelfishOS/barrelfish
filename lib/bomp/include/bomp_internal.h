/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 64, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __BOMP_INTERNAL_H
#define	__BOMP_INTERNAL_H

#include <assert.h>
#include <stdio.h>
#include <stdbool.h>
#include <omp.h>
#include <spin.h>
#include <mutex.h>
#include <environment.h>
#include <abi.h>
#include <icv.h>
#include <bomp_backend.h>

#include <barrelfish/barrelfish.h>


#if XOMP_BENCH_ENABLED
#define XOMP_BENCH_WORKER_EN 0
#define XOMP_BENCH_MASTER_EN 0
#include <bench/bench.h>
#else
#define XOMP_BENCH_WORKER_EN 0
#define XOMP_BENCH_MASTER_EN 0
#endif

#ifdef __k1om__
#define BOMP_DEFAULT_CORE_STRIDE 1
#else
/* TODO: this should been taken from the configuration i.e. use of 
 *       hyperthreads if enabled.
 */
#define BOMP_DEFAULT_CORE_STRIDE 1
#endif
typedef void (*bomp_thread_fn_t)(void *);

/**
 * \brief this struct stores thread local data such as the team / task
 *        of this thread
 */
struct bomp_thread
{
    bomp_thread_fn_t fn;
    void *arg;

    struct bomp_task *task;

};

struct bomp_work {
    void (*fn)(void *);
    void *data;
    unsigned thread_id;
    unsigned num_threads;
    unsigned num_vtreads;
    struct bomp_barrier *barrier;
};

struct bomp_thread_local_data {
    void *thr; // thread reference
    struct bomp_work *work;
    struct bomp_icv_data *icv;
};


extern uint32_t g_thread_limit;;
extern struct bomp_state *g_bomp_state;

/*
 * Functions to obtain the Backend specific state pointer
 */
struct bomp_state * bomp_get_backend_state_bomp(void);
struct bomp_state * bomp_get_backend_state_xomp(void);
struct bomp_state * bomp_get_backend_state_linux(void);

/* common state initialization */
void bomp_common_init(struct bomp_state *st);

/* obtaining ICV pointers */
static inline struct omp_icv_task *bomp_get_icv_task(void)
{
    struct omp_icv_task *icv = NULL;
    /*
     * TODO: get icv_task from thread
     */
    void *task = NULL;
    if (task) {
        //
    } else {
        assert(g_bomp_state);
        icv = &g_bomp_state->icv_task;
    }
    return icv;
}

static inline struct omp_icv_device *bomp_get_icv_device(void)
{
    assert(g_bomp_state);
    return &g_bomp_state->icv_dev;
}



void bomp_set_tls(void *xdata);



void bomp_start_processing(void (*fn) (void *), void *data, unsigned nthreads);
void bomp_end_processing(void);


#endif	/* _LIBBOMP_H */
