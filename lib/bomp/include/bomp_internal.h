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

#include <barrelfish/barrelfish.h>

/*
 * typedefs of backend specific functions
 */
typedef int (*bomp_thread_func_t)(void *);
typedef void (*backend_set_numa_fn_t)(unsigned id);
typedef void (*backend_start_processing_fn_t)(void (*fn)(void *), void *data, unsigned nthreads);
typedef void* (*backend_get_tls_fn_t)(void);
typedef void (*backend_synchronize_fn_t)(void);
typedef void (*backend_set_tls_fn_t)(void *data);
typedef void* (*backend_get_thread_fn_t)(void);
typedef void (*backend_thread_exit_fn_t)(void);
typedef void (*backend_end_processing_fn_t)(void);
typedef struct thread *(*backend_thread_create_fn_t)(bomp_thread_func_t start_func,
                                              void *arg, size_t stacksize);

struct bomp_backend_fn
{
    backend_set_tls_fn_t set_tls;
    backend_get_tls_fn_t get_tls;
    backend_set_numa_fn_t set_numa;
    backend_get_thread_fn_t get_thread;
    backend_start_processing_fn_t start_processing;
    backend_end_processing_fn_t end_processing;
    backend_thread_exit_fn_t thread_exit;
    backend_synchronize_fn_t synchronize;
};

struct bomp_work {
    void (*fn)(void *);
    void *data;
    unsigned thread_id;
    unsigned num_threads;
    struct bomp_barrier *barrier;
};

struct bomp_thread_local_data {
    void *thr; // thread reference
    struct bomp_work *work;
};

struct bomp_state
{
    bomp_backend_t backend_type;
    struct bomp_backend_fn backend;
    bomp_mutex_t critical_lock;
    bomp_lock_t atomic_lock;
    volatile uint32_t nested;
    uint32_t num_threads;
    uint32_t bomp_threads;
    bool behaviour_nested;
    bool behaviour_dynamic;
    struct bomp_thread_local_data **tld;
};

extern struct bomp_state *g_bomp_state;


struct bomp_state * bomp_get_backend_state_bomp(void);
struct bomp_state * bomp_get_backend_state_xomp(void);
struct bomp_state * bomp_get_backend_state_linux(void);

void bomp_common_init(struct bomp_state *st);
void bomp_set_tls(void *xdata);



void bomp_start_processing(void (*fn) (void *), void *data, unsigned nthreads);
void bomp_end_processing(void);


/* These functions are called from GCC-generated code */
void GOMP_atomic_start(void);
void GOMP_atomic_end(void);

/* barrier.c */
void GOMP_barrier(void);
bool GOMP_barrier_cancel (void);

/* critical.c */
void GOMP_critical_start(void);
void GOMP_critical_end(void);
void GOMP_critical_name_start(void **pptr);
void GOMP_critical_name_end(void **pptr);

/* loop.c */
bool GOMP_loop_dynamic_start (long start, long end, long incr, long chunk_size,
                              long *istart, long *iend);
bool GOMP_loop_ordered_runtime_start(long start, long end, long incr,
                                     long *istart,long *iend);
bool GOMP_loop_dynamic_next (long *istart, long *iend);
bool GOMP_loop_runtime_next(long *istart, long *iend);
bool GOMP_loop_ordered_runtime_next(long *istart, long *iend);
void GOMP_loop_end_nowait (void);
void GOMP_loop_end (void);

/* ordered.c */
void GOMP_ordered_start(void);
void GOMP_ordered_end(void);

/* parallel.c */
void GOMP_parallel(void (*fn)(void *), void *data, unsigned num_threads, unsigned int flags);
void GOMP_parallel_start(void (*) (void *), void *, unsigned);
void GOMP_parallel_end(void);
bool GOMP_cancel(int which, bool do_cancel);
bool GOMP_cancellation_point(int which);

/* sections.c */
unsigned GOMP_sections_start(unsigned count);
unsigned GOMP_sections_next(void);
void GOMP_parallel_sections_start(void (*fn)(void *), void *data,
                                  unsigned num_threads, unsigned count);
void GOMP_parallel_sections(void (*fn)(void *), void *data, unsigned num_threads,
                            unsigned count, unsigned flags);
void GOMP_sections_end(void);
bool GOMP_sections_end_cancel(void);
void GOMP_sections_end_nowait(void);

/* single.c */
bool GOMP_single_start(void);
void *GOMP_single_copy_start (void);
void GOMP_single_copy_end (void *data);


#endif	/* _LIBBOMP_H */
