/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 64, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __BOMP_BACKEND_H
#define __BOMP_BACKEND_H


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
typedef void (*backend_thread_exit_fn_t)(int status);
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

struct bomp_state
{
    bomp_backend_t backend_type;    ///< the type of the backend for this state
    struct bomp_backend_fn backend; ///< backend specific functions
    struct omp_icv_device icv_dev; ///< global icv device
    struct omp_icv_task icv_task;  ///< global icv task
    bomp_mutex_t critical_lock;
    bomp_lock_t atomic_lock;
    volatile uint32_t nested;
    uint32_t num_threads;
    uint32_t bomp_threads;
    bool behaviour_nested;
    bool behaviour_dynamic;
    struct bomp_thread_local_data **tld;
};


#endif	/* __BOMP_TEAM_H */
