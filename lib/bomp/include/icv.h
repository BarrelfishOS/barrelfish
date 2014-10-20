/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _LIBBOMP_ICV_H
#define _LIBBOMP_ICV_H

/*
 * ---------------------------------------------------------------------------
 * 2.3 Internal Control Variables
 * ---------------------------------------------------------------------------
 *
 * An OpenMP implementation must act as if there are internal control variables
 * (ICVs) that control the behavior of an OpenMP program.
 * They are initialized by the implementation itself and may be given values
 * through OpenMP environment variables and through calls to OpenMP API routines.
 */

/**
 * \brief
 */
struct omp_icv_task
{
    /**
     * controls whether dynamic adjustment of the number of threads is enabled
     * for encountered parallel regions.
     *
     * Scope:          Data Environment
     * Initialization: OMP_DYNAMIC
     * Accessed:       omp_set_dynamic(), omp_get_dyamic()
     */
    uint8_t dynamic;

    /**
     * controls whether nested parallelism is enabled for encountered parallel
     * regions
     *
     * Scope:          Data Environment
     * Initialization: OMP_NESTED, set to false
     * Accessed:       omp_set_nested(), omp_get_nested()
     */
    uint8_t nested;

    /**
     * controls the number of threads requested for encountered parallel regions
     *
     * Scope:          Data Environment
     * Initialization: OMP_NUM_THREADS
     * Accessed:       omp_set_num_threads(), omp_get_num_threads()
     */
    uint32_t nthreads;

    /**
     * controls the maximum number of threads participating in the contention group
     *
     * Scope:          Data Environment
     * Initialization: OMP_THREAD_LIMIT
     * Accessed:       thread_limit (clause), omp_get_thread_limit()
     */
    uint32_t thread_limit;

    /**
     * controls the place partition available to the execution environment for
     * encountered parallel regions.
     *
     * Scope:          Data Environment
     * Initialization: OMP_PLACES
     */
    uint32_t place_partition;

    /**
     * the number of nested, active parallel regions enclosing the current ask
     * such that all of the parallel regions are enclosed by the outermost initial
     * task region on the current device.
     *
     * Scope:          Data Environment
     * Initialization: set to zero
     * Accessed:       omp_get_active_levels()
     */
    uint8_t active_levels;

    /**
     * the number of nested parallel regions enclosing the current task such that
     * all of the parallel regions are enclosed by the outermost initial task region
     * on the current device.
     *
     * Scope:          Data Environment
     * Initialization: set to zero
     * Accessed:       omp_get_level()
     */
    uint8_t levels;

#if OMP_VERSION >= OMP_VERSION_40
    /**
     * Controls the binding of OpenMP threads to places. When binding is requested,
     * the variable indicates that the execution environment is advised not to
     * move threads between places. The variable can also provide default thread
     * affinity policies.
     *
     * Scope:          Data Environment
     * Initialization: OMP_PROC_BIND
     * Accessed:       omp_get_proc_bind()
     */
    uint8_t bind;

    /**
     * controls the default target device.
     *
     * Scope:          Data Environment
     * Initialization: OMP_DEFAULT_DEVICE
     * Accessed:       omp_set_default_device(), omp_get_default_device()
     */
    uint8_t default_device;
#endif

    /**
     * controls the schedule that the runtime schedule clause uses for loop regions
     *
     * Scope:          Data Environment
     * Initialization: OMP_SCHEDULE
     * Accessed:       omp_set_schedule(), omp_get_schedule()
     */
    omp_sched_t run_sched;
    int         run_sched_modifier;
};

/**
 *
 */
struct omp_icv_device
{
    /**
     * controls the implementation defined default scheduling of loop loop regions
     *
     * Scope:          Device
     * Initialization: (none)
     */
    uint8_t dev_sched;

    /**
     * controls the stack size for threads that the OpenMP implementation creates
     *
     * Scope:          Device
     * Initialization: OMP_STACKSIZE
     */
    uint32_t stack_size;

    /**
     * controls the desired behavior of waiting threads.
     *
     * Scope:          Device
     * Initialization: OMP_WAIT_POLICY
     */
    uint8_t wait_policy;

#if OMP_VERSION >= OMP_VERSION_40
    /**
     * controls the desired behavior of the cancel construct and cancellation points
     *
     * Scope:          Device
     * Initialization: OMP_CANCELLATION
     * Accessed:       omp_get_cancellation()
     */
    uint8_t cancel;
#endif

    /**
     * The number of nested, active parallel regions enclosing the current ask
     * such that all of the parallel regions are enclosed by the outermost initial
     * task region on the current device.
     *
     * Scope:          Device
     * Initialization: OMP_MAX_ACTIVE_LEVELS
     * Accessed:       omp_set_max_active_levels(), omp_get_max_active_levels()
     */
    uint8_t max_active_levels;
};

struct omp_icv_task *bomp_icv_new(void);
void bomp_icv_init_from_parent(struct omp_icv_task *icv_task);
void bomp_icv_init_from_env(struct omp_icv_task *icv_task);
void bomp_icv_dev_init_from_env(struct omp_icv_device *icv_dev);


#define OMP_SET_ICV_DEV(_var, _val) bomp_get_icv_device()->_var = (_val)
#define OMP_GET_ICV_DEV(_var) (bomp_get_icv_device()->_var)
#define OMP_SET_ICV_TASK(_var, _val) (bomp_get_icv_task())->_var = (_val)
#define OMP_GET_ICV_TASK(_var) ((bomp_get_icv_task())->_var)


#endif	/* _LIBBOMP_ENVIRONEMNT_H */
