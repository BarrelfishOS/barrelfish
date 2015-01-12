/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <bomp_internal.h>

#if 0

/**
 * \brief allocated an initializes a new task ICV set
 *
 * \returns pointer to the ICV task struct
 *
 * The struct is initialized based on the parent
 */
struct omp_icv_task *bomp_icv_new(void)
{
    assert(!"NYI");
    return 0;
}

/**
 * \brief Initializes the task specific ICV from the parent task
 *
 * \param icv_task  ICV task struct to initialize
 *
 * If there is no parent task, then the global ICVs are taken
 */
void bomp_icv_init_from_parent(struct omp_icv_task *icv_task)
{
    assert(!"NYI");
}

/**
 * \brief Initializes the task specific ICV from the environment
 *
 * \param icv_task  ICV task struct to initialize
 *
 * This function initializes the task ICV based on the values defined in
 * environment.h.
 *
 * The function may only be called to initialize the global task ICV
 */
void bomp_icv_init_from_env(struct omp_icv_task *icv_task)
{
    icv_task->dynamic = OMP_DYNAMIC;
    icv_task->nested = OMP_NESTED;
    icv_task->nthreads = g_thread_limit;
    icv_task->thread_limit = g_thread_limit;
    icv_task->place_partition = OMP_PLACES;
    icv_task->active_levels = 0;
    icv_task->levels=0;
    icv_task->run_sched = OMP_SCHEDULE;
    icv_task->run_sched_modifier = 0;
#if OMP_VERSION >= OMP_VERSION_40
    icv_task->bind = OMP_PROC_BIND;
    icv_task->default_device = OMP_DEFAULT_DEVICE;
#endif
}

/**
 * \brief Initializes the device specific ICV from the environment
 *
 * \param icv_dev   ICV task struct to initialize
 *
 * This function initializes the device ICV based on the values defined in
 * environment.h.
 */
void bomp_icv_dev_init_from_env(struct omp_icv_device *icv_dev)
{
    icv_dev->dev_sched = 0;
    icv_dev->stack_size = OMP_STACKSIZE;
    icv_dev->wait_policy = OMP_WAIT_POLICY;
    icv_dev->max_active_levels = OMP_MAX_ACTIVE_LEVELS;
#if OMP_VERSION >= OMP_VERSION_40
    icv_dev->cancel = OMP_CANCELLATION;
#endif
}
#endif
