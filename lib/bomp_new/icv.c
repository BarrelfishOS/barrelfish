/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <bomp_internal.h>

struct omp_icv_device g_omp_icv_device_default;
struct omp_icv_global g_omp_icv_global_default;
struct omp_icv_task g_omp_icv_task_default;

void bomp_icv_init_default(coreid_t nthreads)
{
    /* Global Control Variables */
    struct omp_icv_global *icv_glob = &g_omp_icv_global_default;
    icv_glob->thread_limit = nthreads;
    icv_glob->time_start = rdtsc();

    /* Device Control Variables */
    struct omp_icv_device *icv_dev = &g_omp_icv_device_default;
    icv_dev->dev_sched = 0;
    icv_dev->stack_size = OMP_STACKSIZE;
    icv_dev->wait_policy = OMP_WAIT_POLICY;
    icv_dev->max_active_levels = OMP_MAX_ACTIVE_LEVELS;
#if OMP_VERSION >= OMP_VERSION_40
    icv_dev->cancel = OMP_CANCELLATION;
#endif

    struct omp_icv_task *icv_task = &g_omp_icv_task_default;
    /* Task Control Variables */
    icv_task->dynamic = OMP_DYNAMIC;
    icv_task->nested = OMP_NESTED;
    icv_task->nthreads = nthreads;
    icv_task->thread_limit = nthreads;
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
 * \brief allocated an initializes a new task ICV set
 *
 * \returns pointer to the ICV task struct
 *
 * The struct is initialized based on the parent
 */
struct omp_icv_task *bomp_icv_task_new(void)
{
    struct omp_icv_task *icv = calloc(1, sizeof(*icv));
    if (icv == NULL) {
        return icv;
    }

    memcpy(icv, &g_omp_icv_task_default, sizeof(*icv));

    return icv;
}
