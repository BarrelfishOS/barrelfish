/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <bomp_internal.h>

/*
 * These functions implement the PARALLEL construct
 *
 * #pragma omp parallel
 * {
 *  body;
 * }
 *
 * is translated into
 * void subfunction (void *data)
 * {
 *  use data;
 *  body;
 *  }
 *  setup data;
 *  GOMP_parallel_start (subfunction, &data, num_threads);
 *  subfunction (&data);
 *  GOMP_parallel_end ();
 */

void GOMP_parallel_start(void (*fn)(void *),
                         void *data,
                         unsigned nthreads)
{
    debug_printf("GOMP_parallel_start(%p, %p, %u)\n", fn, data, nthreads);

    /* Identify the number of threads that can be spawned and start the processing */
    if (!omp_in_parallel()) {
        debug_printf("not in parallel\n");

        struct omp_icv_task *icv_task = bomp_icv_task_new();
        if (!icv_task) {
            debug_printf("no icv task\n");
            return;
        }

        icv_task->active_levels = 1;
        icv_task->nthreads = omp_get_max_threads();
        debug_printf("omp_get_max_threads = %u\n", icv_task->nthreads);

        if (nthreads == 0 || (icv_task->dynamic && icv_task->nthreads < nthreads)) {
            icv_task->nthreads = OMP_GET_ICV_GLOBAL(thread_limit);
            debug_printf("resetting to = %u\n", icv_task->nthreads);
        }

        bomp_icv_set_task(icv_task);
        debug_printf("icv task set %u\n", icv_task->nthreads);

        /* start processing */
        bomp_start_processing(fn, data, 0, icv_task->nthreads);
    } else {
        if (omp_get_nested()) {
            // handle nested paralellism
            assert(!"Handling nested paralellism\n");
        }

        /* we have already started enough threads */
        uint32_t active_levels = OMP_GET_ICV_TASK(active_levels);
        //debug_printf("setting active_levels to %u\n", active_levels+1);

        OMP_SET_ICV_TASK(active_levels, active_levels+1);
    }
}

void GOMP_parallel_end(void)
{
//    debug_printf("GOMP_parallel_end\n");

    uint32_t active_levels = OMP_GET_ICV_TASK(active_levels);

    if (active_levels == 1) {
        bomp_end_processing();
    } else  {
//        debug_printf("setting active_levels to %u\n", active_levels-1);
        OMP_SET_ICV_TASK(active_levels, active_levels-1);
    }


    debug_printf("GOMP_parallel_end end\n");
}

void GOMP_parallel(void (*fn)(void *),
                   void *data,
                   unsigned num_threads,
                   unsigned int flags)
{
    debug_printf("GOMP_parallel");
    assert(!"NYI");
}

#if OMP_VERSION >= OMP_VERSION_40
bool GOMP_cancel(int which,
                 bool do_cancel)
{
    assert(!"NYI");
    return 0;
}

bool GOMP_cancellation_point(int which)
{
    assert(!"NYI");
    return 0;
}
#endif
