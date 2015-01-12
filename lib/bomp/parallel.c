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
    assert(g_bomp_state != NULL);

    /*
     * TODO:
     * 1) work out how many threads can be usedfor executing the parallel task
     * 2) create a new team for solving the task
     * 3) start the team work
     */

    /* Identify the number of threads that can be spawned and start the processing */
    if (!omp_in_parallel()) {
        g_bomp_state->bomp_threads = omp_get_max_threads();
        if (nthreads == 0 || (g_bomp_state->behaviour_dynamic
                                && g_bomp_state->num_threads < nthreads)) {

            nthreads = g_bomp_state->bomp_threads;
        }
        g_bomp_state->backend.start_processing(fn, data, nthreads);
    }
    g_bomp_state->nested++;
}

void GOMP_parallel_end(void)
{
    /*
     * TODO:
     * 1)
     */
    assert(g_bomp_state != NULL);
    if (g_bomp_state->nested == 1) {
        g_bomp_state->backend.end_processing();
    }
    g_bomp_state->nested--;
}

void GOMP_parallel(void (*fn)(void *),
                   void *data,
                   unsigned num_threads,
                   unsigned int flags)
{
    /*
     * TODO:
     * 1)  work out how many threads
     * 2)  allocate and start a new team
     * 3) call the function
     * 4) call parallel end
     */

    GOMP_parallel_start(fn, data, num_threads);
    fn(data);
    GOMP_parallel_end();
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
