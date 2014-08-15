/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <bomp_internal.h>

void GOMP_parallel_start(void (*fn)(void *),
                         void *data,
                         unsigned nthreads)
{
    assert(g_bomp_state != NULL);

    /* Identify the number of threads that can be spawned and start the processing */
    if (!omp_in_parallel()) {
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
    assert(!"NYI");
}

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
