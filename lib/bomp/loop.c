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
 * this implements the FOR constructs
 *
 * #pragma omp parallel for
 * for (i = lb; i <= ub; i++)
 *  body;
 *
 * becomes
 *
 * void subfunction (void *data) {
 *   long _s0, _e0;
 *   while (GOMP_loop_static_next (&_s0, &_e0)) {
 *      long _e1 = _e0, i;
 *      for (i = _s0; i < _e1; i++)
 *          body;
 *   }
 *   GOMP_loop_end_nowait ();
 * }
 * GOMP_parallel_loop_static (subfunction, NULL, 0, lb, ub+1, 1, 0);
 * subfunction (NULL);
 * GOMP_parallel_end ();
 */

bool GOMP_loop_ordered_runtime_start(long start,
                                     long end,
                                     long incr,
                                     long *istart,
                                     long *iend)
{
    assert(!"NYI");
    return 0;
}

bool GOMP_loop_dynamic_start(long start,
                             long end,
                             long incr,
                             long chunk_size,
                             long *istart,
                             long *iend)
{
    assert(!"NYI");
    return 0;
}

bool GOMP_loop_runtime_next(long *istart,
                            long *iend)
{
    assert(!"NYI");
    return 0;
}

bool GOMP_loop_ordered_runtime_next(long *istart,
                                    long *iend)
{
    assert(!"NYI");
    return 0;
}

bool GOMP_loop_dynamic_next(long *istart,
                            long *iend)
{
    assert(!"NYI");
    return 0;
}

void GOMP_loop_end_nowait(void)
{
    assert(!"NYI");
}

void GOMP_loop_end(void)
{
    assert(!"NYI");
}

