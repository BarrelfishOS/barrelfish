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
 * This functions implement the SECTIONS construct
 *
 * #pragma omp sections
 * {
 *  #pragma omp section
 *  stmt1;
 *  #pragma omp section
 *  stmt2;
 *  #pragma omp section
 *  stmt3;
 *  }
 * becomes
 *
 * for (i = GOMP_sections_start (3); i != 0; i = GOMP_sections_next ())
 *  switch (i) {
 *    case 1:
 *      stmt1;
 *      break;
 *    case 2:
 *      stmt2;
 *      break;
 *    case 3:
 *      stmt3;
 *      break;
 *    }
 *
 *    GOMP_barrier ();
 */

unsigned GOMP_sections_start(unsigned count)
{
    assert(!"NYI");
    return 0;
}

unsigned GOMP_sections_next(void)
{
    assert(!"NYI");
    return 0;
}

void GOMP_parallel_sections_start(void (*fn)(void *),
                                  void *data,
                                  unsigned num_threads,
                                  unsigned count)
{
    assert(!"NYI");
}

void GOMP_parallel_sections(void (*fn)(void *),
                            void *data,
                            unsigned num_threads,
                            unsigned count,
                            unsigned flags)
{
    assert(!"NYI");
}

void GOMP_sections_end(void)
{
    assert(!"NYI");
}

bool GOMP_sections_end_cancel(void)
{

    assert(!"NYI");
    return 0;
}

void GOMP_sections_end_nowait(void)
{
    assert(!"NYI");
}
