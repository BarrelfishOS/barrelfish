/**
 * \file
 * \brief Decleration of functions that gcc pragmas calls
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include "libbomp.h"
#include "backend.h"
#include "omp.h"

bool GOMP_loop_dynamic_start (long start, long end, long incr, long chunk_size,
                              long *istart, long *iend)
{
    return 0;
}

bool GOMP_loop_dynamic_next (long *istart, long *iend)
{
    return 0;
}

void GOMP_loop_end_nowait (void)
{

}

void GOMP_loop_end (void)
{

}


