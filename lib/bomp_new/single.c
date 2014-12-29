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
 * this functions implement the SINGLE construct
 *
 *  #pragma omp single
 *  {
 *    body;
 *  }
 *
 * becomes
 *
 *  if (GOMP_single_start ())
 *      body;
 *  GOMP_barrier ();
 *
 *  and
 *
 *  #pragma omp single copyprivate(x)
 *  {
 *    body;
 *  }
 *
 *  becomse
 *
 *  datap = GOMP_single_copy_start ();
 *  if (datap == NULL) {
 *      body;
 *      data.x = x;
 *      GOMP_single_copy_end (&data);
 *  } else {
 *      x = datap->x;
 *  }
 *  GOMP_barrier ();
 */

/* This function should return true for just the first thread */
bool GOMP_single_start(void)
{
    assert(!"NYI");
    return 0;
}

void *GOMP_single_copy_start (void)
{
    assert(!"NYI");
    return NULL;
}

void GOMP_single_copy_end (void *data)
{
    assert(!"NYI");
}
