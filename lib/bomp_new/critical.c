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
 * These functions implement the OpenMP CRITICAL construct
 *
 * With a specified name, use omp set lock and omp unset lock with name being
 * transformed into a variable declared like
 *
 * omp_lock_t gomp_critical_user_<name> __attribute__((common))
 * Ideally the ABI would specify that all zero is a valid unlocked state, and
 * so we wouldn’t need to initialize this at startup.
 */

void GOMP_critical_start(void)
{
    assert(!"NYI");
//    assert(g_bomp_state);
//    bomp_mutex_lock(&g_bomp_state->critical_lock);
}

void GOMP_critical_end(void)
{
    assert(!"NYI");
//    assert(g_bomp_state);
//    bomp_mutex_unlock(&g_bomp_state->critical_lock);
}

void GOMP_critical_name_start(void **pptr)
{
    assert(!"NYI");
}

void GOMP_critical_name_end(void **pptr)
{
    assert(!"NYI");
}
