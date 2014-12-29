/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <bomp_internal.h>

void GOMP_atomic_start(void)
{
    assert(!"NYI");
//    assert(g_bomp_state);
//    bomp_lock(&g_bomp_state->atomic_lock);
}

void GOMP_atomic_end(void)
{
    assert(!"NYI");
//    assert(g_bomp_state);
//    bomp_unlock(&g_bomp_state->atomic_lock);
}
