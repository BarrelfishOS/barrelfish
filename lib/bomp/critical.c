/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <bomp_internal.h>

void GOMP_critical_start(void)
{
    assert(g_bomp_state);
    bomp_mutex_lock(&g_bomp_state->critical_lock);
}

void GOMP_critical_end(void)
{
    assert(g_bomp_state);
    bomp_mutex_unlock(&g_bomp_state->critical_lock);
}

void GOMP_critical_name_start(void **pptr)
{
    assert(!"NYI");
}

void GOMP_critical_name_end(void **pptr)
{
    assert(!"NYI");
}
