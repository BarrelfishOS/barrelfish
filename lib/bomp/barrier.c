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
 * These functions implement the BARRIER construct
 */

void GOMP_barrier(void)
{
    assert(g_bomp_state);

    struct bomp_thread_local_data *th_local_data = g_bomp_state->backend.get_tls();
    assert(th_local_data != NULL);
    bomp_barrier_wait(th_local_data->work->barrier);
}

bool GOMP_barrier_cancel (void)
{
    assert(!"NYI");
    return 0;
}
