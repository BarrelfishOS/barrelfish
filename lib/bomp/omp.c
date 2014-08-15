/**
 * \file
 * \brief API to use the bomp library
 */

/*
 * Copyright (c)2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <bomp_internal.h>

void omp_set_num_threads(int num_threads)
{
    assert(g_bomp_state);

    if (num_threads > 0) {
        g_bomp_state->bomp_threads = num_threads;
    }
}

int omp_get_num_threads(void)
{
    if (g_bomp_state) {
        return g_bomp_state->num_threads;
    }
    return 0;
}

int omp_get_max_threads(void)
{
    /*
     * TODO: give something meaning full back when a new team is formed
     */
    return 1;
}

int omp_get_thread_num(void)
{
    if (g_bomp_state) {
        if (g_bomp_state->num_threads == 1) {
            return 0;
        }
        struct bomp_thread_local_data *tls = g_bomp_state->backend.get_tls();
        return tls->work->thread_id;
    }
    return 0;
}

int omp_get_num_procs(void)
{
    return 1;
}

int omp_in_parallel(void)
{
    if (g_bomp_state) {
        if (g_bomp_state->num_threads == 1) {
            return 0;
        }
        return g_bomp_state->num_threads;
    }
    return 0;
}

void omp_set_dynamic(int dynamic_threads)
{
    assert(g_bomp_state);

    if (dynamic_threads == 0) {
        g_bomp_state->behaviour_dynamic = false;
    } else {
        g_bomp_state->behaviour_dynamic = true;
    }
}

int omp_get_dynamic(void)
{
    if (g_bomp_state) {
        return g_bomp_state->behaviour_dynamic;
    }
    return 0;
}

void omp_set_nested(int nested)
{
    assert(g_bomp_state);

    if (nested == 0) {
        g_bomp_state->behaviour_nested = false;
    } else {
        g_bomp_state->behaviour_nested = true;
    }
}

int omp_get_nested(void)
{
    if (g_bomp_state) {
        return g_bomp_state->behaviour_nested;
    }
    return 0;
}
