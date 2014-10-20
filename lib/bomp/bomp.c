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

struct bomp_state *g_bomp_state;

int bomp_switch_backend(bomp_backend_t backend)
{
    if (g_bomp_state && (g_bomp_state->backend_type == backend)) {
        return SYS_ERR_OK;
    }

    struct bomp_state *prev = g_bomp_state;

    switch (backend) {
        case BOMP_BACKEND_BOMP:
            g_bomp_state = bomp_get_backend_state_bomp();
            break;
        case BOMP_BACKEND_XOMP:
            g_bomp_state = bomp_get_backend_state_xomp();
            break;
        case BOMP_BACKEND_LINUX:
            g_bomp_state = bomp_get_backend_state_linux();
            break;
        default:
            /* TODO: error code INVAID BACKEND */
            return -1;
            break;
    }

    if (g_bomp_state == NULL) {
        g_bomp_state = prev;
        return -1;
    }

    return SYS_ERR_OK;
}

#include <icv.h>
void bomp_common_init(struct bomp_state *st)
{
    bomp_mutex_init(&st->critical_lock);
    bomp_lock_init(&st->atomic_lock);
    st->nested = 0;
    st->behaviour_nested = 0;
    st->behaviour_dynamic = 0;
    st->bomp_threads = 1;
    bomp_icv_init_from_env(&st->icv_task);
    bomp_icv_dev_init_from_env(&st->icv_dev);
}

void bomp_set_tls(void *xdata)
{
    struct bomp_thread_local_data *local;
    struct bomp_work *work_data = (struct bomp_work*)xdata;

    assert(g_bomp_state);

    /* Populate the Thread Local Storage data */
    local = calloc(1, sizeof(struct bomp_thread_local_data));
    assert(local != NULL);
    local->thr = g_bomp_state->backend.get_thread();
    local->work = work_data;
    g_bomp_state->tld[work_data->thread_id] = local;
    g_bomp_state->backend.set_tls(local);
}


