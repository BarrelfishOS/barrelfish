/**
 * \file
 * \brief Architecture specific dispatcher struct shared between kernel and user
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_KPI_DISPATCHER_SHARED_ARCH_H
#define ARCH_AARCH64_BARRELFISH_KPI_DISPATCHER_SHARED_ARCH_H

#include <target/aarch64/barrelfish_kpi/dispatcher_shared_target.h>

/**
 * \brief Returns whether dispatcher is currently disabled, given IP.
 *
 * \param disp  Pointer to dispatcher
 * \param ip    User-level instruction pointer.
 *
 * \return true if dispatcher disabled, false otherwise.
 */
static inline bool dispatcher_is_disabled_ip(dispatcher_handle_t handle,
                                             uintptr_t rip)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    /* one crit_pc pair */
    struct dispatcher_shared_aarch64 *dispaarch64 =
        get_dispatcher_shared_aarch64(handle);
    return disp->disabled ||
        (dispaarch64->crit_pc_low <= rip && rip < dispaarch64->crit_pc_high);
}

static inline arch_registers_state_t*
dispatcher_get_enabled_save_area(dispatcher_handle_t handle)
{
    return &((struct dispatcher_shared_aarch64 *)handle)->enabled_save_area;
}

static inline arch_registers_state_t*
dispatcher_get_disabled_save_area(dispatcher_handle_t handle)
{
    return &((struct dispatcher_shared_aarch64 *)handle)->disabled_save_area;
}

static inline arch_registers_state_t*
dispatcher_get_trap_save_area(dispatcher_handle_t handle)
{
    return &((struct dispatcher_shared_aarch64 *)handle)->trap_save_area;
}

#endif // ARCH_AARCH64_BARRELFISH_KPI_DISPATCHER_SHARED_ARCH_H
