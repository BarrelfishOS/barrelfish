/**
 * \file
 * \brief Architecture specific dispatcher struct shared between kernel and user
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_BEEHIVE_BARRELFISH_KPI_DISPATCHER_SHARED_H
#define TARGET_BEEHIVE_BARRELFISH_KPI_DISPATCHER_SHARED_H

#include <barrelfish_kpi/dispatcher_shared.h>

///< Architecture specific kernel/user shared dispatcher struct
struct dispatcher_shared_beehive {
    struct dispatcher_shared_generic d; ///< Generic portion

    union registers_beehive enabled_save_area;  ///< Enabled register save area
    union registers_beehive disabled_save_area; ///< Disabled register save area
    union registers_beehive trap_save_area;     ///< Trap register save area
};

static inline struct dispatcher_shared_beehive*
get_dispatcher_shared_beehive(dispatcher_handle_t handle)
{
    return (struct dispatcher_shared_beehive*)handle;
}

static inline union registers_beehive*
dispatcher_beehive_get_enabled_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_beehive *disp =
        get_dispatcher_shared_beehive(handle);
    return &disp->enabled_save_area;
}

static inline union registers_beehive*
dispatcher_beehive_get_disabled_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_beehive *disp =
        get_dispatcher_shared_beehive(handle);
    return &disp->disabled_save_area;
}

static inline union registers_beehive*
dispatcher_beehive_get_trap_save_area(dispatcher_handle_t handle)
{
    struct dispatcher_shared_beehive *disp =
        get_dispatcher_shared_beehive(handle);
    return &disp->trap_save_area;
}

#endif // TARGET_BEEHIVE_BARRELFISH_KPI_DISPATCHER_SHARED_H
