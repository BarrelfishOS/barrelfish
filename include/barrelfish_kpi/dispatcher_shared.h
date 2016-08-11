/**
 * \file
 * \brief Generic dispatcher struct shared between kernel and user
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_KPI_DISPATCHER_SHARED_H
#define BARRELFISH_KPI_DISPATCHER_SHARED_H

#ifndef __ASSEMBLER__

#include <barrelfish_kpi/dispatcher_handle.h>

/**
 * \brief Amount of space required for the dispatcher frame.
 * This should be at least greater than the memory required for
 * struct disp_priv.
 * Keeping it as a power of 2 allows using a single cap for dispatcher frame
 *
 * \bug Make this arch specific
 */
#define DISPATCHER_FRAME_BITS 18
#define DISP_NAME_LEN   16

enum task_type {
    TASK_TYPE_BEST_EFFORT,
    TASK_TYPE_SOFT_REALTIME,
    TASK_TYPE_HARD_REALTIME
};

///< Architecture generic kernel/user shared dispatcher struct
struct dispatcher_shared_generic {
    uint32_t   disabled;                        ///< Disabled flag (Must be able to change atomically)
    uint32_t   haswork;                         ///< Has work (ie. is runnable) (Must be able to change atomically)

    lvaddr_t    udisp;                          ///< User-mode pointer to dispatcher
    uint32_t    lmp_delivered, lmp_seen;        ///< # LMP words delivered and seen
    lvaddr_t    lmp_hint;                       ///< Hint for location of LMP
    lvaddr_t    dispatcher_run;                 ///< Run entry
    lvaddr_t    dispatcher_lrpc;                ///< LRPC entry
    lvaddr_t    dispatcher_pagefault;           ///< Pagefault entry
    lvaddr_t    dispatcher_pagefault_disabled;  ///< Disabled pagefault entry
    lvaddr_t    dispatcher_trap;                ///< Trap entry

    systime_t   systime;                        ///< System time when last dispatched/resumed (W/O to kernel)
    systime_t   wakeup;                         ///< System time at which to wake dispatcher from sleep (R/O by kernel, on yield)

    char        name[DISP_NAME_LEN];            ///< Name of domain, for debugging purposes
    uint32_t    fpu_used;                       ///< Was FPU used while disabled?
    uint32_t    fpu_trap;                       ///< State of FPU trap

    coreid_t    curr_core_id;                   ///< Core id of current core, in this part so kernel can update
#ifdef __k1om__
    uint8_t     xeon_phi_id;
#endif
};

static inline struct dispatcher_shared_generic*
get_dispatcher_shared_generic(dispatcher_handle_t handle)
{
    return (struct dispatcher_shared_generic*)handle;
}

static inline lvaddr_t get_dispatcher_vaddr(dispatcher_handle_t handle)
{
    return (lvaddr_t)handle;
}

#include <stdio.h>
static inline void dump_dispatcher(struct dispatcher_shared_generic *disp)
{
    printf("Dump of dispatcher at address %p:\n", disp);
    printf("  disabled      = %d (%s)\n", disp->disabled, disp->disabled ? "RESUME" : "UPCALL" );
    printf("  haswork       = %d\n", disp->haswork );
    printf("  udisp         = 0x%"PRIxLVADDR"\n", disp->udisp );
    printf("  lmp_delivered = %d\n", disp->lmp_delivered );
    printf("  lmp_seen      = %d\n", disp->lmp_seen );
    printf("  lpm_hint      = 0x%"PRIxLVADDR"\n", disp->lmp_hint );
    printf("  dispatcher_run                = 0x%"PRIxLVADDR"\n", disp->dispatcher_run );
    printf("  dispatcher_pagefault          = 0x%"PRIxLVADDR"\n", disp->dispatcher_pagefault );
    printf("  dispatcher_pagefault_disabled = 0x%"PRIxLVADDR"\n", disp->dispatcher_pagefault_disabled );
    printf("  dispatcher_trap               = 0x%"PRIxLVADDR"\n", disp->dispatcher_trap );
    printf("  systime      = 0x%" PRIuSYSTIME "\n", disp->systime );
    printf("  wakeup       = 0x%" PRIuSYSTIME "\n", disp->wakeup );
    printf("  name         = %.*s\n", DISP_NAME_LEN, disp->name );
    printf("  fpu_used     = %d\n", disp->fpu_used );
    printf("  fpu_trap     = %d\n", disp->fpu_trap );
    printf("  curr_core_id = 0x%" PRIxCOREID "\n", disp->curr_core_id );
}


#endif //__ASSEMBLER__
#endif // BARRELFISH_KPI_DISPATCHER_SHARED_H
