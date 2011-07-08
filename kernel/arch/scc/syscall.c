/**
 * \file
 * \brief Arch-specific system calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <syscall.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/paging_arch.h>
#include <arch/x86/syscall.h>
#ifndef RCK_EMU
#       include <rck.h>
#else
#       include <start_aps.h>
#endif

/**
 * \brief Spawn a new core
 */
struct sysret sys_monitor_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                                     genvaddr_t entry)
{
    int r;
    switch(cpu_type) {
    case CPU_SCC:
#ifndef RCK_EMU
        // XXX: We know the core_data frame is 1 page before the entry point.
        r = rck_start_core(core_id, entry, (struct x86_core_data *)(lvaddr_t)
                           ((entry & ~(BASE_PAGE_OFFSET(entry))) - BASE_PAGE_SIZE));
#else
        r = start_aps_x86_32_start(core_id, entry);
#endif
        if (r != 0) {
            return SYSRET(SYS_ERR_CORE_NOT_FOUND);
        }
        break;
    default:
        assert(!"Architecture not supported");
        return SYSRET(SYS_ERR_CORE_NOT_FOUND);
        break;
    }

    return SYSRET(SYS_ERR_OK);
}
