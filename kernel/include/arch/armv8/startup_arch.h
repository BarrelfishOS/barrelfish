/**
 * \file
 * \brief Startup prototypes.
 */

/*
 * Copyright (c) 2007-2010, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _STARTUP_ARCH_H_
#define _STARTUP_ARCH_H_

#include <startup.h>
#include <offsets.h>
#include <barrelfish_kpi/arm_core_data.h>

#define INIT_L1_ENTRIES 4
#define INIT_L1_BYTES (INIT_L1_ENTRIES * PTABLE_ENTRY_SIZE)
#define INIT_L2_BYTES (INIT_L1_ENTRIES * PTABLE_NUM_ENTRIES * \
                       PTABLE_ENTRY_SIZE)
#define INIT_L3_BYTES (INIT_L1_ENTRIES * PTABLE_NUM_ENTRIES * \
                       PTABLE_NUM_ENTRIES * PTABLE_ENTRY_SIZE)

#define INIT_L2_PAGES ((INIT_SPACE_LIMIT - INIT_VBASE) / BASE_PAGE_SIZE)

#define INIT_BOOTINFO_VBASE   0x200000
#define INIT_ARGS_VBASE       (INIT_BOOTINFO_VBASE + BOOTINFO_SIZE)
#define INIT_DISPATCHER_VBASE (INIT_ARGS_VBASE + ARGS_SIZE)
#define MON_URPC_VBASE        (INIT_DISPATCHER_VBASE + DISPATCHER_SIZE)

#define INIT_PERM_RO          (AARCH64_L3_CACHEABLE  | \
                               AARCH64_L3_BUFFERABLE | \
                               AARCH64_L3_USR_RO)

#define INIT_PERM_RW          (AARCH64_L3_CACHEABLE  | \
                               AARCH64_L3_BUFFERABLE | \
                               AARCH64_L3_USR_RW)
/*
 * Resolves to required number of entries in L1 to map 'limit' number of
 * bytes.
 */
#define AARCH64_L1_ENTRIES(limit) (ARMv8_L1_OFFSET((limit) - 1) + 1)

/*
 * Resolves to required number of entries in L2 to map 'limit' number of
 * bytes.
 */
#define AARCH64_L2_ENTRIES(limit) (ARMv8_L2_OFFSET((limit) - 1) + 1)

#define INIT_L2_SIZE          AARCH64_L1_ENTRIES(INIT_SPACE_LIMIT)
#define INIT_L3_SIZE          AARCH64_L2_ENTRIES(INIT_SPACE_LIMIT)

// Well known address for glbl_core_data @64Kb
#define GLBL_COREDATA_BASE_PHYS         (0x10000)

void create_module_caps(struct spawn_state *st);

struct dcb *spawn_bsp_init(const char *name, alloc_phys_func alloc_phys);

struct dcb *spawn_app_init(struct arm_core_data *core_data,
                           const char *name, alloc_phys_func alloc_phys);

extern struct arm_core_data *glbl_core_data;

#endif
