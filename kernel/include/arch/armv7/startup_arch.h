/**
 * \file
 * \brief Startup prototypes.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _STARTUP_ARCH_H_
#define _STARTUP_ARCH_H_

#include <startup.h>
#include <offsets.h>

#define INIT_L1_BYTES           (ARM_L1_MAX_ENTRIES * ARM_L1_BYTES_PER_ENTRY)

#define INIT_L2_PAGES           ((INIT_SPACE_LIMIT - INIT_VBASE) / BASE_PAGE_SIZE)
#define INIT_L2_BYTES           INIT_L2_PAGES * ARM_L2_BYTES_PER_ENTRY

#define INIT_BOOTINFO_VBASE     0x200000
#define INIT_ARGS_VBASE         (INIT_BOOTINFO_VBASE + BOOTINFO_SIZE)
#define INIT_DISPATCHER_VBASE   (INIT_ARGS_VBASE + ARGS_SIZE)
#define MON_URPC_VBASE          (INIT_DISPATCHER_VBASE + DISPATCHER_SIZE)

#define INIT_PERM_RO            (ARM_L2_SMALL_CACHEABLE  | \
                                 ARM_L2_SMALL_BUFFERABLE | \
                                 ARM_L2_SMALL_USR_RO)

#define INIT_PERM_RW            (ARM_L2_SMALL_CACHEABLE  | \
                                 ARM_L2_SMALL_BUFFERABLE | \
                                 ARM_L2_SMALL_USR_RW)

void create_module_caps(struct spawn_state *st);

struct dcb *spawn_bsp_init(const char *name,
                           alloc_phys_func alloc_phys,
                           alloc_phys_aligned_func alloc_phys_aligned);

struct dcb *spawn_app_init(struct arm_core_data *core_data,
                           const char *name,
                           alloc_phys_func alloc_phys,
                           alloc_phys_aligned_func alloc_phys_aligned);

extern struct arm_core_data *core_data;

/* Initialised by the linker. */
extern struct Elf32_Nhdr build_id_nhdr;

#endif
