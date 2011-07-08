/**
 * \file
 * \brief ARM kernel page-table structures.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARCH_BEEHIVE_PAGING_H
#define KERNEL_ARCH_BEEHIVE_PAGING_H

// XXX: Not sure if these includes are required
#include <capabilities.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/paging_arch.h>

/**
 * Setup bootstrap page table with direct and relocated mappings for kernel.
 *
 * This function does not enable paging.
 *
 * @param initial_base
 * @param initial_size
 */
void paging_map_kernel(uintptr_t initial_base, size_t initial_size);

lvaddr_t paging_map_device(lpaddr_t base, size_t size);

void paging_enable_mmu(void);

void paging_context_switch(lpaddr_t ptbase);

#endif // KERNEL_ARCH_BEEHIVE_PAGING_H
