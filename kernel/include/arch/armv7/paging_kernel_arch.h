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

#ifndef KERNEL_ARCH_ARM_PAGING_H
#define KERNEL_ARCH_ARM_PAGING_H

// XXX: Not sure if these includes are required
#include <capabilities.h>
#include <cache.h>
#include <barrelfish_kpi/arm_core_data.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/paging_arch.h>
#include <cp15.h>

void paging_init(lpaddr_t ram_base, size_t ram_size,
                 struct arm_core_data *boot_core_data);

void enable_mmu(lpaddr_t ttbr0, lpaddr_t ttbr1);

void paging_load_pointers(struct arm_core_data *boot_core_data);

void paging_map_vectors(void);

/*
 * Map a device, and return its virtual address 
 *
 * @param base the physical address of the device
 * @param size the size of the device's address range in bytes
 */
extern lvaddr_t paging_map_device(lpaddr_t base, size_t size);

/**
 * \brief Return whether we have enabled the MMU. Useful for
 * initialization assertions
 */
extern bool paging_mmu_enabled(void);

void paging_map_user_pages_l1(lvaddr_t table_addr, lvaddr_t vaddr, lpaddr_t paddr);

void paging_set_l2_entry(uintptr_t* l2entry, lpaddr_t paddr, uintptr_t flags);

void paging_context_switch(lpaddr_t table_addr);

// REVIEW: [2010-05-04 orion]
// these were deprecated in churn, enabling now to get system running again.

void paging_map_memory(uintptr_t ttbase, lpaddr_t paddr, size_t bytes);

static inline bool is_root_pt(enum objtype type) {
    return type == ObjType_VNode_ARM_l1;
}

static inline size_t get_pte_size(void) {
    // both l1_entry and l2_entry are 4 bytes
    return 4;
}
#define PTABLE_ENTRY_SIZE get_pte_size()

static inline void do_one_tlb_flush(genvaddr_t vaddr)
{
    // TODO: figure out selective flushing for ARM
    invalidate_tlb();
}

static inline void do_selective_tlb_flush(genvaddr_t vaddr, genvaddr_t vend)
{
    // TODO: figure out selective flushing for ARM
    invalidate_tlb();
}

static inline void do_full_tlb_flush(void)
{
    invalidate_tlb();
}

#endif // KERNEL_ARCH_ARM_PAGING_H
