/**
 * \file
 * \brief ARMv8 kernel page-table structures.
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARCH_ARMv8_PAGING_H
#define KERNEL_ARCH_ARMv8_PAGING_H

// XXX: Not sure if these includes are required
#include <capabilities.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/paging_arch.h>
#include <sysreg.h>

/**
 * A translation table entry for VMSAv8-64 stage 1.
 */
union armv8_ttable_entry {
    uint64_t raw;
    struct {
        uint64_t        valid       :1;
        uint64_t        mb1         :1;         // 1 -> table, 0 -> block
        uint64_t        ignored1    :10;        // lower block attrs, ignored for table
        uint64_t        base        :36;        // base address of next level table
        uint64_t        reserved1   :4;
        uint64_t        ignored2    :7;
        uint64_t        pxntable    :1;         // only stage 1, executable from EL1
        uint64_t        uxntable    :1;         // only stage 1, executable from EL0
        uint64_t        aptable     :2;         // only stage 1, access from EL0
        uint64_t        nstable     :1;         // only stage 1, access from secure state
    } d;
    struct {
        uint64_t        valid       :1;
        uint64_t        mb0         :1;         // 1 -> table, 0 -> block
        uint64_t        attrindex   :3;         // mem attr index field, D4-1798
        uint64_t        ns          :1;         // non-secure bit
        uint64_t        ap          :2;         // access permissions bits
        uint64_t        sh          :2;         // shareability field
        uint64_t        af          :1;         // accessed flag
        uint64_t        ng          :1;         // not global bit
        uint64_t        reserved1   :18;
        uint64_t        base        :18;
        uint64_t        reserved2   :4;
        uint64_t        contiguous  :1;         // hint that entry is part of set
                                                // of contiguous entries, D4-1811
        uint64_t        pxn         :1;         // privileged execute never bit
        uint64_t        uxn         :1;         // (user) execute never bit
        uint64_t        avail1      :4;         // available for SW use
        uint64_t        ignored1    :5;
    } block_l1;
    struct {
        uint64_t        valid       :1;
        uint64_t        mb0         :1;         // 1 -> table, 0 -> block
        uint64_t        attrindex   :3;         // mem attr index field, D4-1798
        uint64_t        ns          :1;         // non-secure bit
        uint64_t        ap          :2;         // access permissions bits
        uint64_t        sh          :2;         // shareability field
        uint64_t        af          :1;         // accessed flag
        uint64_t        ng          :1;         // not global bit
        uint64_t        reserved1   :9;
        uint64_t        base        :27;
        uint64_t        reserved2   :4;
        uint64_t        contiguous  :1;         // hint that entry is part of set
                                                // of contiguous entries, D4-1811
        uint64_t        pxn         :1;         // privileged execute never bit
        uint64_t        uxn         :1;         // (user) execute never bit
        uint64_t        avail1      :4;         // available for SW use
        uint64_t        ignored1    :5;
    } block_l2;
    struct {
        uint64_t        valid       :1;
        uint64_t        mb1         :1;         // 0 -> makes entry invalid
        uint64_t        attrindex   :3;         // mem attr index field, D4-1798
        uint64_t        ns          :1;         // non-secure bit
        uint64_t        ap          :2;         // access permissions bits
        uint64_t        sh          :2;         // shareability field
        uint64_t        af          :1;         // accessed flag
        uint64_t        ng          :1;         // not global bit
        uint64_t        base        :36;
        uint64_t        reserved1   :4;
        uint64_t        contiguous  :1;         // hint that entry is part of set
                                                // of contiguous entries, D4-1811
        uint64_t        pxn         :1;         // privileged execute never bit
        uint64_t        uxn         :1;         // (user) execute never bit
        uint64_t        avail1      :4;         // available for SW use
        uint64_t        ignored1    :5;
    } page;
};

STATIC_ASSERT_SIZEOF(union armv8_ttable_entry, sizeof(uint64_t));



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


/**
 * Maps a device to a l2 page.
 * Assumption: corresponding L1 entry already set
 *
 */

void paging_map_device_page(uintptr_t l1_table,
					   	    lvaddr_t device_vbase,
					   	    lpaddr_t device_pbase,
					   	    size_t device_bytes);

/**
 * Add kernel mappings to newly constructed page table.
 *
 * @param new_table_addr  address of newly constructed page table.
 * @param new_table_bytes size of newly constructed page table.
 */
void paging_make_good(lvaddr_t new_table_addr, size_t new_table_bytes);

void paging_map_user_pages_l1(lvaddr_t table_addr, lvaddr_t vaddr, lpaddr_t paddr);

void paging_set_l2_entry(uintptr_t* l2entry, lpaddr_t paddr, uintptr_t flags);

void paging_context_switch(lpaddr_t table_addr);

void paging_arm_reset(lpaddr_t paddr, size_t bytes);


// REVIEW: [2010-05-04 orion]
// these were deprecated in churn, enabling now to get system running again.

void paging_map_kernel_section(uintptr_t ttbase,lvaddr_t vbase, lpaddr_t pbase);
void paging_map_memory(uintptr_t ttbase, lpaddr_t paddr, size_t bytes);

static inline bool is_root_pt(enum objtype type) {
    return type == ObjType_VNode_ARM_l1;
}

static inline size_t get_pte_size(void) {
    // both l1_entry and l2_entry are 4 bytes
    return 4;
}

static inline void do_one_tlb_flush(genvaddr_t vaddr)
{
    // TODO: figure out selective flushing for ARM
    sysreg_invalidate_tlb();
}

static inline void do_selective_tlb_flush(genvaddr_t vaddr, genvaddr_t vend)
{
    // TODO: figure out selective flushing for ARM
    sysreg_invalidate_tlb();
}

static inline void do_full_tlb_flush(void)
{
    sysreg_invalidate_tlb();
}


#endif // KERNEL_ARCH_ARMv8_PAGING_H
