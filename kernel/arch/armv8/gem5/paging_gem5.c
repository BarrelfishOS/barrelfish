/*
 * Copyright (c) 2009-2012,2015, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <sysreg.h>
#include <paging_kernel_arch.h>
#include <string.h>
#include <exceptions.h>
#include <arm_hal.h>
#include <cap_predicates.h>
#include <dispatch.h>
#include <startup_arch.h>

/**
 * Kernel L1 page table
 *
 * We reserve double the space needed, so we can to align the pagetables to
 * their size after relocation.
 */
static union armv8_l1_entry kernel_l1_table[2 * PTABLE_NUM_ENTRIES]
    __attribute__((aligned(PTABLE_ENTRY_SIZE)));
static union armv8_l1_entry *aligned_kernel_l1_table;

/**
 * L2 page table for device mappings
 */
static union armv8_l2_entry device_l2_table[2 * PTABLE_NUM_ENTRIES]
    __attribute__((aligned(PTABLE_SIZE)));
static union armv8_l2_entry *aligned_device_l2_table;

// ------------------------------------------------------------------------
// Utility declarations

inline static uintptr_t paging_round_down(uintptr_t address, uintptr_t size)
{
    return address & ~(size - 1);
}

inline static uintptr_t paging_round_up(uintptr_t address, uintptr_t size)
{
    return (address + size - 1) & ~(size - 1);
}

inline static int aligned(uintptr_t address, uintptr_t bytes)
{
    return (address & (bytes - 1)) == 0;
}

static void
paging_write_l1_entry(uintptr_t ttbase, lvaddr_t va, union armv8_l1_entry l1)
{
    union armv8_l1_entry *l1_table;
    if (ttbase == 0) {
        if(va < MEMORY_OFFSET)
            ttbase = sysreg_read_ttbr0_el1() + MEMORY_OFFSET;
        else
            ttbase = sysreg_read_ttbr1_el1() + MEMORY_OFFSET;
    }
    l1_table = (union armv8_l1_entry *) ttbase;

    l1_table[ARMv8_L1_OFFSET(va)] = l1;
}


// ------------------------------------------------------------------------
// Exported functions

void paging_map_kernel_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa)
{

    union armv8_l1_entry l1;

    l1.raw = 0;
    l1.block.type = ARMv8_Ln_BLOCK;
    l1.block.af         = 1;
    l1.block.base_address = pa >> 21u;
    paging_write_l1_entry(ttbase, va, l1);
}

void paging_map_kernel_l1_block(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa)
{

    union armv8_l1_entry l1;

    l1.raw = 0;
    l1.block.type = ARMv8_Ln_BLOCK;
    l1.block.af         = 1;
    l1.block.base_address = pa >> 30u;
    paging_write_l1_entry(ttbase, va, l1);
}

void paging_map_memory(uintptr_t ttbase, lpaddr_t paddr, size_t bytes)
{
    lpaddr_t pend  = paging_round_up(paddr + bytes, HUGE_PAGE_SIZE);
    while (paddr < pend) {
        paging_map_kernel_l1_block(ttbase,
                (paddr + MEMORY_OFFSET) - KERNEL_OFFSET,
                paddr);
        paddr += HUGE_PAGE_SIZE;
    }
}

static void
paging_map_device_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa)
{
    union armv8_l2_entry l2;
    union armv8_l2_entry *l2_table;

    l2.raw = 0;
    l2.block.type = ARMv8_Ln_BLOCK;
    l2.block.af         = 1;
    l2.block.base_address = pa >> 21u;

    l2_table = (union armv8_l2_entry *) ttbase;

    l2_table[ARMv8_L2_OFFSET(va)] = l2;
}

static lvaddr_t dev_alloc;

lvaddr_t paging_map_device(lpaddr_t device_base, size_t device_bytes)
{
    // HACK to put device in high memory.
    // Should likely track these allocations.
    assert(device_bytes <= LARGE_PAGE_SIZE);
    dev_alloc -= LARGE_PAGE_SIZE;
    paging_map_device_section((uintptr_t)aligned_device_l2_table,
                              dev_alloc, device_base);

    return KERNEL_OFFSET + dev_alloc;
}
/**
 * \brief Reset kernel paging.
 *
 * This function resets the page maps for kernel and memory-space. It clears
 * out all other mappings. Use this only at system bootup!
 */
void paging_arm_reset(lpaddr_t paddr, size_t bytes)
{   
    // Make sure kernel pagetables are aligned to 4KB after relocation
    aligned_kernel_l1_table =
        (union armv8_l1_entry *)ROUND_UP((uintptr_t)kernel_l1_table,
                                         (uintptr_t)PTABLE_SIZE);
    aligned_device_l2_table =
        (union armv8_l2_entry *)ROUND_UP((uintptr_t)device_l2_table,
                                         (uintptr_t)PTABLE_SIZE);

    // Map the device-region table
    paging_map_user_pages_l1(
        (uintptr_t)aligned_kernel_l1_table, 0,
        mem_to_local_phys((uintptr_t)aligned_device_l2_table));
    dev_alloc= DEVICE_OFFSET - KERNEL_OFFSET;

    // Re-map physical memory
    paging_map_memory((uintptr_t)aligned_kernel_l1_table, paddr, bytes);

    sysreg_write_ttbr1_el1(
        mem_to_local_phys((uintptr_t)aligned_kernel_l1_table));

    sysreg_invalidate_tlb();
}

void paging_make_good(lpaddr_t base)
{
    union armv8_l1_entry *newl1 =
        (union armv8_l1_entry *)local_phys_to_mem(base);
    int i;

        // XXX: Disabled till vaddr_t is figured out
    debug(SUBSYS_PAGING, "Is now a L1: table = 0x%"PRIxLPADDR"\n", base);

    // Map memory
    for(i = ARMv8_L1_OFFSET(MEMORY_OFFSET); i < PTABLE_NUM_ENTRIES; i++) {
        newl1[i] = aligned_kernel_l1_table[i];
    }
}

/* XXX - This is poorly named. */
void paging_map_user_pages_l1(lvaddr_t table_base, lvaddr_t va, lpaddr_t pa)
{
    assert(aligned(table_base, PTABLE_SIZE));
    assert(aligned(pa, PTABLE_SIZE));

    union armv8_l1_entry e;

    e.raw                     = 0;
    e.page_table.type         = ARMv8_Ln_TABLE;
    e.page_table.base_address = (pa >> 12);

    paging_write_l1_entry(table_base, va, e);
}

void paging_set_l2_entry(uintptr_t* l2e, lpaddr_t addr, uintptr_t flags)
{
    assert(0 == (flags & 0xfffff000));
    assert(0 == (flags & 0x3));
    assert(0 == (addr & 0xfff));

    union armv8_l2_entry e;
    e.raw = flags;

    e.page_table.type = ARMv8_Ln_TABLE;
    e.page_table.base_address = (addr >> 12);

    *l2e = e.raw;
}

void paging_set_l3_entry(uintptr_t* l3e, lpaddr_t addr, uintptr_t flags)
{
    assert(0 == (flags & 0xfffff000));
    assert(0 == (flags & 0x3));
    assert(0 == (addr & 0xfff));

    union armv8_l3_entry e;
    e.raw = flags;

    e.page.type = ARMv8_L3_PAGE;
    e.page.af         = 1;
    e.page.base_address = (addr >> 12);

    *l3e = e.raw;
}

void paging_context_switch(lpaddr_t ttbr)
{
    assert(ttbr < MEMORY_OFFSET);
    //assert((ttbr & 0x3fff) == 0);

    lpaddr_t old_ttbr = sysreg_read_ttbr0_el1();
    if (ttbr != old_ttbr)
    {
        sysreg_write_ttbr0_el1(ttbr);
        sysreg_invalidate_tlb();
        //this isn't necessary on gem5, since gem5 doesn't implement the cache
        //maintenance instructions, but ensures coherency by itself
        //sysreg_invalidate_i_and_d_caches();
    }
}
