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

// ------------------------------------------------------------------------
// Utility declarations

inline static uintptr_t paging_round_up(uintptr_t address, uintptr_t size)
{
    return (address + size - 1) & ~(size - 1);
}

inline static int aligned(uintptr_t address, uintptr_t bytes)
{
    return (address & (bytes - 1)) == 0;
}

static void
paging_write_l0_entry(union armv8_ttable_entry *l0_table, lvaddr_t va, union armv8_ttable_entry l0)
{
    assert(l0_table);
//    union armv8_l0_entry *l0_table;
//    if (ttbase == 0) {
//        if(va < MEMORY_OFFSET)
//            ttbase = sysreg_read_ttbr0_el1() + MEMORY_OFFSET;
//        else
//            ttbase = sysreg_read_ttbr1_el1() + MEMORY_OFFSET;
//    }
//    l0_table = (union armv8_l0_entry *) ttbase;

    l0_table[VMSAv8_64_L0_BASE(va)] = l0;
}

static void
paging_write_l1_entry(union armv8_ttable_entry *l1_table, lvaddr_t va, union armv8_ttable_entry l1)
{
    assert(l1_table);
//    union armv8_l1_entry *l1_table;
//    if (ttbase == 0) {
//        if(va < MEMORY_OFFSET)
//            ttbase = sysreg_read_ttbr0_el1() + MEMORY_OFFSET;
//        else
//            ttbase = sysreg_read_ttbr1_el1() + MEMORY_OFFSET;
//    }
//    l1_table = (union armv8_l1_entry *) ttbase;

    l1_table[VMSAv8_64_L1_BASE(va)] = l1;
}

static void
paging_write_l2_entry(union armv8_ttable_entry *l2_table, lvaddr_t va, union armv8_ttable_entry l2)
{
    assert(l2_table);
    l2_table[VMSAv8_64_L2_BASE(va)] = l2;
}

static void
paging_write_l3_entry(union armv8_ttable_entry *l3_table, lvaddr_t va, union armv8_ttable_entry l3)
{
    assert(l3_table);
    l3_table[VMSAv8_64_L3_BASE(va)] = l3;
}

// ------------------------------------------------------------------------
// Exported functions

void paging_map_kernel_section(union armv8_ttable_entry *ttbase, lvaddr_t va, lpaddr_t pa)
{
    union armv8_ttable_entry l2;

    l2.raw = 0;
    l2.block_l2.valid = 1;
    l2.block_l2.mb0 = 0;
    l2.block_l2.af = 1;
    l2.block_l2.base = pa >> 21u;
    paging_write_l2_entry(ttbase, va, l2);
}

void paging_map_kernel_l1_block(union armv8_ttable_entry *ttbase, lvaddr_t va, lpaddr_t pa)
{
    union armv8_ttable_entry l1;

    l1.raw = 0;
    l1.block_l1.valid = 1;
    l1.block_l1.mb0 = 0;
    l1.block_l1.af = 1;
    l1.block_l1.base = pa >> 30u;
    paging_write_l1_entry(ttbase, va, l1);
}

void paging_map_memory(union armv8_ttable_entry *ttbase, lpaddr_t paddr, size_t bytes)
{
    lpaddr_t pend  = paging_round_up(paddr + bytes, VMSAv8_64_L1_BLOCK_SIZE);
    while (paddr < pend) {
        paging_map_kernel_l1_block(ttbase,
                (paddr + MEMORY_OFFSET) - KERNEL_OFFSET,
                paddr);
        paddr += VMSAv8_64_L1_BLOCK_SIZE;
    }
}

//static lvaddr_t dev_alloc;

lvaddr_t paging_map_device(lpaddr_t device_base, size_t device_bytes)
{
    // TODO: Implement
    if (device_base < KERNEL_OFFSET) {
        return device_base + KERNEL_OFFSET;
    } else {
        return device_base;
    }
}

void paging_map_table_l0(union armv8_ttable_entry *table_base, lvaddr_t va, lpaddr_t pa)
{
    assert(aligned((uintptr_t)table_base, VMSAv8_64_PTABLE_SIZE));
    assert(aligned(pa, VMSAv8_64_PTABLE_SIZE));

    union armv8_ttable_entry e;

    e.raw = 0;
    e.d.valid = 1;
    e.d.mb1 = 1;
    e.d.base = (pa >> BASE_PAGE_BITS);

    paging_write_l0_entry(table_base, va, e);
}

void paging_map_table_l1(union armv8_ttable_entry *table_base, lvaddr_t va, lpaddr_t pa)
{
    assert(aligned((uintptr_t)table_base, VMSAv8_64_PTABLE_SIZE));
    assert(aligned(pa, VMSAv8_64_PTABLE_SIZE));

    union armv8_ttable_entry e;

    e.raw = 0;
    e.d.valid = 1;
    e.d.mb1 = 1;
    e.d.base = (pa >> BASE_PAGE_BITS);

    paging_write_l1_entry(table_base, va, e);
}

void paging_map_block_l1(union armv8_ttable_entry *table_base, lvaddr_t va, lpaddr_t pa, uintptr_t flags)
{
    assert(aligned((uintptr_t)table_base, VMSAv8_64_PTABLE_SIZE));
    assert(aligned(pa, VMSAv8_64_PTABLE_SIZE));

    union armv8_ttable_entry e;

    e.raw = flags;
    e.block_l1.valid = 1;
    e.block_l1.mb0 = 0;
    e.block_l1.af = 1;
    e.block_l1.base = (pa >> BASE_PAGE_BITS);

    paging_write_l1_entry(table_base, va, e);
}


void paging_map_table_l2(union armv8_ttable_entry *table_base, lvaddr_t va, lpaddr_t pa)
{
    assert(aligned((uintptr_t)table_base, VMSAv8_64_PTABLE_SIZE));
    assert(aligned(pa, VMSAv8_64_PTABLE_SIZE));
    assert(pa);

    union armv8_ttable_entry e;

    e.raw = 0;
    e.d.valid = 1;
    e.d.mb1 = 1;
    e.d.base = (pa >> BASE_PAGE_BITS);

//    printf("map table l2@%p->l3@%p\n", table_base + VMSAv8_64_L2_BASE(va), pa);

    paging_write_l2_entry(table_base, va, e);
}

void paging_map_block_l2(union armv8_ttable_entry *table_base, lvaddr_t va, lpaddr_t pa, uintptr_t flags)
{
    assert(aligned((uintptr_t)table_base, VMSAv8_64_PTABLE_SIZE));
    assert(aligned(pa, VMSAv8_64_PTABLE_SIZE));

    union armv8_ttable_entry e;

    e.raw = 0;
    e.block_l2.valid = 1;
    e.block_l2.mb0 = 0;
    e.block_l2.af = 1;
    e.block_l2.base = (pa >> BASE_PAGE_BITS);

    paging_write_l2_entry(table_base, va, e);
}


void paging_map_page_l3(union armv8_ttable_entry *table_base, lvaddr_t va, lpaddr_t pa, uintptr_t flags)
{
    assert(aligned((uintptr_t)table_base, VMSAv8_64_PTABLE_SIZE));
    assert(aligned(pa, VMSAv8_64_PTABLE_SIZE));

    assert(0 == (flags & 0xfffff000));
    assert(0 == (flags & 0x3));
    assert(0 == (pa & 0xfff));

    union armv8_ttable_entry e;
    e.raw = flags;

    e.page.valid = 1;
    e.page.mb1 = 1;
    e.page.af= 1;
    e.page.base = (pa >> BASE_PAGE_BITS);

    paging_write_l3_entry(table_base, va, e);
}

void paging_set_l3_entry(union armv8_ttable_entry *l3_entry, lpaddr_t pa, uintptr_t flags)
{
    assert(0 == (flags & 0xfffff000));
    assert(0 == (flags & 0x3));
    assert(0 == (pa & 0xfff));


    union armv8_ttable_entry e;
    e.raw = flags;

    e.page.valid = 1;
    e.page.mb1 = 1;
    e.page.af= 1;
    e.page.base = (pa >> BASE_PAGE_BITS);

//    printf("Setting l3 entry@%p -> (%p) %p raw: %p\n", l3_entry, e.page.base, e.page.base << BASE_PAGE_BITS, e.raw);

    *l3_entry = e;
}


//void paging_set_l2_entry(union armv8_ttable_entry *l2e, lpaddr_t addr, uintptr_t flags)
//{
//    assert(0 == (flags & 0xfffff000));
//    assert(0 == (flags & 0x3));
//    assert(0 == (addr & 0xfff));
//
//    union armv8_ttable_entry e;
//    e.raw = flags;
//
//    e.d.valid = 1;
//    e.d.mb1 = 1;
//    e.d.base = (addr >> 12);
//
//    *l2e = e;
//}
//
//void paging_set_l3_entry(union armv8_ttable_entry *l3e, lpaddr_t addr, uintptr_t flags)
//{
//    assert(0 == (flags & 0xfffff000));
//    assert(0 == (flags & 0x3));
//    assert(0 == (addr & 0xfff));
//
//    union armv8_ttable_entry e;
//    e.raw = flags;
//
//    e.page.valid = 1;
//    e.page.mb1 = 1;
//    e.page.af         = 1;
//    e.page.base = (addr >> 12);
//
//    *l3e = e;
//}
