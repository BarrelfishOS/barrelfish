/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <cp15.h>
#include <paging_kernel_arch.h>
#include <string.h>
#include <exceptions.h>
#include <arm_hal.h>

/**
 * Kernel L1 page table
 */
//XXX: We reserve double the space needed to be able to align the pagetable
//	   to 16K after relocation
static union arm_l1_entry kernel_l1_table[2*ARM_L1_MAX_ENTRIES]
__attribute__((aligned(ARM_L1_ALIGN)));
static union arm_l1_entry *aligned_kernel_l1_table;

#if 0
/**
 * Kernel L2 page table for first MB
 */
//XXX: We reserve double the space needed to be able to align the pagetable
//	   to 1K after relocation
static union arm_l2_entry low_l2_table[2*ARM_L2_MAX_ENTRIES]
__attribute__((aligned(ARM_L2_ALIGN)));
static union arm_l2_entry *aligned_low_l2_table;
#endif // 0

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
paging_write_l1_entry(uintptr_t ttbase, lvaddr_t va, union arm_l1_entry l1)
{
    union arm_l1_entry *l1_table;
    if (ttbase == 0) {
        if(va < MEMORY_OFFSET)
        	ttbase = cp15_read_ttbr0() + MEMORY_OFFSET;
        else
        	ttbase = cp15_read_ttbr1() + MEMORY_OFFSET;
    }
    l1_table = (union arm_l1_entry *) ttbase;
    l1_table[ARM_L1_OFFSET(va)] = l1;
}
// ------------------------------------------------------------------------
// Exported functions


void paging_map_kernel_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa)
{

    union arm_l1_entry l1;

    l1.raw = 0;
    l1.section.type = L1_TYPE_SECTION_ENTRY;
    l1.section.bufferable   = 1;
    l1.section.cacheable    = 1;
    l1.section.ap10         = 1;	// RW/NA
    l1.section.ap2          = 0;
    l1.section.base_address = pa >> 20u;

    paging_write_l1_entry(ttbase, va, l1);
}

void paging_map_memory(uintptr_t ttbase, lpaddr_t paddr, size_t bytes)
{
    lpaddr_t pend  = paging_round_down(paddr + bytes, BYTES_PER_SECTION);
    while (paddr < pend) {
        paging_map_kernel_section(ttbase, paddr, paddr);
        paddr += BYTES_PER_SECTION;
    }
}

void
paging_map_device_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa);

/**
 * \brief Reset kernel paging.
 *
 * This function resets the page maps for kernel and memory-space. It clears out
 * all other mappings. Use this only at system bootup!
 */
void paging_arm_reset(lpaddr_t paddr, size_t bytes)
{
    // make sure kernel pagetable is aligned to 16K after relocation
    aligned_kernel_l1_table = (union arm_l1_entry *)ROUND_UP(
            (uintptr_t)kernel_l1_table, ARM_L1_ALIGN);

    // Re-map physical memory
    //
    paging_map_memory((uintptr_t)aligned_kernel_l1_table , paddr, bytes);

    //map high-mem relocated exception vector to kernel section
    paging_map_device_section((uintptr_t)aligned_kernel_l1_table, ETABLE_ADDR,
                              PHYS_MEMORY_START);

    cp15_write_ttbr1(mem_to_local_phys((uintptr_t)aligned_kernel_l1_table));
    /* cp15_invalidate_tlb(); */
}

void
paging_map_device_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa)
{
    union arm_l1_entry l1;

    l1.raw = 0;
    l1.section.type = L1_TYPE_SECTION_ENTRY;
    l1.section.bufferable   = 0;
    l1.section.cacheable    = 0;
    l1.section.ap10         = 3; // prev value: 3 // RW/NA RW/RW
    l1.section.ap2	    = 0;
    l1.section.base_address = pa >> 20u;

    paging_write_l1_entry(ttbase, va, l1);
}

lvaddr_t paging_map_device(lpaddr_t device_base, size_t device_bytes)
{
    // HACK to put device in high memory.
    // Should likely track these allocations.
    static lvaddr_t dev_alloc = DEVICE_OFFSET;
    assert(device_bytes <= BYTES_PER_SECTION);
    dev_alloc -= BYTES_PER_SECTION;

    printf("paging_map_device_section: 0x%"PRIxLVADDR", 0x%"PRIxLVADDR", "
            "0x%"PRIxLPADDR".\n",
            (uintptr_t)aligned_kernel_l1_table, dev_alloc, device_base);

    paging_map_device_section((uintptr_t)aligned_kernel_l1_table, dev_alloc,
            device_base);

    return dev_alloc;
}


#if 0
/**
 * \brief Reset kernel paging.
 *
 * This function resets the page maps for kernel and memory-space. It clears out
 * all other mappings. Use this only at system bootup!
 */
void paging_arm_reset(lpaddr_t paddr, size_t bytes)
{
    // make sure kernel pagetable is aligned to 16K after relocation
    aligned_kernel_l1_table = (union arm_l1_entry *)ROUND_UP(
            (uintptr_t)aligned_kernel_l1_table, ARM_L1_ALIGN);

    // make sure low l2 pagetable is aligned to 1K after relocation
    aligned_low_l2_table = (union arm_l2_entry *)ROUND_UP(
            (uintptr_t)low_l2_table, ARM_L2_ALIGN);

    // Re-map physical memory
    paging_map_memory((uintptr_t)aligned_kernel_l1_table, paddr, bytes);

    // map first MB at granularity of 4K pages
    uint32_t l2_flags = ARM_L2_SMALL_USR_NONE | ARM_L2_SMALL_CACHEABLE |
        ARM_L2_SMALL_BUFFERABLE;
    paging_map_user_pages_l1((uintptr_t)aligned_kernel_l1_table,
            MEMORY_OFFSET, mem_to_local_phys((uintptr_t)aligned_low_l2_table));
    for(lpaddr_t pa=0; pa < ARM_L1_SECTION_BYTES; pa += BYTES_PER_PAGE)
    {
        lvaddr_t va = pa + MEMORY_OFFSET;
        paging_set_l2_entry((uintptr_t *)&aligned_low_l2_table[ARM_L2_OFFSET(va)], pa, l2_flags);
    }

    // map high-mem relocated exception vector to corresponding page in low MB
    // core 0: 0xffff0000 -> 0x80000
    // core 1: 0xffff0000 -> 0x81000
    // ...
    paging_map_user_pages_l1((uintptr_t)aligned_kernel_l1_table, ETABLE_ADDR,
            mem_to_local_phys((uintptr_t)aligned_low_l2_table));
    int core_id = hal_get_cpu_id();
    lpaddr_t addr = ETABLE_PHYS_BASE + core_id * BASE_PAGE_SIZE;
    paging_set_l2_entry((uintptr_t *)&aligned_low_l2_table[ARM_L2_OFFSET(ETABLE_ADDR)], addr, l2_flags);


    //map section containing sysflag registers 1:1
    paging_map_device_section((uintptr_t)aligned_kernel_l1_table, sysflagset_base, sysflagset_base);

    cp15_write_ttbr1(mem_to_local_phys((uintptr_t)aligned_kernel_l1_table));

    cp15_invalidate_tlb();
}
#endif // 0

void paging_make_good(lvaddr_t new_table_base, size_t new_table_bytes)
{
    assert(new_table_base >= MEMORY_OFFSET);
    assert(new_table_bytes == ARM_L1_ALIGN);
    assert(aligned(new_table_base, ARM_L1_ALIGN));

    lvaddr_t ttbr = local_phys_to_mem(cp15_read_ttbr0());
    size_t st = (MEMORY_OFFSET / ARM_L1_SECTION_BYTES) * ARM_L1_BYTES_PER_ENTRY;

    // Copy kernel pages (everything from MEMORY_OFFSET upwards)
    memcpy((void*)new_table_base + st, (void*)ttbr + st,
           ARM_L1_MAX_ENTRIES * ARM_L1_BYTES_PER_ENTRY - st);
}

void paging_map_user_pages_l1(lvaddr_t table_base, lvaddr_t va, lpaddr_t pa)
{
    assert(aligned(table_base, ARM_L1_ALIGN));
    assert(aligned(pa, BYTES_PER_SMALL_PAGE));

    union arm_l1_entry e;

    e.raw                 = 0;
    e.page_table.type         = L1_TYPE_PAGE_TABLE_ENTRY;
    e.page_table.domain       = 0;
    e.page_table.base_address = (pa >> 10);

    paging_write_l1_entry(table_base, va, e);
}

void paging_set_l2_entry(uintptr_t* l2e, lpaddr_t addr, uintptr_t flags)
{
    assert(0 == (flags & 0xfffff000));
    assert(0 == (flags & 0x3));
    assert(0 == (addr & 0xfff));

    union arm_l2_entry e;
    e.raw = flags;

    e.small_page.type = L2_TYPE_SMALL_PAGE;
    e.small_page.base_address = (addr >> 12);

    *l2e = e.raw;
}

void paging_context_switch(lpaddr_t ttbr)
{
    assert(ttbr < MEMORY_OFFSET);
    //assert((ttbr & 0x3fff) == 0);

    lpaddr_t old_ttbr = cp15_read_ttbr0();
    if (ttbr != old_ttbr)
    {
        cp15_write_ttbr0(ttbr);
        cp15_invalidate_tlb();
        //this isn't necessary on gem5, since gem5 doesn't implement the cache
        //maintenance instructions, but ensures coherency by itself
        //cp15_invalidate_i_and_d_caches();
    }
}

static errval_t
caps_map_l1(struct capability* dest,
            cslot_t            slot,
            struct capability* src,
            uintptr_t          kpi_paging_flags,
            uintptr_t          offset)
{
    //
    // Note:
    //
    // We have chicken-and-egg problem in initializing resources so
    // instead of treating an L2 table it's actual 1K size, we treat
    // it as being 4K. As a result when we map an "L2" table we actually
    // map a page of memory as if it is 4 consecutive L2 tables.
    //
    // See lib/barrelfish/arch/arm/pmap_arch.c for more discussion.
    //
    const int ARM_L1_SCALE = 4;

    if (slot >= 1024) {
        panic("oops");
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (src->type != ObjType_VNode_ARM_l2) {
        panic("oops");
        return SYS_ERR_WRONG_MAPPING;
    }

    if (slot >= ARM_L1_OFFSET(MEMORY_OFFSET) / ARM_L1_SCALE) {
        panic("oops");
        return SYS_ERR_VNODE_SLOT_RESERVED;
    }

    // Destination
    lpaddr_t dest_lpaddr = gen_phys_to_local_phys(dest->u.vnode_arm_l1.base);
    lvaddr_t dest_lvaddr = local_phys_to_mem(dest_lpaddr);

    union arm_l1_entry* entry = (union arm_l1_entry*)dest_lvaddr + (slot * ARM_L1_SCALE);

    // Source
    genpaddr_t src_gpaddr = src->u.vnode_arm_l2.base;
    lpaddr_t   src_lpaddr = gen_phys_to_local_phys(src_gpaddr);

    assert(offset == 0);
    assert(aligned(src_lpaddr, 1u << 10));
    assert((src_lpaddr < dest_lpaddr) || (src_lpaddr >= dest_lpaddr + 16384));

    for (int i = 0; i < 4; i++, entry++)
    {
        entry->raw = 0;
        entry->page_table.type   = L1_TYPE_PAGE_TABLE_ENTRY;
        entry->page_table.domain = 0;
        entry->page_table.base_address =
            (src_lpaddr + i * BASE_PAGE_SIZE / ARM_L1_SCALE) >> 10;
        debug(SUBSYS_PAGING, "L1 mapping %"PRIuCSLOT". @%p = %08"PRIx32"\n",
              slot * ARM_L1_SCALE + i, entry, entry->raw);
    }

    cp15_invalidate_tlb();

    return SYS_ERR_OK;
}

static errval_t
caps_map_l2(struct capability* dest,
            cslot_t            slot,
            struct capability* src,
            uintptr_t          kpi_paging_flags,
            uintptr_t          offset)
{
    assert(0 == (kpi_paging_flags & ~KPI_PAGING_FLAGS_MASK));

    // ARM L2 has 256 entries, but we treat a 4K page as a consecutive
    // region of L2 with a single index. 4K == 4 * 1K
    if (slot >= (256 * 4)) {
        panic("oops");
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (src->type != ObjType_Frame && src->type != ObjType_DevFrame) {
        panic("oops");
        return SYS_ERR_WRONG_MAPPING;
    }

    // check offset within frame
    if ((offset + BYTES_PER_PAGE > ((genpaddr_t)1 << src->u.frame.bits)) ||
        ((offset % BYTES_PER_PAGE) != 0)) {
        panic("oops");
        return SYS_ERR_FRAME_OFFSET_INVALID;
    }

    // Destination
    lvaddr_t dest_lvaddr =
        local_phys_to_mem(gen_phys_to_local_phys(dest->u.vnode_arm_l2.base));

    union arm_l2_entry* entry = (union arm_l2_entry*)dest_lvaddr + slot;
    if (entry->small_page.type != L2_TYPE_INVALID_PAGE) {
        panic("Remapping valid page.");
    }

    lpaddr_t src_lpaddr = gen_phys_to_local_phys(src->u.frame.base + offset);
    if ((src_lpaddr & (BASE_PAGE_SIZE - 1))) {
        panic("Invalid target");
    }

    entry->raw = 0;

    entry->small_page.type = L2_TYPE_SMALL_PAGE;
    entry->small_page.bufferable = 1;
    entry->small_page.cacheable =
        (kpi_paging_flags & KPI_PAGING_FLAGS_NOCACHE) ? 0 : 1;

    entry->small_page.ap10  =
        (kpi_paging_flags & KPI_PAGING_FLAGS_READ)  ? 2 : 0;
    entry->small_page.ap10 |=
        (kpi_paging_flags & KPI_PAGING_FLAGS_WRITE) ? 3 : 0;
    entry->small_page.ap2 = 0;


    entry->small_page.base_address = src_lpaddr >> 12;

    debug(SUBSYS_PAGING, "L2 mapping %08"PRIxLVADDR"[%"PRIuCSLOT"] @%p = %08"PRIx32"\n",
           dest_lvaddr, slot, entry, entry->raw);

    // Flush TLB if remapping.
    cp15_invalidate_tlb();

    return SYS_ERR_OK;
}

/// Create page mappings
errval_t caps_copy_to_vnode(struct cte *dest_vnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, uintptr_t param1,
                            uintptr_t param2)
{
    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_vnode_cte->cap;

    if (ObjType_VNode_ARM_l1 == dest_cap->type) {
        return caps_map_l1(dest_cap, dest_slot, src_cap,
                           param1,      // kpi_paging_flags
                           param2       // offset
                          );
    }
    else if (ObjType_VNode_ARM_l2 == dest_cap->type) {
        return caps_map_l2(dest_cap, dest_slot, src_cap,
                           param1,      // kpi_paging_flags
                           param2       // offset
                          );
    }
    else {
        panic("ObjType not VNode");
    }
}

errval_t page_mappings_unmap(struct capability *pgtable, size_t slot)
{
    assert(type_is_vnode(pgtable->type));

    switch(pgtable->type){
    case ObjType_VNode_ARM_l1: {
    	genpaddr_t gp = pgtable->u.vnode_arm_l1.base;
        lpaddr_t   lp = gen_phys_to_local_phys(gp);
        lvaddr_t   lv = local_phys_to_mem(lp);
        union arm_l1_entry *entry = (union arm_l1_entry *)lv + slot;
        entry->raw = 0;
        break;
    }
    case ObjType_VNode_ARM_l2: {
    	genpaddr_t gp = pgtable->u.vnode_arm_l2.base;
        lpaddr_t   lp = gen_phys_to_local_phys(gp);
        lvaddr_t   lv = local_phys_to_mem(lp);
        union arm_l2_entry *entry = (union arm_l2_entry *)lv + slot;
        entry->raw = 0;
        break;
    }
    default:
        assert(!"Should not get here");
    }

    cp15_invalidate_tlb();

    return SYS_ERR_OK;
}
