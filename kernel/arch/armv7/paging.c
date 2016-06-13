/*
 * Copyright (c) 2009 - 2013, 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <cp15.h>
#include <paging_kernel_arch.h>
#include <string.h>
#include <exceptions.h>
#include <platform.h>
#include <cap_predicates.h>
#include <dispatch.h>
#include <mdb/mdb_tree.h>

static bool mmu_enabled = false;

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


union arm_l2_entry;
static void
paging_set_flags(union arm_l2_entry *entry, uintptr_t kpi_paging_flags)
{
        entry->small_page.bufferable = 1;
        entry->small_page.cacheable =
            (kpi_paging_flags & KPI_PAGING_FLAGS_NOCACHE) ? 0 : 1;
        entry->small_page.ap10  =
            (kpi_paging_flags & KPI_PAGING_FLAGS_READ)  ? 2 : 0;
        entry->small_page.ap10 |=
            (kpi_paging_flags & KPI_PAGING_FLAGS_WRITE) ? 3 : 0;
        entry->small_page.ap2 = 0;
}

static void map_kernel_section_hi(lvaddr_t va, union arm_l1_entry l1);
static void map_kernel_section_lo(lvaddr_t va, union arm_l1_entry l1);
static union arm_l1_entry make_ram_section(lpaddr_t pa);
static union arm_l1_entry make_dev_section(lpaddr_t pa);
static void paging_print_l1_pte(lvaddr_t va, union arm_l1_entry pte);

void paging_print_l1(void);


union arm_l1_entry l1_low [ARM_L1_MAX_ENTRIES] __attribute__ ((aligned(ARM_L1_ALIGN)));
union arm_l1_entry l1_high[ARM_L1_MAX_ENTRIES] __attribute__ ((aligned(ARM_L1_ALIGN)));

static void map_kernel_section_lo(lvaddr_t va, union arm_l1_entry l1)
{
    assert( va < MEMORY_OFFSET );
    l1_low[ARM_L1_OFFSET(va)] = l1;
}

static void map_kernel_section_hi(lvaddr_t va, union arm_l1_entry l1)
{
    assert( va >= MEMORY_OFFSET );
    l1_high[ARM_L1_OFFSET(va)] = l1;
}

/**
 * /brief Return an L1 page table entry to map a 1MB 'section' of RAM
 * located at physical address 'pa'.
 */
static union arm_l1_entry make_ram_section(lpaddr_t pa)
{
    // Must be in the 1GB RAM region.
    assert(pa >= MEMORY_OFFSET && pa < (MEMORY_OFFSET + 0x40000000));
    union arm_l1_entry l1;

    l1.raw = 0;
    l1.section.type = L1_TYPE_SECTION_ENTRY;
    // The next three fields (tex,b,c) don't mean what their names
    // suggest - see the ARM ARM for explanations of this.
    // l1.section.tex	    = 1;
    //l1.section.bufferable   = 1;  // Disabled until we can figure
    // out caches.
    // l1.section.cacheable    = 1;
    l1.section.ap10         = 1;    // RW/NA
    l1.section.ap2          = 0;
    l1.section.base_address = ARM_L1_SECTION_NUMBER(pa);
    return l1;
}

/**
 * /brief Return an L1 page table entry to map a 1MB 'section' of
 * device memory located at physical address 'pa'.
 */
static union arm_l1_entry make_dev_section(lpaddr_t pa)
{
    // Must be below 2GB.
    assert(pa < MEMORY_OFFSET);
    union arm_l1_entry l1;

    l1.raw = 0;
    l1.section.type = L1_TYPE_SECTION_ENTRY;
    // l1.section.tex	    = 1;
    l1.section.bufferable   = 0;
    l1.section.cacheable    = 0;
    l1.section.ap10         = 3; // prev value: 3 // RW/NA RW/RW
    // l1.section.ap10         = 1;    // RW/NA
    l1.section.ap2          = 0;
    l1.section.base_address = ARM_L1_SECTION_NUMBER(pa);
    return l1;
}

/**
 * Create initial (temporary) page tables.
 *
 * We use 1MB (ARM_L1_SECTION_BYTES) pages (sections) with a single-level table.
 * This allows 1MB*4k (ARM_L1_MAX_ENTRIES) = 4G per pagetable.
 *
 * Hardware details can be found in:
 * ARM Architecture Reference Manual, ARMv7-A and ARMv7-R edition
 *   B3: Virtual Memory System Architecture (VMSA)
 */
void paging_init(void)
{
    /**
     * Make sure our page tables are correctly aligned in memory
     */
    assert(ROUND_UP((lpaddr_t)l1_low, ARM_L1_ALIGN) == (lpaddr_t)l1_low);
    assert(ROUND_UP((lpaddr_t)l1_high, ARM_L1_ALIGN) == (lpaddr_t)l1_high);

    /**
     * On ARMv7-A, physical RAM (PHYS_MEMORY_START) is the same with the
     * offset of mapped physical memory within virtual address space
     * (PHYS_MEMORY_START). 
     */
    STATIC_ASSERT(MEMORY_OFFSET == PHYS_MEMORY_START, "");

    /**
     * Zero the page tables: this has the effect of marking every PTE
     * as invalid.
     */
    memset(&l1_low, 0, sizeof(l1_low));
    memset(&l1_high, 0, sizeof(l1_high));

    /**
     * Now we layout the kernel's virtual address space.  It's quite
     * simple:
     * 00000000-7FFFFFFFF: 1-1 mappings (hardware we have not mapped
     *                     into high kernel space yet)
     * 80000000-BFFFFFFFF: 1-1 mappings (this is 1GB of RAM)
     * C0000000-FEFFFFFFF: On-demand mappings of hardware devices,
     *                     allocated descending from DEVICE_OFFSET.
     * FF000000-FFFFFFFFF: Exception vectors.  The 1MB region
     *                     containing these is mapped to low RAM.
     */    
    lvaddr_t base = 0;
    size_t i;
    for (i=0, base = 0; i < ARM_L1_MAX_ENTRIES/2; i++) {
	map_kernel_section_lo(base, make_dev_section(base));
        base += ARM_L1_SECTION_BYTES;
    }
    for (i=0, base = MEMORY_OFFSET; i < ARM_L1_MAX_ENTRIES/4; i++) {
	map_kernel_section_hi(base, make_ram_section(base));
        base += ARM_L1_SECTION_BYTES;
    }

    // Map the HiVecs exception vector table to RAM with a single kernel section
    map_kernel_section_hi(ETABLE_ADDR, make_ram_section(PHYS_MEMORY_START));

    /**
     * TTBCR: Translation Table Base Control register.
     *  TTBCR.N is bits[2:0]
     * In a TLB miss TTBCR.N determines whether TTBR0 or TTBR1 is used as the
     * base address for the translation table walk in memory:
     *  N == 0 -> always use TTBR0
     *  N >  0 -> if VA[31:32-N] > 0 use TTBR1 else use TTBR0
     *
     * TTBR0 is typically used for processes-specific addresses
     * TTBR1 is typically used for OS addresses that do not change on context
     *       switch
     *
     * set TTBCR.N = 1 to use TTBR1 for VAs >= MEMORY_OFFSET (=2GB)
     */
    assert(mmu_enabled == false);
    cp15_invalidate_i_and_d_caches_fast();
    cp15_invalidate_tlb();
    cp15_write_ttbr1((lpaddr_t)l1_high);
    cp15_write_ttbr0((lpaddr_t)l1_low);
    #define TTBCR_N 1
    uint32_t ttbcr = cp15_read_ttbcr();
    ttbcr =  (ttbcr & ~7) | TTBCR_N;
    cp15_write_ttbcr(ttbcr);
    STATIC_ASSERT(1UL<<(32-TTBCR_N) == MEMORY_OFFSET, "");
    #undef TTBCR_N
    cp15_enable_mmu();
    cp15_enable_alignment();
    cp15_invalidate_i_and_d_caches_fast();
    cp15_invalidate_tlb();
    mmu_enabled = true;
}

/**
 * \brief Return whether we have enabled the MMU. Useful for
 * initialization assertions
 */
bool paging_mmu_enabled(void)
{
    return mmu_enabled;
}

/**
 * /brief Perform a context switch.  Reload TTBR0 with the new
 * address, and invalidate the TLBs and caches. 
 */
void paging_context_switch(lpaddr_t ttbr)
{
    assert(ttbr > MEMORY_OFFSET);
    lpaddr_t old_ttbr = cp15_read_ttbr0();
    if (ttbr != old_ttbr)
    {
        cp15_write_ttbr0(ttbr);
        cp15_invalidate_tlb();
    }
}

/**
 * \brief Map a device into the kernel's address space.  
 * 
 * \param device_base is the physical address of the device
 * \param device_size is the number of bytes of physical address space
 * the device occupies. 
 *
 * \return the kernel virtual address of the mapped device, or panic. 
 */
lvaddr_t paging_map_device(lpaddr_t dev_base, size_t dev_size)
{
    // We map all hardware devices in the kernel using sections in the
    // top quarter (0xC0000000-0xFE000000) of the address space, just
    // below the exception vectors.  
    // 
    // It makes sense to use sections since (1) we don't map many
    // devices in the CPU driver anyway, and (2) if we did, it might
    // save a wee bit of TLB space. 
    //

    // First, we make sure that the device fits into a single
    // section. 
    if (ARM_L1_SECTION_NUMBER(dev_base) != ARM_L1_SECTION_NUMBER(dev_base+dev_size-1)) {
	panic("Attempt to map device spanning >1 section 0x%"PRIxLPADDR"+0x%x\n",
	      dev_base, dev_size );
    }
    
    // Now, walk down the page table looking for either (a) an

    // existing mapping, in which case return the address the device
    // is already mapped to, or an invalid mapping, in which case map
    // it. 
    uint32_t dev_section = ARM_L1_SECTION_NUMBER(dev_base);
    uint32_t dev_offset  = ARM_L1_SECTION_OFFSET(dev_base);
    lvaddr_t dev_virt    = 0;
    
    for( size_t i = ARM_L1_OFFSET( DEVICE_OFFSET - 1); i > ARM_L1_MAX_ENTRIES / 4 * 3; i-- ) {

	// Work out the virtual address we're looking at
	dev_virt = (lvaddr_t)(i << ARM_L1_SECTION_BITS);

	// If we already have a mapping for that address, return it. 
	if ( L1_TYPE(l1_high[i].raw) == L1_TYPE_SECTION_ENTRY &&
	     l1_high[i].section.base_address == dev_section ) {
	    return dev_virt + dev_offset;
	}

	// Otherwise, if it's free, map it. 
	if ( L1_TYPE(l1_high[i].raw) == L1_TYPE_INVALID_ENTRY ) {
	    map_kernel_section_hi(dev_virt, make_dev_section(dev_base));
	    cp15_invalidate_i_and_d_caches_fast();
	    cp15_invalidate_tlb();
	    return dev_virt + dev_offset;
	} 
    }
    // We're all out of section entries :-(
    panic("Ran out of section entries to map a kernel device");
}

/**
 * \brief Print out a L1 page table entry 'pte', interpreted relative
 * to a given virtual address 'va'. 
 */
static void paging_print_l1_pte(lvaddr_t va, union arm_l1_entry pte)
{
    printf("(memory offset=%x):\n", va);
    if ( L1_TYPE(pte.raw) == L1_TYPE_INVALID_ENTRY) {
	return;
    }
    printf( " %x-%"PRIxLVADDR": ", va, va + ARM_L1_SECTION_BYTES - 1);
    switch( L1_TYPE(pte.raw) ) { 
    case L1_TYPE_INVALID_ENTRY:
	printf("INVALID\n");
	break;
    case L1_TYPE_PAGE_TABLE_ENTRY:
	printf("L2 PT 0x%"PRIxLPADDR" pxn=%d ns=%d sbz=%d dom=0x%04x sbz1=%d \n", 
	       pte.page_table.base_address << 10, 
	       pte.page_table.pxn,
	       pte.page_table.ns,
	       pte.page_table.sbz0,
	       pte.page_table.domain,
	       pte.page_table.sbz1 );
	break;
    case L1_TYPE_SECTION_ENTRY:
	printf("SECTION 0x%"PRIxLPADDR" buf=%d cache=%d xn=%d dom=0x%04x\n", 
	       pte.section.base_address << 20, 
	       pte.section.bufferable,
	       pte.section.cacheable,
	       pte.section.execute_never,
	       pte.section.domain );
	printf("      sbz0=%d ap=0x%03x tex=0x%03x shr=%d ng=%d mbz0=%d ns=%d\n",
	       pte.section.sbz0,
	       (pte.section.ap2) << 2 | pte.section.ap10,
	       pte.section.tex,
	       pte.section.shareable,
	       pte.section.not_global,
	       pte.section.mbz0,
	       pte.section.ns );
	break;
    case L1_TYPE_SUPER_SECTION_ENTRY:
	printf("SUPERSECTION 0x%"PRIxLPADDR" buf=%d cache=%d xn=%d dom=0x%04x\n", 
	       pte.super_section.base_address << 24, 
	       pte.super_section.bufferable,
	       pte.super_section.cacheable,
	       pte.super_section.execute_never,
	       pte.super_section.domain );
	printf("      sbz0=%d ap=0x%03x tex=0x%03x shr=%d ng=%d mbz0=%d ns=%d\n",
	       pte.super_section.sbz0,
	       (pte.super_section.ap2) << 2 | pte.super_section.ap10,
	       pte.super_section.tex,
	       pte.super_section.shareable,
	       pte.super_section.not_global,
	       pte.super_section.mbz0,
	       pte.super_section.ns );
	break;
    }
}

/**
 * /brief Print out the CPU driver's two static page tables.  Note:
 * 
 * 1) This is a lot of output.  Each table has 4096 entries, each of
 *    which takes one or two lines of output.
 * 2) The first half of the TTBR1 table is similarly used, and is
 *    probably (hopefully) all empty. 
 * 3) The second half of the TTBR0 table is similarly never used, and
 *    hopefully empty. 
 * 4) The TTBR0 table is only used anyway at boot, since thereafter it
 *    is replaced by a user page table. 
 * Otherwise, go ahead and knock yourself out. 
 */
void paging_print_l1(void)
{
    size_t i;
    lvaddr_t base = 0;
    printf("TTBR1 table:\n");
    for(i = 0; i < ARM_L1_MAX_ENTRIES; i++, base += ARM_L1_SECTION_BYTES ) { 
	paging_print_l1_pte(base, l1_high[i]);
    }
    printf("TTBR0 table:\n");
    base = 0;
    for(i = 0; i < ARM_L1_MAX_ENTRIES; i++, base += ARM_L1_SECTION_BYTES ) { 
	paging_print_l1_pte(base, l1_low[i]);
    }
}


static errval_t
caps_map_l1(struct capability* dest,
            cslot_t            slot,
            struct capability* src,
            uintptr_t          kpi_paging_flags,
            uintptr_t          offset,
            uintptr_t          pte_count,
            struct cte*        mapping_cte)
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

    if (src->type != ObjType_VNode_ARM_l2) {
        //large page mapping goes here
        assert(0 == (kpi_paging_flags & ~KPI_PAGING_FLAGS_MASK));

        // ARM L1 has 4K entries, we need to fill in individual entries for
        // 1M sections
        // XXX: magic constant
        if (slot >= 4096) {
            panic("oops: slot >= 4096");
            return SYS_ERR_VNODE_SLOT_INVALID;
        }

        if (src->type != ObjType_Frame && src->type != ObjType_DevFrame) {
            panic("oops: src->type != ObjType_Frame && src->type != ObjType_DevFrame");
            return SYS_ERR_WRONG_MAPPING;
        }

        // check offset within frame
        if ((offset + pte_count * BYTES_PER_SECTION > get_size(src)) ||
            ((offset % BYTES_PER_SECTION) != 0)) {
            printf("offset = %"PRIuPTR", pte_count=%"PRIuPTR
                   ", src->size = %"PRIuGENSIZE", src->type = %d\n",
                    offset, pte_count, get_size(src), src->type);
            panic("oops: frame offset invalid");
            return SYS_ERR_FRAME_OFFSET_INVALID;
        }

        // check mapping does not overlap leaf page table
        if (slot + pte_count > 4096) {
            return SYS_ERR_VM_MAP_SIZE;
        }

        // Destination
        lpaddr_t dest_lpaddr = gen_phys_to_local_phys(get_address(dest));
        lvaddr_t dest_lvaddr = local_phys_to_mem(dest_lpaddr);

        union arm_l1_entry* entry = ((union arm_l1_entry*)dest_lvaddr) + slot;
        if (entry->invalid.type != L1_TYPE_INVALID_ENTRY) {
            panic("Remapping valid page.");
        }

        lpaddr_t src_lpaddr = gen_phys_to_local_phys(get_address(src) + offset);
        if ((src_lpaddr & (LARGE_PAGE_SIZE - 1))) {
            panic("Invalid target");
        }

        create_mapping_cap(mapping_cte, src,
                           dest_lpaddr + slot * sizeof(union arm_l1_entry),
                           pte_count);

        for (int i = 0; i < pte_count; i++) {
            entry->raw = 0;

            entry->section.type = L1_TYPE_SECTION_ENTRY;
            entry->section.bufferable = 1;
            entry->section.cacheable = (kpi_paging_flags & KPI_PAGING_FLAGS_NOCACHE)? 0: 1;
            entry->section.ap10 = (kpi_paging_flags & KPI_PAGING_FLAGS_READ)? 2:0;
            entry->section.ap10 |= (kpi_paging_flags & KPI_PAGING_FLAGS_WRITE)? 3:0;
            entry->section.ap2 = 0;
            entry->section.base_address = (src_lpaddr + i * BYTES_PER_SECTION) >> 20;

            entry++;

            debug(SUBSYS_PAGING, "L2 mapping %08"PRIxLVADDR"[%"PRIuCSLOT"] @%p = %08"PRIx32"\n",
                   dest_lvaddr, slot, entry, entry->raw);
        }

        // Flush TLB if remapping.
        cp15_invalidate_tlb();
        return SYS_ERR_OK;
    }

    // XXX: magic constant
    if (slot >= 4096) {
        printf("slot = %"PRIuCSLOT"\n",slot);
        return SYS_ERR_VNODE_SLOT_INVALID;
    }


    // check offset within frame
    if ((offset + pte_count * 1024 > get_size(src)) ||
            ((offset % 1024) != 0)) {
        printf("offset = %"PRIuPTR", pte_count=%"PRIuPTR
                ", src->size = %"PRIuGENSIZE", src->type = %d\n",
                offset, pte_count, get_size(src), src->type);
        return SYS_ERR_FRAME_OFFSET_INVALID;
    }

    // check mapping does not overlap leaf page table
    if (slot + pte_count > 4096) {
        return SYS_ERR_VM_MAP_SIZE;
    }


    if (slot >= ARM_L1_OFFSET(MEMORY_OFFSET)) {
        printf("slot = %"PRIuCSLOT"\n",slot);
        return SYS_ERR_VNODE_SLOT_RESERVED;
    }

    debug(SUBSYS_PAGING, "caps_map_l1: mapping %"PRIuPTR" L2 tables @%"PRIuCSLOT"\n",
            pte_count, slot);
    // Destination
    lpaddr_t dest_lpaddr = gen_phys_to_local_phys(get_address(dest));
    lvaddr_t dest_lvaddr = local_phys_to_mem(dest_lpaddr);

    union arm_l1_entry* entry = (union arm_l1_entry*)dest_lvaddr + slot;

    // Source
    genpaddr_t src_gpaddr = get_address(src);
    lpaddr_t   src_lpaddr = gen_phys_to_local_phys(src_gpaddr) + offset;

    assert(aligned(src_lpaddr, 1u << 10));
    assert((src_lpaddr < dest_lpaddr) || (src_lpaddr >= dest_lpaddr + 16384));

    create_mapping_cap(mapping_cte, src,
                       dest_lpaddr + slot * sizeof(union arm_l1_entry),
                       pte_count);

    for (int i = 0; i < pte_count; i++, entry++)
    {
        entry->raw = 0;
        entry->page_table.type   = L1_TYPE_PAGE_TABLE_ENTRY;
        entry->page_table.domain = 0;
        entry->page_table.base_address =
            (src_lpaddr + i * BASE_PAGE_SIZE / ARM_L1_SCALE) >> 10;
        debug(SUBSYS_PAGING, "L1 mapping %"PRIuCSLOT". @%p = %08"PRIx32"\n",
              slot + i, entry, entry->raw);
    }

    cp15_invalidate_tlb();

    return SYS_ERR_OK;
}

static errval_t
caps_map_l2(struct capability* dest,
            cslot_t            slot,
            struct capability* src,
            uintptr_t          kpi_paging_flags,
            uintptr_t          offset,
            uintptr_t          pte_count,
            struct cte*        mapping_cte)
{
    assert(0 == (kpi_paging_flags & ~KPI_PAGING_FLAGS_MASK));

    // ARM L2 has 256 entries, but we treat a 4K page as a consecutive
    // region of L2 with a single index. 4K == 4 * 1K
    if (slot >= (256 * 4)) {
        panic("oops: slot >= (256 * 4)");
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (src->type != ObjType_Frame && src->type != ObjType_DevFrame) {
        panic("oops: src->type != ObjType_Frame && src->type != ObjType_DevFrame");
        return SYS_ERR_WRONG_MAPPING;
    }

    // check offset within frame
    if ((offset + BYTES_PER_PAGE > get_size(src)) ||
        ((offset % BYTES_PER_PAGE) != 0)) {
        panic("oops: frame offset invalid");
        return SYS_ERR_FRAME_OFFSET_INVALID;
    }

    // check mapping does not overlap leaf page table
    if (slot + pte_count > (256 * 4)) {
        return SYS_ERR_VM_MAP_SIZE;
    }

    // Destination
    lpaddr_t dest_lpaddr = gen_phys_to_local_phys(get_address(dest));
    lvaddr_t dest_lvaddr = local_phys_to_mem(dest_lpaddr);

    union arm_l2_entry* entry = (union arm_l2_entry*)dest_lvaddr + slot;
    if (entry->small_page.type != L2_TYPE_INVALID_PAGE) {
        panic("Remapping valid page.");
    }

    lpaddr_t src_lpaddr = gen_phys_to_local_phys(get_address(src) + offset);
    if ((src_lpaddr & (BASE_PAGE_SIZE - 1))) {
        panic("Invalid target");
    }

    create_mapping_cap(mapping_cte, src,
                       dest_lpaddr + slot * sizeof(union arm_l2_entry),
                       pte_count);

    for (int i = 0; i < pte_count; i++) {
        entry->raw = 0;

        entry->small_page.type = L2_TYPE_SMALL_PAGE;
        paging_set_flags(entry, kpi_paging_flags);
        entry->small_page.base_address = (src_lpaddr + i * BYTES_PER_PAGE) >> 12;

        debug(SUBSYS_PAGING, "L2 mapping %08"PRIxLVADDR"[%"PRIuCSLOT"] @%p = %08"PRIx32"\n",
               dest_lvaddr, slot, entry, entry->raw);

        entry++;
    }

    // Flush TLB if remapping.
    cp15_invalidate_tlb();

    return SYS_ERR_OK;
}

/// Create page mappings
errval_t caps_copy_to_vnode(struct cte *dest_vnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count,
                            struct cte *mapping_cte)
{
    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_vnode_cte->cap;
    assert(mapping_cte->cap.type == ObjType_Null);
    errval_t err;

    if (ObjType_VNode_ARM_l1 == dest_cap->type) {
        //printf("caps_map_l1: %zu\n", (size_t)pte_count);
        err = caps_map_l1(dest_cap, dest_slot, src_cap,
                           flags,
                           offset,
                           pte_count,
                           mapping_cte
                          );
    }
    else if (ObjType_VNode_ARM_l2 == dest_cap->type) {
        //printf("caps_map_l2: %zu\n", (size_t)pte_count);
        err = caps_map_l2(dest_cap, dest_slot, src_cap,
                           flags,
                           offset,
                           pte_count,
                           mapping_cte
                          );
    }
    else {
        panic("ObjType not VNode");
    }

    if (err_is_fail(err)) {
        memset(mapping_cte, 0, sizeof(*mapping_cte));
        return err;
    }

    assert(type_is_mapping(mapping_cte->cap.type));
    err = mdb_insert(mapping_cte);
    if (err_is_fail(err)) {
        printk(LOG_ERR, "%s: mdb_insert: %"PRIuERRV"\n", __FUNCTION__, err);
    }

    TRACE_CAP_MSG("created", mapping_cte);

    return err;
}

size_t do_unmap(lvaddr_t pt, cslot_t slot, size_t num_pages)
{
    size_t unmapped_pages = 0;
    union arm_l2_entry *ptentry = (union arm_l2_entry *)pt + slot;
    for (int i = 0; i < num_pages; i++) {
        ptentry++->raw = 0;
        unmapped_pages++;
    }
    return unmapped_pages;
}

errval_t paging_modify_flags(struct capability *mapping, uintptr_t offset,
                             uintptr_t pages, uintptr_t kpi_paging_flags)
{
    // XXX: modify flags for sections?
    assert(type_is_mapping(mapping->type));
    // check flags
    assert(0 == (kpi_paging_flags & ~KPI_PAGING_FLAGS_MASK));

    struct Frame_Mapping *info = &mapping->u.frame_mapping;

    /* Calculate location of page table entries we need to modify */
    lvaddr_t base = local_phys_to_mem(info->pte) +
        offset * sizeof(union arm_l2_entry);

    for (int i = 0; i < pages; i++) {
        union arm_l2_entry *entry =
            (union arm_l2_entry *)base + i;
        paging_set_flags(entry, kpi_paging_flags);
    }

    return paging_tlb_flush_range(cte_for_cap(mapping), offset, pages);
}

void paging_dump_tables(struct dcb *dispatcher)
{
    if (!local_phys_is_valid(dispatcher->vspace)) {
        printk(LOG_ERR, "dispatcher->vspace = 0x%"PRIxLPADDR": too high!\n" ,
               dispatcher->vspace);
        return;
    }
    lvaddr_t l1 = local_phys_to_mem(dispatcher->vspace);

    for (int l1_index = 0; l1_index < ARM_L1_MAX_ENTRIES; l1_index++) {
        // get level2 table
        union arm_l1_entry *l1_e = (union arm_l1_entry *)l1 + l1_index;
        if (!l1_e->raw) { continue; }
        if (l1_e->invalid.type == 2) { // section 
            genpaddr_t paddr = (genpaddr_t)(l1_e->section.base_address) << 20;
            printf("%d: (section) 0x%"PRIxGENPADDR"\n", l1_index, paddr);
        } else if (l1_e->invalid.type == 1) { // l2 table
            genpaddr_t ptable_gp = (genpaddr_t)(l1_e->page_table.base_address) << 10;
            lvaddr_t ptable_lv = local_phys_to_mem(gen_phys_to_local_phys(ptable_gp));

            printf("%d: (l2table) 0x%"PRIxGENPADDR"\n", l1_index, ptable_gp);

            for (int entry = 0; entry < ARM_L2_MAX_ENTRIES; entry++) {
                union arm_l2_entry *e =
                    (union arm_l2_entry *)ptable_lv + entry;
                genpaddr_t paddr = (genpaddr_t)(e->small_page.base_address) << BASE_PAGE_BITS;
                if (!paddr) {
                    continue;
                }
                printf("%d.%d: 0x%"PRIxGENPADDR"\n", l1_index, entry, paddr);
            }
        }
    }
}

/**
 * \brief Install a page table pointer in a level 1 page table
 * located at 'table_base' to map addresses starting at virtual
 * address 'va'.  The level 2 page table to be used is assumed to be
 * located at physical address 'pa'. 
 */
void paging_map_user_pages_l1(lvaddr_t table_base, lvaddr_t va, lpaddr_t pa)
{
    assert(aligned(table_base, ARM_L1_ALIGN));
    assert(aligned(pa, BYTES_PER_SMALL_PAGE));

    union arm_l1_entry e;
    union arm_l1_entry *l1_table;

    e.raw                     = 0;
    e.page_table.type         = L1_TYPE_PAGE_TABLE_ENTRY;
    e.page_table.domain       = 0;
    e.page_table.base_address = ARM_L2_TABLE_PPN(pa);

    if (table_base == 0) {
        if(va < MEMORY_OFFSET) {
            table_base = cp15_read_ttbr0() + MEMORY_OFFSET;
	} else {
            table_base = cp15_read_ttbr1() + MEMORY_OFFSET;
	}
    }
    l1_table = (union arm_l1_entry *) table_base;
    l1_table[ARM_L1_OFFSET(va)] = e;
}

/**
 * /brief Install a level 2 page table entry located at l2e, to map
 * physical address 'pa', with flags 'flags'.   'flags' here is in the
 * form of a prototype 32-bit L2 *invalid* PTE with address 0.
 */
void paging_set_l2_entry(uintptr_t* l2e, lpaddr_t addr, uintptr_t flags)
{
    union arm_l2_entry e;
    e.raw = flags;

    assert( L2_TYPE(e.raw) == L2_TYPE_INVALID_PAGE );
    assert( e.small_page.base_address == 0);
    assert( ARM_PAGE_OFFSET(addr) == 0 );

    e.small_page.type = L2_TYPE_SMALL_PAGE;
    e.small_page.base_address = (addr >> 12);

    *l2e = e.raw;
}
