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
#include <dispatch.h>
#include <sysreg.h>
#include <paging_kernel_arch.h>
#include <string.h>
#include <exceptions.h>
#include <arm_hal.h>
#include <cap_predicates.h>
#include <dispatch.h>

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
paging_set_flags(union armv8_l3_entry *entry, uintptr_t kpi_paging_flags)
{

		entry->page.ap = 0;

		if(kpi_paging_flags & KPI_PAGING_FLAGS_WRITE)
			entry->page.ap = 1;
		else if (kpi_paging_flags & KPI_PAGING_FLAGS_READ)
			entry->page.ap = 3;
		else
			panic("oops: wrong page flags");

		entry->page.af = 1;
}

static errval_t
caps_map_l1(struct capability* dest,
            cslot_t            slot,
            struct capability* src,
            uintptr_t          kpi_paging_flags,
            uintptr_t          offset,
            uintptr_t          pte_count)
{
    //
    // Note:
    //
    // We have chicken-and-egg problem in initializing resources so
    // instead of treating an L3 table it's actual 1K size, we treat
    // it as being 4K. As a result when we map an "L3" table we actually
    // map a page of memory as if it is 4 consecutive L3 tables.
    //
    // See lib/barrelfish/arch/arm/pmap_arch.c for more discussion.
    //

    if (slot >= PTABLE_NUM_ENTRIES) {
        printf("slot = %"PRIuCSLOT"\n",slot);
        panic("oops: slot id >= %d", PTABLE_NUM_ENTRIES);
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (pte_count != 1) {
        printf("pte_count = %zu\n",(size_t)pte_count);
        panic("oops: pte_count");
        return SYS_ERR_VM_MAP_SIZE;
    }

    if (src->type != ObjType_VNode_AARCH64_l2) {
        panic("oops: l1 wrong src type");
        return SYS_ERR_WRONG_MAPPING;
    }

    if (slot >= ARMv8_L1_OFFSET(MEMORY_OFFSET)) {
        printf("slot = %"PRIuCSLOT"\n",slot);
        panic("oops: l1 slot id");
        return SYS_ERR_VNODE_SLOT_RESERVED;
    }

    // Destination
    lpaddr_t dest_lpaddr = gen_phys_to_local_phys(get_address(dest));
    lvaddr_t dest_lvaddr = local_phys_to_mem(dest_lpaddr);

    union armv8_l1_entry* entry = (union armv8_l1_entry*)dest_lvaddr + slot;

    // Source
    genpaddr_t src_gpaddr = get_address(src);
    lpaddr_t   src_lpaddr = gen_phys_to_local_phys(src_gpaddr);

	//union armv8_l2_entry* entry1 = (union armv8_l2_entry*)local_phys_to_mem(src_gpaddr);


    assert(offset == 0);
    assert(aligned(src_lpaddr, 1u << 12));
    assert((src_lpaddr < dest_lpaddr) || (src_lpaddr >= dest_lpaddr + 32));

    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.pte = dest_lpaddr + slot;
    src_cte->mapping_info.offset = 0;

    entry->raw = 0;
    entry->page_table.type   = ARMv8_Ln_TABLE;
    entry->page_table.base_address =
            (src_lpaddr) >> 12;
    debug(SUBSYS_PAGING, "L1 mapping %"PRIuCSLOT". @%p = %08"PRIx32"\n",
              slot, entry, entry->raw);

    sysreg_invalidate_tlb();

    return SYS_ERR_OK;
}

static errval_t
caps_map_l2(struct capability* dest,
            cslot_t            slot,
            struct capability* src,
            uintptr_t          kpi_paging_flags,
            uintptr_t          offset,
            uintptr_t          pte_count)
{
    //
    // Note:
    //
    // We have chicken-and-egg problem in initializing resources so
    // instead of treating an L3 table it's actual 1K size, we treat
    // it as being 4K. As a result when we map an "L3" table we actually
    // map a page of memory as if it is 4 consecutive L3 tables.
    //
    // See lib/barrelfish/arch/arm/pmap_arch.c for more discussion.
    //
    if (slot >= PTABLE_NUM_ENTRIES) {
        printf("slot = %"PRIuCSLOT"\n",slot);
        panic("oops: slot id >= 512");
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (pte_count != 1) {
        printf("pte_count = %zu\n",(size_t)pte_count);
        panic("oops: pte_count");
        return SYS_ERR_VM_MAP_SIZE;
    }

    if (src->type != ObjType_VNode_AARCH64_l3) {
        panic("oops: l2 wrong src type");
        return SYS_ERR_WRONG_MAPPING;
    }

    if (slot > PTABLE_NUM_ENTRIES) {
        printf("slot = %"PRIuCSLOT"\n",slot);
        panic("oops: l2 slot id");
        return SYS_ERR_VNODE_SLOT_RESERVED;
    }

    // Destination
    lpaddr_t dest_lpaddr = gen_phys_to_local_phys(get_address(dest));
    lvaddr_t dest_lvaddr = local_phys_to_mem(dest_lpaddr);

    union armv8_l2_entry* entry = (union armv8_l2_entry*)dest_lvaddr + slot;

    // Source
    genpaddr_t src_gpaddr = get_address(src);
    lpaddr_t   src_lpaddr = gen_phys_to_local_phys(src_gpaddr);

    assert(offset == 0);
    assert(aligned(src_lpaddr, 1u << 12));
    assert((src_lpaddr < dest_lpaddr) || (src_lpaddr >= dest_lpaddr + 4096));

    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.pte = dest_lpaddr + slot;
    src_cte->mapping_info.offset = 0;

    entry->raw = 0;
    entry->page_table.type   = ARMv8_Ln_TABLE;
    entry->page_table.base_address =
            (src_lpaddr) >> 12;
    debug(SUBSYS_PAGING, "L2 mapping %"PRIuCSLOT". @%p = %08"PRIx32"\n",
              slot, entry, entry->raw);

    sysreg_invalidate_tlb();

    return SYS_ERR_OK;
}


static errval_t
caps_map_l3(struct capability* dest,
            cslot_t            slot,
            struct capability* src,
            uintptr_t          kpi_paging_flags,
            uintptr_t          offset,
            uintptr_t          pte_count)
{
    assert(0 == (kpi_paging_flags & ~KPI_PAGING_FLAGS_MASK));

    // ARM L3 has 256 entries, but we treat a 4K page as a consecutive
    // region of L3 with a single index. 4K == 4 * 1K
    if (slot >= PTABLE_NUM_ENTRIES) {
        panic("oops: slot >= 512");
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (src->type != ObjType_Frame && src->type != ObjType_DevFrame) {
        panic("oops: src->type != ObjType_Frame && src->type != ObjType_DevFrame");
        return SYS_ERR_WRONG_MAPPING;
    }

    // check offset within frame
    if ((offset + BASE_PAGE_SIZE > get_size(src)) ||
        ((offset % BASE_PAGE_SIZE) != 0)) {
        panic("oops: frame offset invalid");
        return SYS_ERR_FRAME_OFFSET_INVALID;
    }

    // check mapping does not overlap leaf page table
    if (slot + pte_count > PTABLE_NUM_ENTRIES ) {
        return SYS_ERR_VM_MAP_SIZE;
    }

    // Destination
    lpaddr_t dest_lpaddr = gen_phys_to_local_phys(get_address(dest));
    lvaddr_t dest_lvaddr = local_phys_to_mem(dest_lpaddr);

    union armv8_l3_entry* entry = (union armv8_l3_entry*)dest_lvaddr + slot;
    if (entry->page.type != ARMv8_Ln_INVALID) {
        panic("Remapping valid page.");
    }

    lpaddr_t src_lpaddr = gen_phys_to_local_phys(get_address(src) + offset);
    if ((src_lpaddr & (BASE_PAGE_SIZE - 1))) {
        panic("Invalid target");
    }

    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.pte = dest_lpaddr;
    src_cte->mapping_info.offset = offset;

    for (int i = 0; i < pte_count; i++) {
        entry->raw = 0;

        entry->page.type = ARMv8_L3_PAGE;
        paging_set_flags(entry, kpi_paging_flags);
        entry->page.base_address = (src_lpaddr + i * BASE_PAGE_SIZE) >> 12;

		entry++;

        debug(SUBSYS_PAGING, "L3 mapping %08"PRIxLVADDR"[%"PRIuCSLOT"] @%p = %08"PRIx32"\n",
               dest_lvaddr, slot, entry, entry->raw);
    }

    // Flush TLB if remapping.
    sysreg_invalidate_tlb();

    return SYS_ERR_OK;
}

/// Create page mappings
errval_t caps_copy_to_vnode(struct cte *dest_vnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count)
{
    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_vnode_cte->cap;

    if (src_cte->mapping_info.pte) {
        return SYS_ERR_VM_ALREADY_MAPPED;
    }

    if (ObjType_VNode_AARCH64_l1 == dest_cap->type) {
        return caps_map_l1(dest_cap, dest_slot, src_cap,
                           flags,
                           offset,
                           pte_count
                          );
    }
    else if (ObjType_VNode_AARCH64_l2 == dest_cap->type) {
        return caps_map_l2(dest_cap, dest_slot, src_cap,
                           flags,
                           offset,
                           pte_count
                          );
    }
    else if (ObjType_VNode_AARCH64_l3 == dest_cap->type) {
        return caps_map_l3(dest_cap, dest_slot, src_cap,
                           flags,
                           offset,
                           pte_count
                          );
    }
    else {
        panic("ObjType not VNode");
    }
}

size_t do_unmap(lvaddr_t pt, cslot_t slot, size_t num_pages)
{
    size_t unmapped_pages = 0;
    union armv8_l3_entry *ptentry = (union armv8_l3_entry *)pt + slot;
    for (int i = 0; i < num_pages; i++) {
        ptentry++->raw = 0;
        unmapped_pages++;
    }
    return unmapped_pages;
}

static inline void read_pt_entry(struct capability *pgtable, size_t slot, genpaddr_t *paddr)
{
    assert(type_is_vnode(pgtable->type));
    assert(paddr);

    genpaddr_t gp = get_address(pgtable);
    lpaddr_t lp = gen_phys_to_local_phys(gp);
    lvaddr_t lv = local_phys_to_mem(lp);

    switch (pgtable->type) {
        case ObjType_VNode_AARCH64_l1:
        {
            union armv8_l1_entry *e = (union armv8_l1_entry*)lv;
            *paddr = (genpaddr_t)(e->page_table.base_address) << 12;
            return;
        }
        case ObjType_VNode_AARCH64_l2:
        {
            union armv8_l2_entry *e = (union armv8_l2_entry*)lv;
            *paddr = (genpaddr_t)(e->page_table.base_address) << 12;
            return;
        }
        case ObjType_VNode_AARCH64_l3:
        {
            union armv8_l3_entry *e = (union armv8_l3_entry*)lv;
            *paddr = (genpaddr_t)(e->page.base_address) << 12;
            return;
        }
        default:
            assert(!"Should not get here");
    }
}

errval_t page_mappings_unmap(struct capability *pgtable, struct cte *mapping, size_t slot, size_t num_pages)
{
    assert(type_is_vnode(pgtable->type));
    //printf("page_mappings_unmap(%zd pages, slot = %zd)\n", num_pages, slot);

    // get page table entry data
    genpaddr_t paddr;
    //lpaddr_t pte;
    read_pt_entry(pgtable, slot, &paddr);
    lvaddr_t pt = local_phys_to_mem(gen_phys_to_local_phys(get_address(pgtable)));

    // get virtual address of first page
    // TODO: error checking
    genvaddr_t vaddr;
    struct cte *leaf_pt = cte_for_cap(pgtable);
    compile_vaddr(leaf_pt, slot, &vaddr);
    //genvaddr_t vend = vaddr + num_pages * BASE_PAGE_SIZE;
    // printf("vaddr = 0x%"PRIxGENVADDR"\n", vaddr);
    // printf("num_pages = %zu\n", num_pages);

    // get cap for mapping
    /*
    struct cte *mem;
    errval_t err = lookup_cap_for_mapping(paddr, pte, &mem);
    if (err_is_fail(err)) {
        printf("page_mappings_unmap: %ld\n", err);
        return err;
    }
    */
    //printf("state before unmap: mapped_pages = %zd\n", mem->mapping_info.mapped_pages);
    //printf("state before unmap: num_pages    = %zd\n", num_pages);

    if (num_pages != mapping->mapping_info.pte_count) {
        printf("num_pages = %zu, mapping = %zu\n", num_pages, mapping->mapping_info.pte_count);
        // want to unmap a different amount of pages than was mapped
        return SYS_ERR_VM_MAP_SIZE;
    }

    do_unmap(pt, slot, num_pages);

    // flush TLB for unmapped pages
    // TODO: selective TLB flush
    sysreg_invalidate_tlb();

    // update mapping info
    memset(&mapping->mapping_info, 0, sizeof(struct mapping_info));

    return SYS_ERR_OK;
}

errval_t paging_modify_flags(struct capability *frame, uintptr_t offset,
                             uintptr_t pages, uintptr_t kpi_paging_flags)
{
    // check flags
    assert(0 == (kpi_paging_flags & ~KPI_PAGING_FLAGS_MASK));

    struct cte *mapping = cte_for_cap(frame);
    struct mapping_info *info = &mapping->mapping_info;

    /* Calculate location of page table entries we need to modify */
    lvaddr_t base = local_phys_to_mem(info->pte) + offset;

    for (int i = 0; i < pages; i++) {
        union armv8_l3_entry *entry =
            (union armv8_l3_entry *)base + i;
        paging_set_flags(entry, kpi_paging_flags);
    }

    return paging_tlb_flush_range(mapping, 0, pages);
}

void paging_dump_tables(struct dcb *dispatcher)
{
    printf("dump_hw_page_tables\n");
    lvaddr_t l1 = local_phys_to_mem(dispatcher->vspace);

    for (int l1_index = 0; l1_index < PTABLE_NUM_ENTRIES; l1_index++) {
        // get level3 table
        union armv8_l1_entry *l1_e = (union armv8_l1_entry *)l1 + l1_index;
        if (!l1_e->raw) { continue; }
        genpaddr_t ptable_gp = (genpaddr_t)(l1_e->page_table.base_address) << 10;
        lvaddr_t ptable_lv = local_phys_to_mem(gen_phys_to_local_phys(ptable_gp));

        for (int entry = 0; entry < PTABLE_NUM_ENTRIES; entry++) {
            union armv8_l3_entry *e =
                (union armv8_l3_entry *)ptable_lv + entry;
            genpaddr_t paddr = (genpaddr_t)(e->page.base_address) << BASE_PAGE_BITS;
            if (!paddr) {
                continue;
            }
            printf("%d.%d: 0x%"PRIxGENPADDR"\n", l1_index, entry, paddr);
        }
    }
}
