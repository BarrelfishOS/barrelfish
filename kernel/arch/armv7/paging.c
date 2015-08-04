/*
 * Copyright (c) 2009 - 2013 ETH Zurich.
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

        struct cte *src_cte = cte_for_cap(src);
        src_cte->mapping_info.pte_count = pte_count;
        src_cte->mapping_info.pte = dest_lpaddr;
        src_cte->mapping_info.offset = offset;

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

    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.pte = dest_lpaddr + slot;
    src_cte->mapping_info.offset = 0;

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
            uintptr_t          pte_count)
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

    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.pte = dest_lpaddr;
    src_cte->mapping_info.offset = offset;

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
                            uintptr_t offset, uintptr_t pte_count)
{
    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_vnode_cte->cap;

    if (src_cte->mapping_info.pte) {
        return SYS_ERR_VM_ALREADY_MAPPED;
    }

    if (ObjType_VNode_ARM_l1 == dest_cap->type) {
        //printf("caps_map_l1: %zu\n", (size_t)pte_count);
        return caps_map_l1(dest_cap, dest_slot, src_cap,
                           flags,
                           offset,
                           pte_count
                          );
    }
    else if (ObjType_VNode_ARM_l2 == dest_cap->type) {
        //printf("caps_map_l2: %zu\n", (size_t)pte_count);
        return caps_map_l2(dest_cap, dest_slot, src_cap,
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
    union arm_l2_entry *ptentry = (union arm_l2_entry *)pt + slot;
    for (int i = 0; i < num_pages; i++) {
        ptentry++->raw = 0;
        unmapped_pages++;
    }
    return unmapped_pages;
}

static inline void read_pt_entry(struct capability *pgtable,
        size_t slot, bool is_section, genpaddr_t *paddr)
{
    assert(type_is_vnode(pgtable->type));
    assert(paddr);

    genpaddr_t gp = get_address(pgtable);
    lpaddr_t lp = gen_phys_to_local_phys(gp);
    lvaddr_t lv = local_phys_to_mem(lp);

    switch (pgtable->type) {
        case ObjType_VNode_ARM_l1:
        {
            union arm_l1_entry *e = (union arm_l1_entry*)lv;
            if (is_section) {
                *paddr = (genpaddr_t)(e->section.base_address) << 20;
                return;
            } else {
                *paddr = (genpaddr_t)(e->page_table.base_address) << 10;
                return;
            }
        }
        case ObjType_VNode_ARM_l2:
        {
            union arm_l2_entry *e = (union arm_l2_entry*)lv;
            *paddr = (genpaddr_t)(e->small_page.base_address) << 12;
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

    bool is_section = false;
    if (pgtable->type == ObjType_VNode_ARM_l1) {
        // transform slot to hw slot
        if (mapping->cap.type == ObjType_VNode_ARM_l2) {
            // l2 table
            debug(SUBSYS_PAGING, "unmapping l2 tables: %zu, #pages: %zu\n",
                    slot, num_pages);
        }
        else {
            // section
            is_section = true;
            debug(SUBSYS_PAGING, "unmapping section: %zu, #pages: %zu\n",
                    slot, num_pages);
        }
    }
    // get page table entry data
    genpaddr_t paddr;
    //lpaddr_t pte;
    read_pt_entry(pgtable, slot, is_section, &paddr);
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
        debug(SUBSYS_PAGING, "num_pages = %zu, mapping = %zu\n",
                num_pages, mapping->mapping_info.pte_count);
        // want to unmap a different amount of pages than was mapped
        return SYS_ERR_VM_MAP_SIZE;
    }

    do_unmap(pt, slot, num_pages);

    // flush TLB for unmapped pages
    // TODO: selective TLB flush
    cp15_invalidate_tlb();

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
        union arm_l2_entry *entry =
            (union arm_l2_entry *)base + i;
        paging_set_flags(entry, kpi_paging_flags);
    }

    return paging_tlb_flush_range(mapping, offset, pages);
}

void paging_dump_tables(struct dcb *dispatcher)
{
    printf("dump_hw_page_tables\n");
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
