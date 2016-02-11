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
#include <arm_hal.h>
#include <cap_predicates.h>
#include <dispatch.h>
#include <mdb/mdb_tree.h>

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
