/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010-2013, 2016 ETH Zurich.
 * Copyright (c) 2014, HP Labs.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <target/x86_32/paging_kernel_target.h>
#include <target/x86_32/offsets_target.h>
#include <paging_kernel_arch.h>
#include <mdb/mdb_tree.h>
#include <string.h>
#include <cap_predicates.h>
#include <paging_generic.h>

#ifdef CONFIG_PAE
/// Map within a x86_32 pdpt
static errval_t x86_32_pdpt(struct capability *dest, cslot_t slot,
                            struct capability * src, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count,
                            struct cte *mapping_cte)
{
    if (slot >= X86_32_PTABLE_SIZE) { // Slot within page table
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (pte_count > 1) { // disallow multiple pdpt mappings at a time
        return SYS_ERR_VM_MAP_SIZE;
    }

    if (src->type != ObjType_VNode_x86_32_pdir) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    if(slot >= X86_32_PDPTE_BASE(X86_32_MEMORY_OFFSET)) { // Kernel mapped here
        return SYS_ERR_VNODE_SLOT_RESERVED;
    }

    // Destination
    genpaddr_t dest_gp   = dest->u.vnode_x86_32_pdpt.base;
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    union x86_32_pdpte_entry *entry =
        (union x86_32_pdpte_entry *)dest_lv + slot;

    // Set metadata
    create_mapping_cap(mapping_cte, src,
                       dest_lp + slot * sizeof(union x86_32_pdpte_entry),
                       pte_count);

    // Source
    genpaddr_t src_gp   = src->u.vnode_x86_32_pdir.base;
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    paging_x86_32_map_pdpte(entry, src_lp);
    paging_x86_32_context_switch(dcb_current->vspace); // To flush TLB

    return SYS_ERR_OK;
}
#endif

/// Map within a x86_32 pdir
static errval_t x86_32_pdir(struct capability *dest, cslot_t slot,
                            struct capability * src, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count,
                            struct cte *mapping_cte)
{
    //printf("x86_32_pdir\n");
    if (slot >= X86_32_PTABLE_SIZE) { // Slot within page table
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (slot + pte_count > X86_32_PTABLE_SIZE) {
        // check that mapping fits page directory
        return SYS_ERR_VM_MAP_SIZE;
    }

#ifndef CONFIG_PAE
    if(slot >= X86_32_PDIR_BASE(X86_32_MEMORY_OFFSET)) { // Kernel mapped here
        return SYS_ERR_VNODE_SLOT_RESERVED;
    }
#endif

    // large page code
    if(src->type == ObjType_Frame || src->type == ObjType_DevFrame)
    {
        cslot_t last_slot = slot + pte_count;

        // check offset within frame
        if (offset + pte_count * X86_32_LARGE_PAGE_SIZE > get_size(src)) {
            return SYS_ERR_FRAME_OFFSET_INVALID;
        }

        /* Calculate page access protection flags */
        // Get frame cap rights
        paging_x86_32_flags_t flags_large =
            paging_x86_32_cap_to_page_flags(src->rights);
        // Mask with provided access rights mask
        flags_large = paging_x86_32_mask_attrs(flags_large, X86_32_PTABLE_ACCESS(flags));
        // Add additional arch-specific flags
        flags_large |= X86_32_PTABLE_FLAGS(flags);
        // Unconditionally mark the page present
        flags_large |= X86_32_PTABLE_PRESENT;

        // Convert destination base address
        genpaddr_t dest_gp   = get_address(dest);
        lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
        lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
        // Convert source base address
        genpaddr_t src_gp   = get_address(src);
        lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
        // Set metadata
        create_mapping_cap(mapping_cte, src,
                           dest_lp + slot * sizeof(union x86_32_ptable_entry),
                           offset,
                           pte_count);

        for (; slot < last_slot; slot++, offset += X86_32_LARGE_PAGE_SIZE) {
            union x86_32_ptable_entry *entry =
                (union x86_32_ptable_entry *)dest_lv + slot;

            /* FIXME: Flush TLB if the page is already present
             * in the meantime, since we don't do this, we just assert that
             * we never reuse a VA mapping */
            if (X86_32_IS_PRESENT(entry)) {
                printf("Trying to map into an already present page NYI.\n");
                return SYS_ERR_VNODE_SLOT_INUSE;
            }

            // Carry out the page mapping
            paging_x86_32_map_large(entry, src_lp + offset, flags_large);
        }

        return SYS_ERR_OK;
    }

    if (src->type != ObjType_VNode_x86_32_ptable) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    // Destination
    genpaddr_t dest_gp   = dest->u.vnode_x86_32_pdir.base;
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    union x86_32_pdir_entry *entry =
        (union x86_32_pdir_entry *)dest_lv + slot;

    // Set metadata
    create_mapping_cap(mapping_cte, src,
                       dest_lp + slot * sizeof(union x86_32_pdir_entry),
                       pte_count);


    // Source
    // XXX: offset is ignored
    genpaddr_t src_gp   = src->u.vnode_x86_32_pdir.base;
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    paging_x86_32_map_table(entry, src_lp);

    return SYS_ERR_OK;
}

/// Map within a x86_32 ptable
static errval_t x86_32_ptable(struct capability *dest, cslot_t slot,
                              struct capability * src, uintptr_t uflags,
                              uintptr_t offset, uintptr_t pte_count,
                              struct cte *mapping_cte)
{
    //printf("x86_32_ptable\n");
    if (slot >= X86_32_PTABLE_SIZE) { // Slot within page table
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    cslot_t last_slot = slot + pte_count;

    if (last_slot > X86_32_PTABLE_SIZE) {
        printf("slot = %"PRIuCSLOT", last_slot = %"PRIuCSLOT", PTABLE_SIZE = %d\n", slot, last_slot, X86_32_PTABLE_SIZE);
        return SYS_ERR_VM_MAP_SIZE;
    }

    if (src->type != ObjType_Frame &&
        src->type != ObjType_DevFrame) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    // check offset within frame
    if (offset + pte_count * X86_32_BASE_PAGE_SIZE > get_size(src)) {
        return SYS_ERR_FRAME_OFFSET_INVALID;
    }

    /* Calculate page access protection flags */
    // Get frame cap rights
    paging_x86_32_flags_t flags =
        paging_x86_32_cap_to_page_flags(src->rights);
    // Mask with provided access rights mask
    flags = paging_x86_32_mask_attrs(flags, X86_32_PTABLE_ACCESS(uflags));
    // Add additional arch-specific flags
    flags |= X86_32_PTABLE_FLAGS(uflags);
    // Unconditionally mark the page present
    flags |= X86_32_PTABLE_PRESENT;

    // Convert destination base address
    genpaddr_t dest_gp   = get_address(dest);
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    // Convert source base address
    genpaddr_t src_gp   = get_address(src);
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    // Set metadata
    create_mapping_cap(mapping_cte, src,
                       dest_lp + slot * sizeof(union x86_32_ptable_entry),
                       offset,
                       pte_count);


    for (; slot < last_slot; slot++, offset += X86_32_BASE_PAGE_SIZE) {
        union x86_32_ptable_entry *entry =
            (union x86_32_ptable_entry *)dest_lv + slot;

        /* FIXME: Flush TLB if the page is already present
         * in the meantime, since we don't do this, we just assert that
         * we never reuse a VA mapping */
        if (X86_32_IS_PRESENT(entry)) {
            panic("Trying to map into an already present page NYI.");
        }

        // Carry out the page mapping
        paging_x86_32_map(entry, src_lp + offset, flags);
    }

    return SYS_ERR_OK;
}

typedef errval_t (*mapping_handler_t)(struct capability *dest_cap,
                                      cslot_t dest_slot,
                                      struct capability *src_cap,
                                      uintptr_t flags, uintptr_t offset,
                                      uintptr_t pte_count,
                                      struct cte *mapping_cte);

/// Dispatcher table for the type of mapping to create
static mapping_handler_t handler[ObjType_Num] = {
#ifdef CONFIG_PAE
    [ObjType_VNode_x86_32_pdpt]   = x86_32_pdpt,
#endif
    [ObjType_VNode_x86_32_pdir]   = x86_32_pdir,
    [ObjType_VNode_x86_32_ptable] = x86_32_ptable,
};

/// Create page mappings
errval_t caps_copy_to_vnode(struct cte *dest_vnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count,
                            struct cte *mapping_cte)
{
    assert(type_is_vnode(dest_vnode_cte->cap.type));
    assert(mapping_cte->cap.type == ObjType_Null);

    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_vnode_cte->cap;
    mapping_handler_t handler_func = handler[dest_cap->type];

    assert(handler_func != NULL);

    cslot_t last_slot = dest_slot + pte_count;

    // TODO: PAE
    if (last_slot > X86_32_PTABLE_SIZE) {
        // requested map overlaps leaf page table
        debug(SUBSYS_CAPS,
                "caps_copy_to_vnode: requested mapping spans multiple leaf page tables\n");
        return SYS_ERR_VM_RETRY_SINGLE;
    }

    errval_t r = handler_func(dest_cap, dest_slot, src_cap, flags, offset,
                              pte_count, mapping_cte);
    if (err_is_fail(r)) {
        assert(mapping_cte->cap.type == ObjType_Null);
        debug(SUBSYS_PAGING, "caps_copy_to_vnode: handler func returned %d\n", r);
        return r;
    }

    /* insert mapping cap into mdb */
    errval_t err = mdb_insert(mapping_cte);
    if (err_is_fail(err)) {
        printk(LOG_ERR, "%s: mdb_insert: %"PRIuERRV"\n", __FUNCTION__, err);
    }

    TRACE_CAP_MSG("created", mapping_cte);

    return err;
}

size_t do_unmap(lvaddr_t pt, cslot_t slot, size_t num_pages)
{
    size_t unmapped_pages = 0;
    union x86_32_ptable_entry *ptentry = (union x86_32_ptable_entry *)pt + slot;
    for (int i = 0; i < num_pages; i++) {
        ptentry++->raw = 0;
        unmapped_pages++;
    }
    return unmapped_pages;
}

errval_t page_mappings_modify_flags(struct capability *mapping, size_t offset,
                                    size_t pages, size_t mflags, genvaddr_t va_hint)
{
    assert(type_is_mapping(mapping->type));
    struct Frame_Mapping *info = &mapping->u.frame_mapping;

    /* Calculate page access protection flags */
    // Get frame cap rights
    paging_x86_32_flags_t flags =
        paging_x86_32_cap_to_page_flags(info->cap->rights);
    // Mask with provided access rights mask
    flags = paging_x86_32_mask_attrs(flags, X86_32_PTABLE_ACCESS(mflags));
    // Add additional arch-specific flags
    flags |= X86_32_PTABLE_FLAGS(mflags);
    // Unconditionally mark the page present
    flags |= X86_32_PTABLE_PRESENT;

    // check arguments
    if (offset >= X86_32_PTABLE_SIZE) { // Within pagetable
        return SYS_ERR_VNODE_SLOT_INVALID;
    }
    if (offset + pages > X86_32_PTABLE_SIZE) { // mapping size ok
        return SYS_ERR_VM_MAP_SIZE;
    }

    /* Calculate location of page table entries we need to modify */
    lvaddr_t base = local_phys_to_mem(info->pte) +
        offset * sizeof(union x86_32_ptable_entry);

    // get pt cap to figure out page size
    struct cte *leaf_pt;
    errval_t err;
    err = mdb_find_cap_for_address(info->pte, &leaf_pt);
    if (err_is_fail(err)) {
        return err;
    }

    size_t pagesize = BASE_PAGE_SIZE;
    switch(leaf_pt->cap.type) {
        case ObjType_VNode_x86_32_ptable :
            for (int i = 0; i < pages; i++) {
                union x86_32_ptable_entry *entry =
                    (union x86_32_ptable_entry *)base + i;
                paging_x86_32_modify_flags(entry, flags);
            }
            break;
        case ObjType_VNode_x86_32_pdir :
            for (int i = 0; i < pages; i++) {
                union x86_32_ptable_entry *entry =
                    (union x86_32_ptable_entry *)base + i;
                paging_x86_32_modify_flags_large(entry, flags);
            }
            pagesize = LARGE_PAGE_SIZE;
            break;
        default:
            return SYS_ERR_WRONG_MAPPING;
    }

    if (va_hint != 0 && va_hint > BASE_PAGE_SIZE) {
        // use as direct hint
        // invlpg should work for large/huge pages
        for (int i = 0; i < pages; i++) {
            do_one_tlb_flush(va_hint + i * pagesize);
        }
    } else {
        /* do full TLB flush */
        do_full_tlb_flush();
    }
    return SYS_ERR_OK;
}

void paging_dump_tables(struct dcb *dispatcher)
{
    if (dispatcher->vspace > X86_32_PADDR_SPACE_LIMIT) {
        printk(LOG_ERR, "dispatcher->vspace = 0x%"PRIxLPADDR": too high!\n" ,
               dispatcher->vspace);
        return;
    }
    lvaddr_t root_pt = local_phys_to_mem(dispatcher->vspace);

#ifdef CONFIG_PAE
    // loop over pdpt entries
    for (int pdir_index = 0; pdir_index < X86_32_PDPTE_SIZE; pdir_index++) {
        // get pdir
        union x86_32_pdpte_entry *pdir = (union x86_32_pdpte_entry *)root_pt + pdir_index;
        if (!pdir->raw) { continue; }
        genpaddr_t pdir_gp = pdir->d.base_addr << BASE_PAGE_BITS;
        lvaddr_t pdir_lv = local_phys_to_mem(gen_phys_to_local_phys(pdir_gp));
#else
        int pdir_index = 0;
        lvaddr_t pdir_lv = root_pt;
#endif

        // only go to 512 because upper half of address space is kernel space
        // (1:1 mapped)
        // TODO: figure out what we need to do here for PAE
        for (int ptable_index = 0; ptable_index < 512; ptable_index++) {
            // get ptable
            union x86_32_pdir_entry *ptable = (union x86_32_pdir_entry *)pdir_lv + ptable_index;
            union x86_32_ptable_entry *large = (union x86_32_ptable_entry *)ptable;
            if (!ptable->raw) { continue; }
            if (large->large.always1) {
                // large page
                genpaddr_t paddr = large->large.base_addr << X86_32_LARGE_PAGE_BITS;
                printf("%d.%d: 0x%"PRIxGENPADDR"\n", pdir_index,
                        ptable_index, paddr);
            }
            genpaddr_t ptable_gp = ptable->d.base_addr << BASE_PAGE_BITS;
            lvaddr_t ptable_lv = local_phys_to_mem(gen_phys_to_local_phys(ptable_gp));

            for (int entry = 0; entry < X86_32_PTABLE_SIZE; entry++) {
                union x86_32_ptable_entry *e =
                    (union x86_32_ptable_entry *)ptable_lv + entry;
                genpaddr_t paddr = (genpaddr_t)e->base.base_addr << BASE_PAGE_BITS;
                if (!paddr) {
                    continue;
                }
                printf("%d.%d.%d: 0x%"PRIxGENPADDR"\n", pdir_index, ptable_index, entry, paddr);
            }
        }
#ifdef CONFIG_PAE
    } // endfor PDPT entries
#endif
}
