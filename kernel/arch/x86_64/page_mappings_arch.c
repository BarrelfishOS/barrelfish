/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <target/x86_64/paging_kernel_target.h>
#include <target/x86_64/offsets_target.h>
#include <mdb/mdb_tree.h>
#include <string.h>

/// Map within a x86_64 pml4
static errval_t x86_64_pml4(struct capability *dest, cslot_t slot,
                            struct capability *src, uintptr_t param1,
                            uintptr_t param2, lvaddr_t *pte)
{
    if (slot >= X86_64_PTABLE_SIZE) { // Within pagetable
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (src->type != ObjType_VNode_x86_64_pdpt) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    if(slot >= X86_64_PML4_BASE(X86_64_MEMORY_OFFSET)) { // Kernel mapped here
        return SYS_ERR_VNODE_SLOT_RESERVED;
    }

    // Destination
    genpaddr_t dest_gp   = dest->u.vnode_x86_64_pml4.base;
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    union x86_64_pdir_entry *entry = (union x86_64_pdir_entry *)dest_lv + slot;
    // assign output param
    *pte = dest_lp + slot * sizeof(union x86_64_ptable_entry);

    if (X86_64_IS_PRESENT(entry)) {
        return SYS_ERR_VNODE_SLOT_INUSE;
    }

    // Source
    genpaddr_t src_gp   = src->u.vnode_x86_64_pdpt.base;
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    paging_x86_64_map_table(entry, src_lp);

    return SYS_ERR_OK;
}

/// Map within a x86_64 pdpt
static errval_t x86_64_pdpt(struct capability *dest, cslot_t slot,
                            struct capability *src, uintptr_t param1,
                            uintptr_t param2, lvaddr_t *pte)
{
    if (slot >= X86_64_PTABLE_SIZE) { // Within pagetable
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (src->type != ObjType_VNode_x86_64_pdir) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    // Destination
    genpaddr_t dest_gp   = dest->u.vnode_x86_64_pdpt.base;
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    union x86_64_pdir_entry *entry = (union x86_64_pdir_entry *)dest_lv + slot;
    // assign output param
    *pte = dest_lp + slot * sizeof(union x86_64_ptable_entry);

    if (X86_64_IS_PRESENT(entry)) {
        return SYS_ERR_VNODE_SLOT_INUSE;
    }

    // Source
    genpaddr_t src_gp   = src->u.vnode_x86_64_pdir.base;
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    paging_x86_64_map_table(entry, src_lp);

    return SYS_ERR_OK;
}

/// Map within a x86_64 pdir
static errval_t x86_64_pdir(struct capability *dest, cslot_t slot,
                            struct capability *src, uintptr_t param1,
                            uintptr_t param2, lvaddr_t *pte)
{
    if (slot >= X86_64_PTABLE_SIZE) { // Within pagetable
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (src->type != ObjType_VNode_x86_64_ptable) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    // Destination
    genpaddr_t dest_gp   = dest->u.vnode_x86_64_pdir.base;
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    union x86_64_pdir_entry *entry = (union x86_64_pdir_entry *)dest_lv + slot;
    // assign output param
    *pte = dest_lp + slot * sizeof(union x86_64_ptable_entry);

    if (X86_64_IS_PRESENT(entry)) {
        return SYS_ERR_VNODE_SLOT_INUSE;
    }

    // Source
    genpaddr_t src_gp   = src->u.vnode_x86_64_ptable.base;
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    paging_x86_64_map_table(entry, src_lp);

    return SYS_ERR_OK;
}

/// Map within a x86_64 ptable
static errval_t x86_64_ptable(struct capability *dest, cslot_t slot,
                              struct capability *src, uintptr_t param1,
                              uintptr_t param2, lvaddr_t *pte)
{
    if (slot >= X86_64_PTABLE_SIZE) { // Within pagetable
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (src->type != ObjType_Frame &&
        src->type != ObjType_DevFrame) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    // check offset within frame
    genpaddr_t offset = param2;
    if (offset + X86_64_BASE_PAGE_SIZE > ((genpaddr_t)1 << src->u.frame.bits)) {
        return SYS_ERR_FRAME_OFFSET_INVALID;
    }


    /* Calculate page access protection flags */
    // Get frame cap rights
    paging_x86_64_flags_t flags =
        paging_x86_64_cap_to_page_flags(src->rights);
    // Mask with provided access rights mask
    flags = paging_x86_64_mask_attrs(flags, X86_64_PTABLE_ACCESS(param1));
    // Add additional arch-specific flags
    flags |= X86_64_PTABLE_FLAGS(param1);
    // Unconditionally mark the page present
    flags |= X86_64_PTABLE_PRESENT;

    // Destination
    genpaddr_t dest_gp   = dest->u.vnode_x86_64_ptable.base;
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    union x86_64_ptable_entry *entry =
        (union x86_64_ptable_entry *)dest_lv + slot;
    // assign output param
    *pte = dest_lp + slot * sizeof(union x86_64_ptable_entry);

    /* FIXME: Flush TLB if the page is already present
     * in the meantime, since we don't do this, we just fail to avoid
     * ever reusing a VA mapping */
    if (X86_64_IS_PRESENT(entry)) {
        debug(LOG_WARN, "Trying to remap an already-present page is NYI, but "
              "this is most likely a user-space bug!\n");
        return SYS_ERR_VNODE_SLOT_INUSE;
    }

    // Carry out the page mapping
    genpaddr_t src_gp   = src->u.frame.base + offset;
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    paging_x86_64_map(entry, src_lp, flags);

    return SYS_ERR_OK;
}

typedef errval_t (*mapping_handler_t)(struct capability *dest_cap,
                                      cslot_t dest_slot,
                                      struct capability *src_cap,
                                      uintptr_t param1, uintptr_t param2, lvaddr_t *pte);

/// Dispatcher table for the type of mapping to create
static mapping_handler_t handler[ObjType_Num] = {
    [ObjType_VNode_x86_64_pml4]   = x86_64_pml4,
    [ObjType_VNode_x86_64_pdpt]   = x86_64_pdpt,
    [ObjType_VNode_x86_64_pdir]   = x86_64_pdir,
    [ObjType_VNode_x86_64_ptable] = x86_64_ptable,
};


#define DIAGNOSTIC_ON_ERROR 1
#define RETURN_ON_ERROR 1

/// Create page mappings
errval_t caps_copy_to_vnode(struct cte *dest_vnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, uintptr_t param1,
                            uintptr_t param2)
{
    assert(type_is_vnode(dest_vnode_cte->cap.type));

    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_vnode_cte->cap;
    mapping_handler_t handler_func = handler[dest_cap->type];

    assert(handler_func != NULL);
    lvaddr_t pte;

    size_t mapped_pages = src_cte->mapping_info.mapped_pages;

    if (mapped_pages >= src_cte->mapping_info.page_count) {
        // exceeded allowed page count for this mapping
#if DIAGNOSTIC_ON_ERROR
        printf("caps_copy_to_vnode: exceeded allowed page count for this mapping\n");
        printf("                    mapped_pages = %zd, mapping_info.page_count = %zd\n",
                mapped_pages, src_cte->mapping_info.page_count);
#endif
#if RETURN_ON_ERROR
        return SYS_ERR_VM_MAP_SIZE;
#endif
    }

    if ((param2 - src_cte->mapping_info.offset)/BASE_PAGE_SIZE >= src_cte->mapping_info.page_count) {
        // requested map offset exceeds mapping region
#if DIAGNOSTIC_ON_ERROR
        printf("caps_copy_to_vnode: requested map offset exceeds mapping region\n");
        printf("                    offset = %zd, mapping_info.offset = %zd, page_count = %zd",
                param2, src_cte->mapping_info.offset,
                src_cte->mapping_info.page_count);
#endif
#if RETURN_ON_ERROR
        return SYS_ERR_VM_MAP_OFFSET;
#endif
    }

    errval_t r = handler_func(dest_cap, dest_slot, src_cap, param1, param2, &pte);
    if (err_is_ok(r)) {
        // set current pte as mapping pte if not set already
        if (src_cte->mapping_info.pte == 0) {
            src_cte->mapping_info.pte = pte;
            src_cte->mapping_info.pt_slot = dest_slot;
        }
        else if (dest_slot == 0 && src_cte->mapping_info.pt2 == 0) {
            // if dest_slot zero and pte set, assume we crossed leaf ptable
            // boundaries, and store pte in mapping_info.pte2 (if not set already)
            src_cte->mapping_info.pt2 = pte;
        }
        else if (dest_slot == 0) {
            printf("mapping uses more than 3 leaf ptables, expect unmap badness\n");
        }
        src_cte->mapping_info.mapped_pages += 1;
    }
    return r;
}

errval_t page_mappings_unmap(struct capability *pgtable, size_t slot)
{
    assert(type_is_vnode(pgtable->type));

    // XXX: temporary
    genpaddr_t paddr;

    switch (pgtable->type) {
    case ObjType_VNode_x86_64_pml4: {
        genpaddr_t gp = pgtable->u.vnode_x86_64_pml4.base;
        lpaddr_t   lp = gen_phys_to_local_phys(gp);
        lvaddr_t   lv = local_phys_to_mem(lp);
        union x86_64_pdir_entry *entry =
            (union x86_64_pdir_entry *)lv + slot;
        paddr = entry->d.base_addr << BASE_PAGE_BITS;
        entry->raw = X86_64_PTABLE_CLEAR;
        break;
    }
    case ObjType_VNode_x86_64_pdpt: {
        genpaddr_t gp = pgtable->u.vnode_x86_64_pdpt.base;
        lpaddr_t   lp = gen_phys_to_local_phys(gp);
        lvaddr_t   lv = local_phys_to_mem(lp);
        union x86_64_pdir_entry *entry =
            (union x86_64_pdir_entry *)lv + slot;
        paddr = entry->d.base_addr << BASE_PAGE_BITS;
        entry->raw = X86_64_PTABLE_CLEAR;
        break;
    }
    case ObjType_VNode_x86_64_pdir: {
        genpaddr_t gp = pgtable->u.vnode_x86_64_pdir.base;
        lpaddr_t   lp = gen_phys_to_local_phys(gp);
        lvaddr_t   lv = local_phys_to_mem(lp);
        union x86_64_pdir_entry *entry =
            (union x86_64_pdir_entry *)lv + slot;
        paddr = entry->d.base_addr << BASE_PAGE_BITS;
        entry->raw = X86_64_PTABLE_CLEAR;
        break;
    }
    case ObjType_VNode_x86_64_ptable: {
        genpaddr_t gp = pgtable->u.vnode_x86_64_ptable.base;
        lpaddr_t   lp = gen_phys_to_local_phys(gp);
        lvaddr_t   lv = local_phys_to_mem(lp);
        union x86_64_ptable_entry *entry =
            (union x86_64_ptable_entry *)lv + slot;
        paddr = entry->base.base_addr << BASE_PAGE_BITS;
        entry->raw = X86_64_PTABLE_CLEAR;
        break;
    }
    default:
        assert(!"Should not get here");
    }

    // XXX: temporary
    // TODO: fix lookup to choose correct cap
    struct cte *mem;
    errval_t err = mdb_find_cap_for_address(paddr, &mem);
    if (err_is_fail(err)) {
        return err;
    }
    // clear mapping info
    memset(&mem->mapping_info, 0, sizeof(struct mapping_info));

    // XXX: FIXME: Going to reload cr3 to flush the entire TLB.
    // This is inefficient.
    // The current implementation is also not multicore safe.
    // We should only invalidate the affected entry using invlpg
    // and figure out which remote tlbs to flush.
    uint64_t cr3;
    __asm__ __volatile__("mov %%cr3,%0" : "=a" (cr3) : );
    __asm__ __volatile__("mov %0,%%cr3" :  : "a" (cr3));

    return SYS_ERR_OK;
}
