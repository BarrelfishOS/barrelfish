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
#include <target/x86_32/paging_kernel_target.h>
#include <target/x86_32/offsets_target.h>

#ifdef CONFIG_PAE
/// Map within a x86_32 pdpt
static errval_t x86_32_pdpt(struct capability *dest, cslot_t slot,
                            struct capability * src, uintptr_t param1,
                            uintptr_t param2)
{
    if (slot >= X86_32_PTABLE_SIZE) { // Slot within page table
        return SYS_ERR_VNODE_SLOT_INVALID;
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
                            struct capability * src, uintptr_t param1,
                            uintptr_t param2)
{
    if (slot >= X86_32_PTABLE_SIZE) { // Slot within page table
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

#ifndef CONFIG_PAE
    if(slot >= X86_32_PDIR_BASE(X86_32_MEMORY_OFFSET)) { // Kernel mapped here
        return SYS_ERR_VNODE_SLOT_RESERVED;
    }
#endif

    if (src->type != ObjType_VNode_x86_32_ptable) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    // Destination
    genpaddr_t dest_gp   = dest->u.vnode_x86_32_pdir.base;
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    union x86_32_pdir_entry *entry =
        (union x86_32_pdir_entry *)dest_lv + slot;

    // Source
    genpaddr_t src_gp   = src->u.vnode_x86_32_pdir.base;
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    paging_x86_32_map_table(entry, src_lp);

    return SYS_ERR_OK;
}

/// Map within a x86_32 ptable
static errval_t x86_32_ptable(struct capability *dest, cslot_t slot,
                              struct capability * src, uintptr_t param1,
                              uintptr_t param2)
{
    if (slot >= X86_32_PTABLE_SIZE) { // Slot within page table
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (src->type != ObjType_Frame &&
        src->type != ObjType_DevFrame) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    // check offset within frame
    genpaddr_t offset = param2;
    if (offset + X86_32_BASE_PAGE_SIZE > ((genpaddr_t)1 << src->u.frame.bits)) {
        return SYS_ERR_FRAME_OFFSET_INVALID;
    }

    /* Calculate page access protection flags */
    // Get frame cap rights
    paging_x86_32_flags_t flags =
        paging_x86_32_cap_to_page_flags(src->rights);
    // Mask with provided access rights mask
    flags = paging_x86_32_mask_attrs(flags, X86_32_PTABLE_ACCESS(param1));
    // Add additional arch-specific flags
    flags |= X86_32_PTABLE_FLAGS(param1);
    // Unconditionally mark the page present
    flags |= X86_32_PTABLE_PRESENT;

    // Destination
    genpaddr_t dest_gp   = dest->u.vnode_x86_32_ptable.base;
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    union x86_32_ptable_entry *entry =
        (union x86_32_ptable_entry *)dest_lv + slot;

    /* FIXME: Flush TLB if the page is already present
     * in the meantime, since we don't do this, we just assert that
     * we never reuse a VA mapping */
    if (X86_32_IS_PRESENT(entry)) {
        panic("Trying to map into an already present page NYI.");
    }

    // Carry out the page mapping
    genpaddr_t src_gp   = src->u.frame.base + offset;
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    paging_x86_32_map(entry, src_lp, flags);

    return SYS_ERR_OK;
}

typedef errval_t (*mapping_handler_t)(struct capability *dest_cap,
                                      cslot_t dest_slot,
                                      struct capability *src_cap,
                                      uintptr_t param1, uintptr_t param2);

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
                            struct cte *src_cte, uintptr_t param1,
                            uintptr_t param2)
{
    assert(type_is_vnode(dest_vnode_cte->cap.type));

    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_vnode_cte->cap;
    mapping_handler_t handler_func = handler[dest_cap->type];

    assert(handler_func != NULL);
    return handler_func(dest_cap, dest_slot, src_cap, param1, param2);
}

errval_t page_mappings_unmap(struct capability *pgtable, size_t slot)
{
    assert(type_is_vnode(pgtable->type));

    switch (pgtable->type) {
    case ObjType_VNode_x86_32_pdpt: {
        genpaddr_t gp = pgtable->u.vnode_x86_32_pdpt.base;
        lpaddr_t   lp = gen_phys_to_local_phys(gp);
        lvaddr_t   lv = local_phys_to_mem(lp);
        union x86_32_pdir_entry *entry =
            (union x86_32_pdir_entry *)lv + slot;
        entry->raw = X86_32_PTABLE_CLEAR;
        break;
    }
    case ObjType_VNode_x86_32_pdir: {
        genpaddr_t gp = pgtable->u.vnode_x86_32_pdir.base;
        lpaddr_t   lp = gen_phys_to_local_phys(gp);
        lvaddr_t   lv = local_phys_to_mem(lp);
        union x86_32_pdir_entry *entry =
            (union x86_32_pdir_entry *)lv + slot;
        entry->raw = X86_32_PTABLE_CLEAR;
        break;
    }
    case ObjType_VNode_x86_32_ptable: {
        genpaddr_t gp = pgtable->u.vnode_x86_32_ptable.base;
        lpaddr_t   lp = gen_phys_to_local_phys(gp);
        lvaddr_t   lv = local_phys_to_mem(lp);
        union x86_32_ptable_entry *entry =
            (union x86_32_ptable_entry *)lv + slot;
        entry->raw = X86_32_PTABLE_CLEAR;
        break;
    }
    default:
        assert(!"Should not get here");
    }

    // XXX: FIXME: Going to reload cr3 to flush the entire TLB.
    // This is inefficient.
    // The current implementation is also not multicore safe.
    // We should only invalidate the affected entry using invlpg
    // and figure out which remote tlbs to flush.
    uint32_t cr3;
    __asm__ __volatile__("mov %%cr3,%0" : "=a" (cr3) : );
    __asm__ __volatile__("mov %0,%%cr3" :  : "a" (cr3));

    return SYS_ERR_OK;
}
