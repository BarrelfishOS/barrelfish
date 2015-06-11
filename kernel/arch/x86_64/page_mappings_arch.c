/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010-2013 ETH Zurich.
 * Copyright (c) 2014, HP Labs.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <target/x86_64/paging_kernel_target.h>
#include <target/x86_64/offsets_target.h>
#include <paging_kernel_arch.h>
#include <mdb/mdb_tree.h>
#include <string.h>
#include <barrelfish_kpi/init.h>
#include <cap_predicates.h>

#ifdef __k1om__
#include <target/k1om/offsets_target.h>
#define MEMORY_OFFSET K1OM_MEMORY_OFFSET
#else
#include <target/x86_64/offsets_target.h>
#define MEMORY_OFFSET X86_64_MEMORY_OFFSET
#endif

/// Map within a x86_64 non leaf ptable
static errval_t x86_64_non_ptable(struct capability *dest, cslot_t slot,
                                  struct capability *src, uintptr_t flags,
                                  uintptr_t offset, size_t pte_count)
{
    //printf("page_mappings_arch:x86_64_non_ptable\n");
    if (slot >= X86_64_PTABLE_SIZE) { // Within pagetable
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (type_is_vnode(src->type) && pte_count != 1) { // only allow single ptable mappings
        printf("src type and count mismatch\n");
        return SYS_ERR_VM_MAP_SIZE;
    }

    if (slot + pte_count > X86_64_PTABLE_SIZE) { // mapping size ok
        printf("mapping size invalid (%zd)\n", pte_count);
        return SYS_ERR_VM_MAP_SIZE;
    }

    size_t page_size = 0;
    paging_x86_64_flags_t flags_large = 0;
    switch (dest->type) {
        case ObjType_VNode_x86_64_pml4:
            if (src->type != ObjType_VNode_x86_64_pdpt) { // Right mapping
                printf("src type invalid\n");
                return SYS_ERR_WRONG_MAPPING;
            }
            if(slot >= X86_64_PML4_BASE(MEMORY_OFFSET)) { // Kernel mapped here
                return SYS_ERR_VNODE_SLOT_RESERVED;
            }
            break;
        case ObjType_VNode_x86_64_pdpt:
            // huge page support
            if (src->type != ObjType_VNode_x86_64_pdir) { // Right mapping
                // TODO: check if the system allows 1GB mappings
                page_size = X86_64_HUGE_PAGE_SIZE;
                // check offset within frame
                genpaddr_t off = offset;

                if (off + pte_count * X86_64_HUGE_PAGE_SIZE > get_size(src)) {
                    return SYS_ERR_FRAME_OFFSET_INVALID;
                }
                // Calculate page access protection flags /
                // Get frame cap rights
                flags_large = paging_x86_64_cap_to_page_flags(src->rights);
                // Mask with provided access rights mask
                flags_large = paging_x86_64_mask_attrs(flags, X86_64_PTABLE_ACCESS(flags));
                // Add additional arch-specific flags
                flags_large |= X86_64_PTABLE_FLAGS(flags);
                // Unconditionally mark the page present
                flags_large |= X86_64_PTABLE_PRESENT;
            }
            break;
        case ObjType_VNode_x86_64_pdir:
            // superpage support
            if (src->type != ObjType_VNode_x86_64_ptable) { // Right mapping
                page_size = X86_64_LARGE_PAGE_SIZE;

                // check offset within frame
                genpaddr_t off = offset;

                if (off + pte_count * X86_64_LARGE_PAGE_SIZE > get_size(src)) {
                    return SYS_ERR_FRAME_OFFSET_INVALID;
                }
                // Calculate page access protection flags /
                // Get frame cap rights
                flags_large = paging_x86_64_cap_to_page_flags(src->rights);
                // Mask with provided access rights mask
                flags_large = paging_x86_64_mask_attrs(flags, X86_64_PTABLE_ACCESS(flags));
                // Add additional arch-specific flags
                flags_large |= X86_64_PTABLE_FLAGS(flags);
                // Unconditionally mark the page present
                flags_large |= X86_64_PTABLE_PRESENT;

            }
            break;
        default:
            printf("dest type invalid\n");
            return SYS_ERR_DEST_TYPE_INVALID;
    }

    // Convert destination base address
    genpaddr_t dest_gp   = get_address(dest);
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    // Convert source base address
    genpaddr_t src_gp   = get_address(src);
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);

    // set metadata
    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte = dest_lp + slot * sizeof(union x86_64_ptable_entry);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.offset = offset;

    cslot_t last_slot = slot + pte_count;
    for (; slot < last_slot; slot++, offset += page_size) {
        // Destination
        union x86_64_pdir_entry *entry = (union x86_64_pdir_entry *)dest_lv + slot;

        if (X86_64_IS_PRESENT(entry)) {
            // cleanup mapping info
            // TODO: cleanup already mapped pages
            memset(&src_cte->mapping_info, 0, sizeof(struct mapping_info));
            printf("slot in use\n");
            return SYS_ERR_VNODE_SLOT_INUSE;
        }

        // determine if we map a large/huge page or a normal entry
        if (page_size == X86_64_LARGE_PAGE_SIZE)
        {
            //a large page is mapped
            paging_x86_64_map_large((union x86_64_ptable_entry *)entry, src_lp + offset, flags_large);
        } else if (page_size == X86_64_HUGE_PAGE_SIZE) {
            // a huge page is mapped
            paging_x86_64_map_huge((union x86_64_ptable_entry *)entry, src_lp + offset, flags_large);
        } else {
            //a normal paging structure entry is mapped
            paging_x86_64_map_table(entry, src_lp + offset);
        }
    }

    return SYS_ERR_OK;
}

/// Map within a x86_64 ptable
static errval_t x86_64_ptable(struct capability *dest, cslot_t slot,
                              struct capability *src, uintptr_t mflags,
                              uintptr_t offset, size_t pte_count)
{
    //printf("page_mappings_arch:x86_64_ptable\n");
    if (slot >= X86_64_PTABLE_SIZE) { // Within pagetable
        printf("    vnode_invalid\n");
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (slot + pte_count > X86_64_PTABLE_SIZE) { // mapping size ok
        printf("mapping size invalid (%zd)\n", pte_count);
        return SYS_ERR_VM_MAP_SIZE;
    }

    if (src->type != ObjType_Frame &&
        src->type != ObjType_DevFrame) { // Right mapping
        printf("src type invalid\n");
        return SYS_ERR_WRONG_MAPPING;
    }

    // check offset within frame
    genpaddr_t off = offset;
    if (off + pte_count * X86_64_BASE_PAGE_SIZE > get_size(src)) {
        printf("frame offset invalid\n");
        return SYS_ERR_FRAME_OFFSET_INVALID;
    }


    /* Calculate page access protection flags */
    // Get frame cap rights
    paging_x86_64_flags_t flags =
        paging_x86_64_cap_to_page_flags(src->rights);
    // Mask with provided access rights mask
    flags = paging_x86_64_mask_attrs(flags, X86_64_PTABLE_ACCESS(mflags));
    // Add additional arch-specific flags
    flags |= X86_64_PTABLE_FLAGS(mflags);
    // Unconditionally mark the page present
    flags |= X86_64_PTABLE_PRESENT;

    // Convert destination base address
    genpaddr_t dest_gp   = get_address(dest);
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    // Convert source base address
    genpaddr_t src_gp   = get_address(src);
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    // Set metadata
    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte = dest_lp + slot * sizeof(union x86_64_ptable_entry);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.offset = offset;

    cslot_t last_slot = slot + pte_count;
    for (; slot < last_slot; slot++, offset += X86_64_BASE_PAGE_SIZE) {
        union x86_64_ptable_entry *entry =
            (union x86_64_ptable_entry *)dest_lv + slot;

        /* FIXME: Flush TLB if the page is already present
         * in the meantime, since we don't do this, we just fail to avoid
         * ever reusing a VA mapping */
        if (X86_64_IS_PRESENT(entry)) {
            // TODO: cleanup already mapped pages
            memset(&src_cte->mapping_info, 0, sizeof(struct mapping_info));
            debug(LOG_WARN, "Trying to remap an already-present page is NYI, but "
                  "this is most likely a user-space bug!\n");
            return SYS_ERR_VNODE_SLOT_INUSE;
        }

        // Carry out the page mapping
        paging_x86_64_map(entry, src_lp + offset, flags);
    }

    return SYS_ERR_OK;
}

typedef errval_t (*mapping_handler_t)(struct capability *dest_cap,
                                      cslot_t dest_slot,
                                      struct capability *src_cap,
                                      uintptr_t flags, uintptr_t offset,
                                      size_t pte_count);

/// Dispatcher table for the type of mapping to create
static mapping_handler_t handler[ObjType_Num] = {
    [ObjType_VNode_x86_64_pml4]   = x86_64_non_ptable,
    [ObjType_VNode_x86_64_pdpt]   = x86_64_non_ptable,
    [ObjType_VNode_x86_64_pdir]   = x86_64_non_ptable,
    [ObjType_VNode_x86_64_ptable] = x86_64_ptable,
};


#define DIAGNOSTIC_ON_ERROR 1
#define RETURN_ON_ERROR 1

/// Create page mappings
errval_t caps_copy_to_vnode(struct cte *dest_vnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count)
{
    assert(type_is_vnode(dest_vnode_cte->cap.type));

    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_vnode_cte->cap;
    mapping_handler_t handler_func = handler[dest_cap->type];

    assert(handler_func != NULL);

#if 0
    genpaddr_t paddr = get_address(&src_cte->cap) + offset;
    genvaddr_t vaddr;
    compile_vaddr(dest_vnode_cte, dest_slot, &vaddr);
    printf("mapping 0x%"PRIxGENPADDR" to 0x%"PRIxGENVADDR"\n", paddr, vaddr);
#endif

    if (src_cte->mapping_info.pte) {
        // this cap is already mapped
#if DIAGNOSTIC_ON_ERROR
        printf("caps_copy_to_vnode: this copy is already mapped @pte 0x%lx (paddr = 0x%"PRIxGENPADDR")\n", src_cte->mapping_info.pte, get_address(src_cap));
#endif
#if RETURN_ON_ERROR
        return SYS_ERR_VM_ALREADY_MAPPED;
#endif
    }

    cslot_t last_slot = dest_slot + pte_count;

    if (last_slot > X86_64_PTABLE_SIZE) {
        // requested map overlaps leaf page table
#if DIAGNOSTIC_ON_ERROR
        printf("caps_copy_to_vnode: requested mapping spans multiple leaf page tables\n");
#endif
#if RETURN_ON_ERROR
        return SYS_ERR_VM_RETRY_SINGLE;
#endif
    }

    errval_t r = handler_func(dest_cap, dest_slot, src_cap, flags, offset, pte_count);
    if (err_is_fail(r)) {
        printf("caps_copy_to_vnode: handler func returned %ld\n", r);
    }
#if 0
    else {
        printf("mapping_info.pte       = 0x%lx\n", src_cte->mapping_info.pte);
        printf("mapping_info.offset    = 0x%lx\n", src_cte->mapping_info.offset);
        printf("mapping_info.pte_count = %zu\n", src_cte->mapping_info.pte_count);
    }
#endif
    return r;
}

static inline void read_pt_entry(struct capability *pgtable, size_t slot,
                                 genpaddr_t *mapped_addr, lpaddr_t *pte,
                                 void **entry)
{
    assert(type_is_vnode(pgtable->type));

    genpaddr_t paddr;
    lpaddr_t pte_;
    void *entry_;

    genpaddr_t gp = get_address(pgtable);
    lpaddr_t lp = gen_phys_to_local_phys(gp);
    lvaddr_t lv = local_phys_to_mem(lp);

    // get paddr
    switch (pgtable->type) {
    case ObjType_VNode_x86_64_pml4:
    case ObjType_VNode_x86_64_pdpt:
    case ObjType_VNode_x86_64_pdir: {
        union x86_64_pdir_entry *e =
            (union x86_64_pdir_entry *)lv + slot;
        paddr = (lpaddr_t)e->d.base_addr << BASE_PAGE_BITS;
        entry_ = e;
        pte_ = lp + slot * sizeof(union x86_64_pdir_entry);
        break;
    }
    case ObjType_VNode_x86_64_ptable: {
        union x86_64_ptable_entry *e =
            (union x86_64_ptable_entry *)lv + slot;
        paddr = (lpaddr_t)e->base.base_addr << BASE_PAGE_BITS;
        entry_ = e;
        pte_ = lp + slot * sizeof(union x86_64_ptable_entry);
        break;
    }
    default:
        assert(!"Should not get here");
    }

    if (mapped_addr) {
        *mapped_addr = paddr;
    }
    if (pte) {
        *pte = pte_;
    }
    if (entry) {
        *entry = entry_;
    }
}

__attribute__((unused))
static inline lvaddr_t get_leaf_ptable_for_vaddr(genvaddr_t vaddr)
{
    lvaddr_t root_pt = local_phys_to_mem(dcb_current->vspace);

    // get pdpt
    union x86_64_pdir_entry *pdpt = (union x86_64_pdir_entry *)root_pt + X86_64_PML4_BASE(vaddr);
    if (!pdpt->raw) { return 0; }
    genpaddr_t pdpt_gp = pdpt->d.base_addr << BASE_PAGE_BITS;
    lvaddr_t pdpt_lv = local_phys_to_mem(gen_phys_to_local_phys(pdpt_gp));
    // get pdir
    union x86_64_pdir_entry *pdir = (union x86_64_pdir_entry *)pdpt_lv + X86_64_PDPT_BASE(vaddr);
    if (!pdir->raw) { return 0; }
    genpaddr_t pdir_gp = pdir->d.base_addr << BASE_PAGE_BITS;
    lvaddr_t pdir_lv = local_phys_to_mem(gen_phys_to_local_phys(pdir_gp));
    // get ptable
    union x86_64_ptable_entry *ptable = (union x86_64_ptable_entry *)pdir_lv + X86_64_PDIR_BASE(vaddr);
    if (!ptable->raw) { return 0; }
    genpaddr_t ptable_gp = ptable->base.base_addr << BASE_PAGE_BITS;
    lvaddr_t ptable_lv = local_phys_to_mem(gen_phys_to_local_phys(ptable_gp));

    return ptable_lv;
}

size_t do_unmap(lvaddr_t pt, cslot_t slot, size_t num_pages)
{
    // iterate over affected leaf ptables
    size_t unmapped_pages = 0;
    union x86_64_ptable_entry *ptentry = (union x86_64_ptable_entry *)pt + slot;
    for (int i = 0; i < num_pages; i++) {
        ptentry++->raw = 0;
        unmapped_pages++;
    }
    return unmapped_pages;
}

errval_t page_mappings_unmap(struct capability *pgtable, struct cte *mapping,
                             size_t slot, size_t num_pages)
{
    assert(type_is_vnode(pgtable->type));
    errval_t err;
    debug(SUBSYS_PAGING, "page_mappings_unmap(%zd pages)\n", num_pages);

    // get page table entry data
    genpaddr_t paddr;

    read_pt_entry(pgtable, slot, &paddr, NULL, NULL);
    lvaddr_t pt = local_phys_to_mem(gen_phys_to_local_phys(get_address(pgtable)));

    // get virtual address of first page
    // TODO: error checking
    genvaddr_t vaddr;
    bool tlb_flush_necessary = true;
    struct cte *leaf_pt = cte_for_cap(pgtable);
    err = compile_vaddr(leaf_pt, slot, &vaddr);
    if (err_is_fail(err)) {
        if (err_no(err) == SYS_ERR_VNODE_NOT_INSTALLED) {
            debug(SUBSYS_PAGING, "couldn't reconstruct virtual address\n");
        } else if (err_no(err) == SYS_ERR_VNODE_SLOT_INVALID
                   && leaf_pt->mapping_info.pte == 0) {
            debug(SUBSYS_PAGING, "unmapping in floating page table; not flushing TLB\n");
            tlb_flush_necessary = false;
        } else {
            return err;
        }
    }

    if (num_pages != mapping->mapping_info.pte_count) {
        // want to unmap a different amount of pages than was mapped
        return SYS_ERR_VM_MAP_SIZE;
    }

    do_unmap(pt, slot, num_pages);

    // flush TLB for unmapped pages if we got a valid virtual address
    // TODO: heuristic that decides if selective or full flush is more
    //       efficient?
    if (tlb_flush_necessary) {
        if (num_pages > 1 || err_is_fail(err)) {
            do_full_tlb_flush();
        } else {
            do_one_tlb_flush(vaddr);
        }
    }

    // update mapping info
    memset(&mapping->mapping_info, 0, sizeof(struct mapping_info));

    return SYS_ERR_OK;
}

/**
 * \brief modify flags of mapping for `frame`.
 *
 * \arg frame the frame whose mapping should be modified
 * \arg offset the offset from the first page table entry in entries
 * \arg pages the number of pages to modify
 * \arg mflags the new flags
 * \arg va_hint a user-supplied virtual address for hinting selective TLB
 *              flushing
 */
errval_t page_mappings_modify_flags(struct capability *frame, size_t offset,
                                    size_t pages, size_t mflags, genvaddr_t va_hint)
{
    struct cte *mapping = cte_for_cap(frame);
    struct mapping_info *info = &mapping->mapping_info;
    struct cte *leaf_pt;
    errval_t err;
    err = mdb_find_cap_for_address(info->pte, &leaf_pt);
    if (err_is_fail(err)) {
        return err;
    }

    /* Calculate page access protection flags */
    // Get frame cap rights
    paging_x86_64_flags_t flags =
        paging_x86_64_cap_to_page_flags(frame->rights);
    // Mask with provided access rights mask
    flags = paging_x86_64_mask_attrs(flags, X86_64_PTABLE_ACCESS(mflags));
    // Add additional arch-specific flags
    flags |= X86_64_PTABLE_FLAGS(mflags);
    // Unconditionally mark the page present
    flags |= X86_64_PTABLE_PRESENT;

    // check arguments
    if (offset >= X86_64_PTABLE_SIZE) { // Within pagetable
        return SYS_ERR_VNODE_SLOT_INVALID;
    }
    if (offset + pages > X86_64_PTABLE_SIZE) { // mapping size ok
        return SYS_ERR_VM_MAP_SIZE;
    }

    /* Calculate location of first pt entry we need to modify */
    lvaddr_t base = local_phys_to_mem(info->pte) +
        offset * sizeof(union x86_64_ptable_entry);

    size_t pagesize = BASE_PAGE_SIZE;
    switch(leaf_pt->cap.type) {
        case ObjType_VNode_x86_64_ptable :
            for (int i = 0; i < pages; i++) {
                union x86_64_ptable_entry *entry =
                    (union x86_64_ptable_entry *)base + i;
                paging_x86_64_modify_flags(entry, flags);
            }
            break;
        case ObjType_VNode_x86_64_pdir :
            for (int i = 0; i < pages; i++) {
                union x86_64_ptable_entry *entry =
                    (union x86_64_ptable_entry *)base + i;
                paging_x86_64_modify_flags_large(entry, flags);
            }
            pagesize = LARGE_PAGE_SIZE;
            break;
        case ObjType_VNode_x86_64_pdpt :
            for (int i = 0; i < pages; i++) {
                union x86_64_ptable_entry *entry =
                    (union x86_64_ptable_entry *)base + i;
                paging_x86_64_modify_flags_huge(entry, flags);
            }
            pagesize = HUGE_PAGE_SIZE;
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
    lvaddr_t root_pt = local_phys_to_mem(dispatcher->vspace);

    // loop over pdpts
    union x86_64_ptable_entry *pt;
    size_t kernel_pml4e = X86_64_PML4_BASE(X86_64_MEMORY_OFFSET);
    for (int pdpt_index = 0; pdpt_index < kernel_pml4e; pdpt_index++) {
        union x86_64_pdir_entry *pdpt = (union x86_64_pdir_entry *)root_pt + pdpt_index;
        if (!pdpt->raw) { continue; }
        genpaddr_t pdpt_gp = pdpt->d.base_addr << BASE_PAGE_BITS;
        lvaddr_t pdpt_lv = local_phys_to_mem(gen_phys_to_local_phys(pdpt_gp));

        for (int pdir_index = 0; pdir_index < X86_64_PTABLE_SIZE; pdir_index++) {
            // get pdir
            union x86_64_pdir_entry *pdir = (union x86_64_pdir_entry *)pdpt_lv + pdir_index;
            pt = (union x86_64_ptable_entry*)pdir;
            if (!pdir->raw) { continue; }
            // check if pdir or huge page
            if (pt->huge.always1) {
                // is huge page mapping
                genpaddr_t paddr = (genpaddr_t)pt->huge.base_addr << HUGE_PAGE_BITS;
                printf("%d.%d: 0x%"PRIxGENPADDR"\n", pdpt_index, pdir_index, paddr);
                // goto next pdpt entry
                continue;
            }
            genpaddr_t pdir_gp = pdir->d.base_addr << BASE_PAGE_BITS;
            lvaddr_t pdir_lv = local_phys_to_mem(gen_phys_to_local_phys(pdir_gp));

            for (int ptable_index = 0; ptable_index < X86_64_PTABLE_SIZE; ptable_index++) {
                // get ptable
                union x86_64_pdir_entry *ptable = (union x86_64_pdir_entry *)pdir_lv + ptable_index;
                pt = (union x86_64_ptable_entry *)ptable;
                if (!ptable->raw) { continue; }
                // check if ptable or large page
                if (pt->large.always1) {
                    // is large page mapping
                    genpaddr_t paddr = (genpaddr_t)pt->large.base_addr << LARGE_PAGE_BITS;
                    printf("%d.%d.%d: 0x%"PRIxGENPADDR"\n", pdpt_index, pdir_index, ptable_index, paddr);
                    // goto next pdir entry
                    continue;
                }
                genpaddr_t ptable_gp = ptable->d.base_addr << BASE_PAGE_BITS;
                lvaddr_t ptable_lv = local_phys_to_mem(gen_phys_to_local_phys(ptable_gp));

                for (int entry = 0; entry < X86_64_PTABLE_SIZE; entry++) {
                    union x86_64_ptable_entry *e =
                        (union x86_64_ptable_entry *)ptable_lv + entry;
                    genpaddr_t paddr = (genpaddr_t)e->base.base_addr << BASE_PAGE_BITS;
                    if (!paddr) {
                        continue;
                    }
                    printf("%d.%d.%d.%d: 0x%"PRIxGENPADDR"\n", pdpt_index, pdir_index, ptable_index, entry, paddr);
                }
            }
        }
    }
}
