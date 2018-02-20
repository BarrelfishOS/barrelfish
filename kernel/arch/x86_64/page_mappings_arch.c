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
#include <target/x86_64/paging_kernel_target.h>
#include <target/x86_64/offsets_target.h>
#include <paging_kernel_arch.h>
#include <mdb/mdb_tree.h>
#include <string.h>
#include <barrelfish_kpi/init.h>
#include <cap_predicates.h>
#include <paging_generic.h>

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
                                  uintptr_t offset, size_t pte_count,
                                  struct cte *mapping_cte)
{
    //printf("page_mappings_arch:x86_64_non_ptable\n");
    if (slot >= X86_64_PTABLE_SIZE) { // Within pagetable
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (type_is_vnode(src->type) && pte_count != 1) { // only allow single ptable mappings
        debug(SUBSYS_PAGING, "src type and count mismatch\n");
        return SYS_ERR_VM_MAP_SIZE;
    }

    if (slot + pte_count > X86_64_PTABLE_SIZE) { // mapping size ok
        debug(SUBSYS_PAGING, "mapping size invalid (%zd)\n", pte_count);
        return SYS_ERR_VM_MAP_SIZE;
    }

    size_t page_size = 0;
    paging_x86_64_flags_t flags_large = 0;
    switch (dest->type) {
        case ObjType_VNode_x86_64_pml4:
            if (src->type != ObjType_VNode_x86_64_pdpt) { // Right mapping
                debug(SUBSYS_PAGING, "src type invalid: %d\n", src->type);
                return SYS_ERR_WRONG_MAPPING;
            }
            if(slot >= X86_64_PML4_BASE(MEMORY_OFFSET)) { // Kernel mapped here
                return SYS_ERR_VNODE_SLOT_RESERVED;
            }
            break;
        case ObjType_VNode_x86_64_pdpt:
            // huge page support
            if (src->type != ObjType_VNode_x86_64_pdir) { // Right mapping
                if (src->type != ObjType_Frame &&
                    src->type != ObjType_DevFrame) { // Right mapping
                    debug(SUBSYS_PAGING, "src type invalid: %d\n", src->type);
                    return SYS_ERR_WRONG_MAPPING;
                }

                if (get_size(src) < HUGE_PAGE_SIZE) {
                    return SYS_ERR_VM_FRAME_TOO_SMALL;
                }

                if ((get_address(src)+offset) & HUGE_PAGE_MASK) {
                    return SYS_ERR_VM_FRAME_UNALIGNED;
                }

                // TODO: check if the system allows 1GB mappings
                page_size = X86_64_HUGE_PAGE_SIZE;
                // check offset within frame
                genpaddr_t off = offset;

                if (off + pte_count * X86_64_HUGE_PAGE_SIZE > get_size(src)) {
                    printk(LOG_NOTE, "frame offset invalid: %zx > 0x%"PRIxGENSIZE"\n",
                            off + pte_count * X86_64_BASE_PAGE_SIZE, get_size(src));
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
                if (src->type != ObjType_Frame &&
                    src->type != ObjType_DevFrame) { // Right mapping
                    debug(SUBSYS_PAGING, "src type invalid: %d\n", src->type);
                    return SYS_ERR_WRONG_MAPPING;
                }

                if (get_size(src) < LARGE_PAGE_SIZE) {
                    return SYS_ERR_VM_FRAME_TOO_SMALL;
                }

                if ((get_address(src)+offset) & LARGE_PAGE_MASK) {
                    return SYS_ERR_VM_FRAME_UNALIGNED;
                }

                page_size = X86_64_LARGE_PAGE_SIZE;

                // check offset within frame
                genpaddr_t off = offset;

                if (off + pte_count * X86_64_LARGE_PAGE_SIZE > get_size(src)) {
                    printk(LOG_NOTE, "frame offset invalid: %zx > 0x%"PRIxGENSIZE"\n",
                            off + pte_count * X86_64_BASE_PAGE_SIZE, get_size(src));
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
            debug(SUBSYS_PAGING, "dest type invalid\n");
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
    create_mapping_cap(mapping_cte, src, cte_for_cap(dest),
                       slot, pte_count);

    cslot_t last_slot = slot + pte_count;
    for (; slot < last_slot; slot++, offset += page_size) {
        // Destination
        union x86_64_pdir_entry *entry = (union x86_64_pdir_entry *)dest_lv + slot;

        if (X86_64_IS_PRESENT(entry)) {
            // cleanup mapping info
            // TODO: cleanup already mapped pages
            memset(mapping_cte, 0, sizeof(*mapping_cte));
            debug(SUBSYS_PAGING, "slot in use\n");
            printk(LOG_NOTE, "slot = 0x%016"PRIx64"\n", entry->raw);
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
                              uintptr_t offset, size_t pte_count,
                              struct cte *mapping_cte)
{
    //printf("page_mappings_arch:x86_64_ptable\n");
    if (slot >= X86_64_PTABLE_SIZE) { // Within pagetable
        debug(SUBSYS_PAGING, "    vnode_invalid\n");
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (slot + pte_count > X86_64_PTABLE_SIZE) { // mapping size ok
        debug(SUBSYS_PAGING, "mapping size invalid (%zd)\n", pte_count);
        return SYS_ERR_VM_MAP_SIZE;
    }

    if (src->type != ObjType_Frame &&
        src->type != ObjType_DevFrame) { // Right mapping
        debug(SUBSYS_PAGING, "src type invalid\n");
        return SYS_ERR_WRONG_MAPPING;
    }

    // check offset within frame
    genpaddr_t off = offset;
    if (off + pte_count * X86_64_BASE_PAGE_SIZE > get_size(src)) {
        debug(SUBSYS_PAGING, "frame offset invalid: %zx > 0x%"PRIxGENSIZE"\n",
                off + pte_count * X86_64_BASE_PAGE_SIZE, get_size(src));
        printk(LOG_NOTE, "frame offset invalid: %zx > 0x%"PRIxGENSIZE"\n",
                off + pte_count * X86_64_BASE_PAGE_SIZE, get_size(src));
        char buf[256];
        sprint_cap(buf,256,src);
        printk(LOG_NOTE, "src = %s\n", buf);
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
    create_mapping_cap(mapping_cte, src, cte_for_cap(dest),
                       slot, pte_count);

    cslot_t last_slot = slot + pte_count;
    for (; slot < last_slot; slot++, offset += X86_64_BASE_PAGE_SIZE) {
        union x86_64_ptable_entry *entry =
            (union x86_64_ptable_entry *)dest_lv + slot;

        /* FIXME: Flush TLB if the page is already present
         * in the meantime, since we don't do this, we just fail to avoid
         * ever reusing a VA mapping */
        if (X86_64_IS_PRESENT(entry)) {
            // TODO: cleanup already mapped pages
            memset(mapping_cte, 0, sizeof(*mapping_cte));
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
                                      size_t pte_count,
                                      struct cte *mapping_cte);

/// Dispatcher table for the type of mapping to create
static mapping_handler_t handler[ObjType_Num] = {
    [ObjType_VNode_x86_64_pml4]   = x86_64_non_ptable,
    [ObjType_VNode_x86_64_pdpt]   = x86_64_non_ptable,
    [ObjType_VNode_x86_64_pdir]   = x86_64_non_ptable,
    [ObjType_VNode_x86_64_ptable] = x86_64_ptable,
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

#if 0
    genpaddr_t paddr = get_address(&src_cte->cap) + offset;
    genvaddr_t vaddr;
    compile_vaddr(dest_vnode_cte, dest_slot, &vaddr);
    printk(LOG_NOTE, "mapping 0x%"PRIxGENPADDR" to 0x%"PRIxGENVADDR"\n", paddr, vaddr);
#endif

    cslot_t last_slot = dest_slot + pte_count;

    if (last_slot > X86_64_PTABLE_SIZE) {
        // requested map overlaps leaf page table
        debug(SUBSYS_CAPS,
                "caps_copy_to_vnode: requested mapping spans multiple leaf page tables\n");
        return SYS_ERR_VM_RETRY_SINGLE;
    }

    errval_t r = handler_func(dest_cap, dest_slot, src_cap, flags, offset,
                              pte_count, mapping_cte);
    if (err_is_fail(r)) {
        assert(mapping_cte->cap.type == ObjType_Null);
        debug(SUBSYS_PAGING, "caps_copy_to_vnode: handler func returned %ld\n", r);
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
    // iterate over affected leaf ptables
    size_t unmapped_pages = 0;
    union x86_64_ptable_entry *ptentry = (union x86_64_ptable_entry *)pt + slot;
    for (int i = 0; i < num_pages; i++) {
        ptentry++->raw = 0;
        unmapped_pages++;
    }
    return unmapped_pages;
}

static size_t ptable_type_get_page_size(enum objtype type)
{
    switch(type) {
        case ObjType_VNode_x86_64_ptable:
            return BASE_PAGE_SIZE;
        case ObjType_VNode_x86_64_pdir:
            return LARGE_PAGE_SIZE;
        case ObjType_VNode_x86_64_pdpt:
            return HUGE_PAGE_SIZE;
        case ObjType_VNode_x86_64_pml4:
            return 0;

        default:
            assert(!"Type not x86_64 vnode");
    }
    return 0;
}

/**
 * \brief modify flags of entries in `leaf_pt`.
 *
 * \arg leaf_pt the frame whose mapping should be modified
 * \arg offset the offset from the first page table entry in entries
 * \arg pages the number of pages to modify
 * \arg flags the new flags
 * \arg va_hint a user-supplied virtual address for hinting selective TLB
 *              flushing
 */
static errval_t generic_modify_flags(struct cte *leaf_pt, size_t offset,
                                     size_t pages,
                                     paging_x86_64_flags_t flags)
{
    lvaddr_t base = local_phys_to_mem(get_address(&leaf_pt->cap)) +
        offset * sizeof(union x86_64_ptable_entry);

    size_t pagesize = BASE_PAGE_SIZE;
    switch(leaf_pt->cap.type) {
        case ObjType_VNode_x86_64_ptable :
            for (int i = 0; i < pages; i++) {
                union x86_64_ptable_entry *entry =
                    (union x86_64_ptable_entry *)base + i;
                if (entry->base.present) {
                    paging_x86_64_modify_flags(entry, flags);
                }
            }
            break;
        case ObjType_VNode_x86_64_pdir :
            for (int i = 0; i < pages; i++) {
                union x86_64_ptable_entry *entry =
                    (union x86_64_ptable_entry *)base + i;
                if (entry->large.present) {
                    paging_x86_64_modify_flags_large(entry, flags);
                }
            }
            pagesize = LARGE_PAGE_SIZE;
            break;
        case ObjType_VNode_x86_64_pdpt :
            for (int i = 0; i < pages; i++) {
                union x86_64_ptable_entry *entry =
                    (union x86_64_ptable_entry *)base + i;
                if (entry->large.present) {
                    paging_x86_64_modify_flags_huge(entry, flags);
                }
            }
            pagesize = HUGE_PAGE_SIZE;
            break;
        default:
            return SYS_ERR_WRONG_MAPPING;
    }

    return SYS_ERR_OK;
}

/**
 * \brief modify flags of mapping `mapping`.
 *
 * \arg mapping the mapping to modify
 * \arg offset the offset from the first page table entry in entries
 * \arg pages the number of pages to modify
 * \arg flags the new flags
 * \arg va_hint a user-supplied virtual address for hinting selective TLB
 *              flushing
 */
errval_t page_mappings_modify_flags(struct capability *mapping, size_t offset,
                                    size_t pages, size_t mflags, genvaddr_t va_hint)
{
    assert(type_is_mapping(mapping->type));
    struct Frame_Mapping *info = &mapping->u.frame_mapping;

    /* Calculate page access protection flags */
    // Get frame cap rights
    paging_x86_64_flags_t flags =
        paging_x86_64_cap_to_page_flags(info->cap->rights);
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

    // get pt cap to figure out page size
    struct cte *leaf_pt = info->ptable;
    if (!type_is_vnode(leaf_pt->cap.type)) {
        return SYS_ERR_VNODE_TYPE;
    }
    assert(type_is_vnode(leaf_pt->cap.type));

    errval_t err;
    err = generic_modify_flags(leaf_pt, offset, pages, flags);
    if (err_is_fail(err)) {
        return err;
    }

    size_t pagesize = ptable_type_get_page_size(leaf_pt->cap.type);
    if (va_hint != 0 && va_hint > BASE_PAGE_SIZE) {
        debug(SUBSYS_PAGING,
                "selective flush: 0x%"PRIxGENVADDR"--0x%"PRIxGENVADDR"\n",
                va_hint, va_hint + pages * pagesize);
        // use as direct hint
        // invlpg should work for large/huge pages
        for (int i = 0; i < pages; i++) {
            do_one_tlb_flush(va_hint + i * pagesize);
        }
    } else if (va_hint == 1) {
        // XXX: remove this or cleanup interface, -SG, 2015-03-11
        // do computed selective flush
        debug(SUBSYS_PAGING, "computed selective flush\n");
        return paging_tlb_flush_range(cte_for_cap(mapping), offset, pages);
    } else {
        debug(SUBSYS_PAGING, "full flush\n");
        /* do full TLB flush */
        do_full_tlb_flush();
    }

    return SYS_ERR_OK;
}

errval_t ptable_modify_flags(struct capability *leaf_pt, size_t offset,
                                    size_t pages, size_t mflags)
{
    /* Calculate page access protection flags */
    // Mask with provided access rights mask
    paging_x86_64_flags_t flags = X86_64_PTABLE_USER_SUPERVISOR;
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

    errval_t err = generic_modify_flags(cte_for_cap(leaf_pt), offset, pages, flags);

    do_full_tlb_flush();

    return err;
}

void paging_dump_tables(struct dcb *dispatcher)
{
    if (!local_phys_is_valid(dispatcher->vspace)) {
        printk(LOG_ERR, "dispatcher->vspace = 0x%"PRIxLPADDR": too high!\n" ,
               dispatcher->vspace);
        return;
    }
    lvaddr_t root_pt = local_phys_to_mem(dispatcher->vspace);

    // loop over pdpts
    union x86_64_ptable_entry *pt;
    size_t kernel_pml4e = X86_64_PML4_BASE(X86_64_MEMORY_OFFSET);
    for (int pdpt_index = 0; pdpt_index < kernel_pml4e; pdpt_index++) {
        union x86_64_pdir_entry *pdpt = (union x86_64_pdir_entry *)root_pt + pdpt_index;
        if (!pdpt->d.present) { continue; }
        genpaddr_t pdpt_gp = (genpaddr_t)pdpt->d.base_addr << BASE_PAGE_BITS;
        lvaddr_t pdpt_lv = local_phys_to_mem(gen_phys_to_local_phys(pdpt_gp));

        for (int pdir_index = 0; pdir_index < X86_64_PTABLE_SIZE; pdir_index++) {
            // get pdir
            union x86_64_pdir_entry *pdir = (union x86_64_pdir_entry *)pdpt_lv + pdir_index;
            pt = (union x86_64_ptable_entry*)pdir;
            if (!pdir->d.present) { continue; }
            // check if pdir or huge page
            if (pt->huge.always1) {
                // is huge page mapping
                genpaddr_t paddr = (genpaddr_t)pt->huge.base_addr << HUGE_PAGE_BITS;
                printf("%d.%d: 0x%"PRIxGENPADDR"\n", pdpt_index, pdir_index, paddr);
                // goto next pdpt entry
                continue;
            }
            genpaddr_t pdir_gp = (genpaddr_t)pdir->d.base_addr << BASE_PAGE_BITS;
            lvaddr_t pdir_lv = local_phys_to_mem(gen_phys_to_local_phys(pdir_gp));

            for (int ptable_index = 0; ptable_index < X86_64_PTABLE_SIZE; ptable_index++) {
                // get ptable
                union x86_64_pdir_entry *ptable = (union x86_64_pdir_entry *)pdir_lv + ptable_index;
                pt = (union x86_64_ptable_entry *)ptable;
                if (!ptable->d.present) { continue; }
                // check if ptable or large page
                if (pt->large.always1) {
                    // is large page mapping
                    genpaddr_t paddr = (genpaddr_t)pt->large.base_addr << LARGE_PAGE_BITS;
                    printf("%d.%d.%d: 0x%"PRIxGENPADDR"\n", pdpt_index, pdir_index, ptable_index, paddr);
                    // goto next pdir entry
                    continue;
                }
                genpaddr_t ptable_gp = (genpaddr_t)ptable->d.base_addr << BASE_PAGE_BITS;
                lvaddr_t ptable_lv = local_phys_to_mem(gen_phys_to_local_phys(ptable_gp));

                for (int entry = 0; entry < X86_64_PTABLE_SIZE; entry++) {
                    union x86_64_ptable_entry *e =
                        (union x86_64_ptable_entry *)ptable_lv + entry;
                    if (!e->base.present) { continue; }
                    genpaddr_t paddr = (genpaddr_t)e->base.base_addr << BASE_PAGE_BITS;
                    printf("%d.%d.%d.%d: 0x%"PRIxGENPADDR" (raw=0x%016"PRIx64")\n",
                            pdpt_index, pdir_index, ptable_index, entry, paddr, e->raw);
                }
            }
        }
    }
}
