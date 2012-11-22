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
#include <barrelfish_kpi/init.h>

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

    /*genpaddr_t paddr = get_address(&src_cte->cap) + param2;
    genvaddr_t vaddr;
    compile_vaddr(dest_vnode_cte, dest_slot, &vaddr);
    printf("mapping 0x%"PRIxGENPADDR" to 0x%"PRIxGENVADDR"\n", paddr, vaddr); */

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
        }
        src_cte->mapping_info.mapped_pages += 1;
    }
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
        paddr = e->d.base_addr << BASE_PAGE_BITS;
        entry_ = e;
        pte_ = lp + slot * sizeof(union x86_64_pdir_entry);
        break;
    }
    case ObjType_VNode_x86_64_ptable: {
        union x86_64_ptable_entry *e =
            (union x86_64_ptable_entry *)lv + slot;
        paddr = e->base.base_addr << BASE_PAGE_BITS;
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

static inline errval_t lookup_cap_for_mapping(genpaddr_t paddr, lvaddr_t pte, struct cte **retcte)
{
    // lookup matching cap
    struct cte *mem, *last, *orig;
    // find a cap for paddr
    errval_t err = mdb_find_cap_for_address(paddr, &mem);
    if (err_is_fail(err)) {
        printf("could not find a cap for 0x%"PRIxGENPADDR" (%ld)\n", paddr, err);
        return err;
    }
    //printf("lookup request = 0x%"PRIxGENPADDR"\n", paddr);
    //printf("has_copies(mem) = %d\n", has_copies(mem));

    // look at all copies of mem
    last = mem;
    orig = mem;
    // search backwards in tree
    while (is_copy(&mem->cap, &last->cap)) {
        struct capability *cap = &mem->cap;
        struct mapping_info *map = &mem->mapping_info;
        genpaddr_t base = get_address(cap);
        // only match mappings that start where we want to unmap
        if (base + map->offset == paddr && map->pte == pte)
        {
            // found matching cap
            *retcte = mem;
            return SYS_ERR_OK;
        }
        last = mem;
        mem = mdb_predecessor(mem);
    }
    last = orig;
    // search forward in tree
    mem = mdb_successor(orig);
    while (is_copy(&mem->cap, &last->cap)) {
        struct capability *cap = &mem->cap;
        struct mapping_info *map = &mem->mapping_info;
        genpaddr_t base = get_address(cap);
        // only match mappings that start where we want to unmap
        if (base + map->offset == paddr && map->pte == pte)
        {
            // found matching cap
            *retcte = mem;
            return SYS_ERR_OK;
        }
        last = mem;
        mem = mdb_successor(mem);
    }

    // if we get here, we have not found a matching cap
    return SYS_ERR_CAP_NOT_FOUND;
}

static inline void clear_pt_entry(lvaddr_t pte) {
    ((union x86_64_pdir_entry *)pte)->raw = 0;
}


static inline struct cte *cte_for_cap(struct capability *cap)
{
    return (struct cte *) (cap - offsetof(struct cte, cap));
}

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

size_t do_unmap(lvaddr_t pt, cslot_t slot, genvaddr_t vaddr, size_t num_pages)
{
    // iterate over affected leaf ptables
    size_t unmapped_pages = 0;
    union x86_64_ptable_entry *ptentry = (union x86_64_ptable_entry *)pt + slot;
    do {
        size_t target = (num_pages - unmapped_pages) < (X86_64_PTABLE_SIZE - slot) ? slot + (num_pages - unmapped_pages) : X86_64_PTABLE_SIZE;
        int i;
        size_t old_unmapped = unmapped_pages;
        for (i = slot; i < target; i++) {
            ptentry++->raw = 0;
            unmapped_pages++;
        }
        if (i == X86_64_PTABLE_SIZE && unmapped_pages < num_pages) {
            // get next leaf pt
            vaddr += (unmapped_pages - old_unmapped) * X86_64_BASE_PAGE_SIZE;
            while (!(pt = get_leaf_ptable_for_vaddr(vaddr)) && unmapped_pages < num_pages) {
                // no leaf page table for this address
                unmapped_pages += X86_64_PTABLE_SIZE * X86_64_BASE_PAGE_SIZE;
                vaddr += X86_64_PTABLE_SIZE * X86_64_BASE_PAGE_SIZE;
            }
            slot = 0;
            ptentry = (union x86_64_ptable_entry *)pt;
        }
    } while(unmapped_pages < num_pages);

    if (unmapped_pages > num_pages) { unmapped_pages = num_pages; }

    return unmapped_pages;
}

errval_t page_mappings_unmap(struct capability *pgtable, size_t slot, size_t num_pages)
{
    assert(type_is_vnode(pgtable->type));
    //printf("page_mappings_unmap(%zd pages)\n", num_pages);

    // get page table entry data
    genpaddr_t paddr;
    lvaddr_t pte;
    read_pt_entry(pgtable, slot, &paddr, &pte, NULL);
    lvaddr_t pt = local_phys_to_mem(gen_phys_to_local_phys(get_address(pgtable)));

    // get virtual address of first page
    // TODO: error checking
    genvaddr_t vaddr;
    struct cte *leaf_pt = cte_for_cap(pgtable);
    compile_vaddr(leaf_pt, slot, &vaddr);

    // get cap for mapping
    struct cte *mem;
    errval_t err = lookup_cap_for_mapping(paddr, pte, &mem);
    if (err_is_fail(err)) {
        return err;
    }
    //printf("state before unmap: mapped_pages = %zd\n", mem->mapping_info.mapped_pages);
    //printf("state before unmap: num_pages    = %zd\n", num_pages);

    if (num_pages > mem->mapping_info.mapped_pages) {
        // want to unmap too many pages
        return SYS_ERR_VM_MAP_SIZE;
    }

    size_t unmapped_pages = do_unmap(pt, slot, vaddr, num_pages);

    // update mapping info
    mem->mapping_info.mapped_pages -= unmapped_pages;
    if (mem->mapping_info.mapped_pages == 0) {
        memset(&mem->mapping_info, 0, sizeof(struct mapping_info));
    }
    else {
        // set first still mapped entry as mapping info pte
        mem->mapping_info.pte = pt;
    }

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
