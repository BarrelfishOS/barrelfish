/**
 * \file
 * \brief pmap management
 *
 * x86_64 specific management of page tables
 *
 * Warning: This code is coupled with the code in slot_alloc/. and pinned.c
 *
 * The maximum number of slots required to map a BASE_PAGE_SIZE
 * sized page is the number of page table levels + 1.
 * The sum for x86_64 is 4.
 *
 * Warning: Additional slots will be required to map a BASE_PAGE_SIZE size page,
 * if we also track the actual frames that are mapped.
 * Currently this is not the case.
 */

/*
 * Copyright (c) 2009-2013 ETH Zurich.
 * Copyright (c) 2014 HP Labs.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include "target/x86/pmap_x86.h"
#include <stdio.h>

// Size of virtual region mapped by a single PML4 entry
#define PML4_MAPPING_SIZE ((genvaddr_t)512*512*512*BASE_PAGE_SIZE)

// Location and size of virtual address space reserved for mapping
// frames backing refill_slabs
#define META_DATA_RESERVED_BASE (PML4_MAPPING_SIZE * (disp_get_core_id() + 1))
#define META_DATA_RESERVED_SIZE (X86_64_BASE_PAGE_SIZE * 80000)

/**
 * \brief Translate generic vregion flags to architecture specific pmap flags
 */
static paging_x86_64_flags_t vregion_to_pmap_flag(vregion_flags_t vregion_flags)
{
    paging_x86_64_flags_t pmap_flags =
        PTABLE_USER_SUPERVISOR | PTABLE_EXECUTE_DISABLE;

    if (!(vregion_flags & VREGION_FLAGS_GUARD)) {
        if (vregion_flags & VREGION_FLAGS_WRITE) {
            pmap_flags |= PTABLE_READ_WRITE;
        }
        if (vregion_flags & VREGION_FLAGS_EXECUTE) {
            pmap_flags &= ~PTABLE_EXECUTE_DISABLE;
        }
        if (vregion_flags & VREGION_FLAGS_NOCACHE) {
            pmap_flags |= PTABLE_CACHE_DISABLED;
        }
        else if (vregion_flags & VREGION_FLAGS_WRITE_COMBINING) {
            // PA4 is configured as write-combining
            pmap_flags |= PTABLE_ATTR_INDEX;
        }
    }

    return pmap_flags;
}

// returns whether va1 and va2 share a page directory entry
// not using X86_64_PDIR_BASE() macro as this would give false positives (same
// entry in different directories)
static inline bool is_same_pdir(genvaddr_t va1, genvaddr_t va2)
{
    return (va1>>X86_64_LARGE_PAGE_BITS) == ((va2-1)>>X86_64_LARGE_PAGE_BITS);
}
// returns whether va1 and va2 share a page directory pointer table entry
static inline bool is_same_pdpt(genvaddr_t va1, genvaddr_t va2)
{
    return (va1>>X86_64_HUGE_PAGE_BITS) == ((va2-1)>>X86_64_HUGE_PAGE_BITS);
}
// returns whether va1 and va2 share a page map level 4 entry
static inline bool is_same_pml4(genvaddr_t va1, genvaddr_t va2)
{
    // the base macros work here as we only have one pml4.
    return X86_64_PML4_BASE(va1) == X86_64_PML4_BASE(va2-1);
}
// size indicates how many bits to shift
static inline genvaddr_t get_addr_prefix(genvaddr_t va, uint8_t size)
{
    return va >> size;
}

static inline bool is_large_page(struct vnode *p)
{
    return !p->is_vnode && p->u.frame.flags & VREGION_FLAGS_LARGE;
}

static inline bool is_huge_page(struct vnode *p)
{
    return !p->is_vnode && p->u.frame.flags & VREGION_FLAGS_HUGE;
}

/**
 * \brief Returns the vnode for the pdpt mapping a given vspace address
 */
static inline errval_t get_pdpt(struct pmap_x86 *pmap, genvaddr_t base,
                                struct vnode **pdpt)
{
    errval_t err;
    struct vnode *root = &pmap->root;
    assert(root != NULL);

    // PML4 mapping
    if((*pdpt = find_vnode(root, X86_64_PML4_BASE(base))) == NULL) {
        err = alloc_vnode(pmap, root, ObjType_VNode_x86_64_pdpt,
                            X86_64_PML4_BASE(base), pdpt);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "alloc_vnode for pdpt");
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Returns the vnode for the page directory mapping a given vspace
 * address
 */
static inline errval_t get_pdir(struct pmap_x86 *pmap, genvaddr_t base,
                                struct vnode **pdir)
{
    errval_t err;
    struct vnode *pdpt;
    err = get_pdpt(pmap, base, &pdpt);
    if (err_is_fail(err)) {
        return err;
    }
    assert(pdpt != NULL);

    // PDPT mapping
    if((*pdir = find_vnode(pdpt, X86_64_PDPT_BASE(base))) == NULL) {
        err = alloc_vnode(pmap, pdpt, ObjType_VNode_x86_64_pdir,
                            X86_64_PDPT_BASE(base), pdir);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "alloc_vnode for pdir");
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Returns the vnode for the pagetable mapping a given vspace address
 */
static inline errval_t get_ptable(struct pmap_x86 *pmap, genvaddr_t base,
                                  struct vnode **ptable)
{
    errval_t err;
    struct vnode *pdir;
    err = get_pdir(pmap, base, &pdir);
    if (err_is_fail(err)) {
        return err;
    }
    assert(pdir != NULL);

    // PDIR mapping
    if((*ptable = find_vnode(pdir, X86_64_PDIR_BASE(base))) == NULL) {
        err = alloc_vnode(pmap, pdir, ObjType_VNode_x86_64_ptable,
                            X86_64_PDIR_BASE(base), ptable);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "alloc_vnode for ptable");
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Returns the vnode for the page directory pointer table mapping for a
 * given vspace address
 */
static inline struct vnode *find_pdpt(struct pmap_x86 *pmap, genvaddr_t base)
{
    struct vnode *root = &pmap->root;
    assert(root != NULL);

    // PDPT mapping
    return find_vnode(root, X86_64_PML4_BASE(base));
}

/**
 * \brief Returns the vnode for the page directory mapping a given vspace
 * address, without performing allocations as get_pdir() does
 */
static inline struct vnode *find_pdir(struct pmap_x86 *pmap, genvaddr_t base)
{
    struct vnode *pdpt = find_pdpt(pmap, base);

    if (pdpt) {
        // PDPT mapping
        return find_vnode(pdpt, X86_64_PDPT_BASE(base));
    } else {
        return NULL;
    }
}

/**
 * \brief Returns the vnode for the pagetable mapping a given vspace address,
 * without performing allocations as get_ptable() does
 */
static inline struct vnode *find_ptable(struct pmap_x86 *pmap, genvaddr_t base)
{
    struct vnode *pdir = find_pdir(pmap, base);

    if (pdir) {
        // PDIR mapping
        return find_vnode(pdir, X86_64_PDIR_BASE(base));
    } else {
        return NULL;
    }
}

static errval_t do_single_map(struct pmap_x86 *pmap, genvaddr_t vaddr,
                              genvaddr_t vend, struct capref frame,
                              size_t offset, size_t pte_count,
                              vregion_flags_t flags)
{
    if (pte_count == 0) {
        debug_printf("do_single_map: pte_count == 0, called from %p\n",
                __builtin_return_address(0));
        return SYS_ERR_OK;
    }
    assert(pte_count > 0);
    // translate flags
    paging_x86_64_flags_t pmap_flags = vregion_to_pmap_flag(flags);

    // Get the paging structure and set paging relevant parameters
    struct vnode *ptable = NULL;
    errval_t err;
    size_t table_base;

    // get the right paging table and address part
    if(flags & VREGION_FLAGS_LARGE) {
        //large 2M pages, mapped into pdir
        err = get_pdir(pmap, vaddr, &ptable);
        table_base = X86_64_PDIR_BASE(vaddr);
    } else if (flags & VREGION_FLAGS_HUGE) {
        //huge 1GB pages, mapped into pdpt
        err = get_pdpt(pmap, vaddr, &ptable);
        table_base = X86_64_PDPT_BASE(vaddr);
    } else {
        //normal 4K pages, mapped into ptable
        err = get_ptable(pmap, vaddr, &ptable);
        table_base = X86_64_PTABLE_BASE(vaddr);
    }
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_GET_PTABLE);
    }
    assert(ptable->is_vnode);

    // check if there is an overlapping mapping
    if (has_vnode(ptable, table_base, pte_count, false)) {
        if (has_vnode(ptable, table_base, pte_count, true)) {
            printf("page already exists in 0x%"
                    PRIxGENVADDR"--0x%"PRIxGENVADDR"\n", vaddr, vend);
            return LIB_ERR_PMAP_EXISTING_MAPPING;
        } else {
            // clean out empty page tables. We do this here because we benefit
            // from having the page tables in place when doing lots of small
            // mappings
            remove_empty_vnodes(pmap, ptable, table_base, pte_count);
        }
    }

    // setup userspace mapping
    struct vnode *page = slab_alloc(&pmap->slab);
    assert(page);
    page->is_vnode = false;
    page->entry = table_base;
    page->next  = ptable->u.vnode.children;
    ptable->u.vnode.children = page;
    page->u.frame.cap = frame;
    page->u.frame.offset = offset;
    page->u.frame.flags = flags;
    page->u.frame.pte_count = pte_count;

    err = pmap->p.slot_alloc->alloc(pmap->p.slot_alloc, &page->mapping);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    // do map
    assert(!capref_is_null(ptable->u.vnode.invokable));
    err = vnode_map(ptable->u.vnode.invokable, frame, table_base,
                    pmap_flags, offset, pte_count, page->mapping);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VNODE_MAP);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Called when enough slabs exist for the given mapping
 */
static errval_t do_map(struct pmap_x86 *pmap, genvaddr_t vaddr,
                       struct capref frame, size_t offset, size_t size,
                       vregion_flags_t flags, size_t *retoff, size_t *retsize)
{
    errval_t err;

    // determine page size and relevant address part
    size_t page_size  = X86_64_BASE_PAGE_SIZE;
    size_t table_base = X86_64_PTABLE_BASE(vaddr);
    uint8_t map_bits  = X86_64_BASE_PAGE_BITS + X86_64_PTABLE_BITS;
    bool debug_out    = false;

    // get base address and size of frame
    struct frame_identity fi;
    err = frame_identify(frame, &fi);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_DO_MAP);
    }

    if ((flags & VREGION_FLAGS_HUGE) &&
        (vaddr & X86_64_HUGE_PAGE_MASK) == 0 &&
        fi.bytes >= X86_64_HUGE_PAGE_SIZE &&
        ((fi.base & X86_64_HUGE_PAGE_MASK) == 0))
    {
        // huge page branch (1GB)
        page_size  = X86_64_HUGE_PAGE_SIZE;
        table_base = X86_64_PDPT_BASE(vaddr);
        map_bits   = X86_64_HUGE_PAGE_BITS + X86_64_PTABLE_BITS;
        debug_out  = false;
        // remove large flag, if we're doing huge mapping
        flags     &= ~VREGION_FLAGS_LARGE;
    } else if ((flags & VREGION_FLAGS_LARGE) &&
               (vaddr & X86_64_LARGE_PAGE_MASK) == 0 &&
               fi.bytes >= X86_64_LARGE_PAGE_SIZE &&
               ((fi.base & X86_64_LARGE_PAGE_MASK) == 0))
    {
        // large page branch (2MB)
        page_size  = X86_64_LARGE_PAGE_SIZE;
        table_base = X86_64_PDIR_BASE(vaddr);
        map_bits   = X86_64_LARGE_PAGE_BITS + X86_64_PTABLE_BITS;
        debug_out  = false;
    } else {
        // remove large/huge flags
        flags &= ~(VREGION_FLAGS_LARGE|VREGION_FLAGS_HUGE);
    }

    // round to the next full page and calculate end address and #ptes
    size = ROUND_UP(size, page_size);
    size_t pte_count = DIVIDE_ROUND_UP(size, page_size);
    genvaddr_t vend = vaddr + size;

    if (offset+size > fi.bytes) {
        debug_printf("do_map: offset=%zu; size=%zu; frame size=%zu\n",
                offset, size, fi.bytes);
        return LIB_ERR_PMAP_FRAME_SIZE;
    }

#if 0
    if (true || debug_out) {
        genpaddr_t paddr = fi.base + offset;

        debug_printf("do_map: 0x%"
                PRIxGENVADDR"--0x%"PRIxGENVADDR" -> 0x%"PRIxGENPADDR
                "; pte_count = %zd; frame bytes = 0x%zx; page size = 0x%zx\n",
                vaddr, vend, paddr, pte_count, fi.bytes, page_size);
    }
#endif

    // all mapping on one leaf table?
    if (is_same_pdir(vaddr, vend) ||
        (flags & VREGION_FLAGS_LARGE && is_same_pdpt(vaddr, vend)) ||
        (flags & VREGION_FLAGS_HUGE && is_same_pml4(vaddr, vend))) {
        // fast path
        if (debug_out) {
            debug_printf("  do_map: fast path: %zd\n", pte_count);
        }
        err = do_single_map(pmap, vaddr, vend, frame, offset, pte_count, flags);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_DO_MAP);
        }
    }
    else { // multiple leaf page tables
        // first leaf
        uint32_t c = X86_64_PTABLE_SIZE - table_base;
        if (debug_out) {
            debug_printf("  do_map: slow path: first leaf %"PRIu32"\n", c);
        }
        genvaddr_t temp_end = vaddr + c * page_size;
        err = do_single_map(pmap, vaddr, temp_end, frame, offset, c, flags);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_DO_MAP);
        }

        // map full leaves
        while (get_addr_prefix(temp_end, map_bits) <
                get_addr_prefix(vend, map_bits))
        {
            // update vars
            vaddr = temp_end;
            temp_end = vaddr + X86_64_PTABLE_SIZE * page_size;
            offset += c * page_size;
            c = X86_64_PTABLE_SIZE;

            // do mapping
            if (debug_out) {
                debug_printf("  do_map: slow path: full leaf\n");
            }
            err = do_single_map(pmap, vaddr, temp_end, frame, offset,
                    X86_64_PTABLE_SIZE, flags);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_DO_MAP);
            }
        }

        // map remaining part
        offset += c * page_size;

        // calculate remaining pages (subtract ptable bits from map_bits to
        // get #ptes of last-level instead of 2nd-to-last).
        c = get_addr_prefix(vend, map_bits-X86_64_PTABLE_BITS) -
            get_addr_prefix(temp_end, map_bits-X86_64_PTABLE_BITS);

        if (c) {
            // do mapping
            if (debug_out) {
                debug_printf("do_map: slow path: last leaf %"PRIu32"\n", c);
            }
            err = do_single_map(pmap, temp_end, vend, frame, offset, c, flags);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_DO_MAP);
            }
        }
    }

    if (retoff) {
        *retoff = offset;
    }
    if (retsize) {
        *retsize = size;
    }
    return SYS_ERR_OK;
}

/// Computer upper limit on number of slabs required to perform a mapping
static size_t max_slabs_for_mapping(size_t bytes)
{
    size_t max_pages  = DIVIDE_ROUND_UP(bytes, X86_64_BASE_PAGE_SIZE);
    size_t max_ptable = DIVIDE_ROUND_UP(max_pages, X86_64_PTABLE_SIZE);
    size_t max_pdir   = DIVIDE_ROUND_UP(max_ptable, X86_64_PTABLE_SIZE);
    size_t max_pdpt   = DIVIDE_ROUND_UP(max_pdir, X86_64_PTABLE_SIZE);
    return max_pages + max_ptable + max_pdir + max_pdpt;
}

static size_t max_slabs_for_mapping_large(size_t bytes)
{
    size_t max_pages  = DIVIDE_ROUND_UP(bytes, X86_64_LARGE_PAGE_SIZE);
    size_t max_pdir   = DIVIDE_ROUND_UP(max_pages, X86_64_PTABLE_SIZE);
    size_t max_pdpt   = DIVIDE_ROUND_UP(max_pdir, X86_64_PTABLE_SIZE);
    return max_pages  + max_pdir + max_pdpt;
}

static size_t max_slabs_for_mapping_huge(size_t bytes)
{
    size_t max_pages  = DIVIDE_ROUND_UP(bytes, X86_64_HUGE_PAGE_SIZE);
    size_t max_pdpt   = DIVIDE_ROUND_UP(max_pages, X86_64_PTABLE_SIZE);
    return max_pages  + max_pdpt;
}

/**
 * \brief Refill slabs used for metadata
 *
 * \param pmap     The pmap to refill in
 * \param request  The number of slabs the allocator must have
 * when the function returns
 *
 * When the current pmap is initialized,
 * it reserves some virtual address space for metadata.
 * This reserved address space is used here
 *
 * Can only be called for the current pmap
 * Will recursively call into itself till it has enough slabs
 */
static errval_t refill_slabs(struct pmap_x86 *pmap, size_t request)
{
    errval_t err;

    /* Keep looping till we have #request slabs */
    while (slab_freecount(&pmap->slab) < request) {
        // Amount of bytes required for #request
        size_t bytes = SLAB_STATIC_SIZE(request - slab_freecount(&pmap->slab),
                                        sizeof(struct vnode));

        /* Get a frame of that size */
        struct capref cap;
        err = frame_alloc(&cap, bytes, &bytes);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_FRAME_ALLOC);
        }

        /* If we do not have enough slabs to map the frame in, recurse */
        size_t required_slabs_for_frame = max_slabs_for_mapping(bytes);
        if (slab_freecount(&pmap->slab) < required_slabs_for_frame) {
            // If we recurse, we require more slabs than to map a single page
            assert(required_slabs_for_frame > 4);

            err = refill_slabs(pmap, required_slabs_for_frame);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_SLAB_REFILL);
            }
        }

        /* Perform mapping */
        genvaddr_t genvaddr = pmap->vregion_offset;
        pmap->vregion_offset += (genvaddr_t)bytes;
        assert(pmap->vregion_offset < vregion_get_base_addr(&pmap->vregion) +
               vregion_get_size(&pmap->vregion));

        err = do_map(pmap, genvaddr, cap, 0, bytes,
                     VREGION_FLAGS_READ_WRITE, NULL, NULL);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_DO_MAP);
        }

        /* Grow the slab */
        lvaddr_t buf = vspace_genvaddr_to_lvaddr(genvaddr);
        slab_grow(&pmap->slab, (void*)buf, bytes);
    }

    return SYS_ERR_OK;
}

/// Minimally refill the slab allocator
static errval_t min_refill_slabs(struct pmap_x86 *pmap)
{
    return refill_slabs(pmap, 5);
}

/**
 * \brief Create page mappings
 *
 * \param pmap     The pmap object
 * \param vaddr    The virtual address to create the mapping for
 * \param frame    The frame cap to map in
 * \param offset   Offset into the frame cap
 * \param size     Size of the mapping
 * \param flags    Flags for the mapping
 * \param retoff   If non-NULL, filled in with adjusted offset of mapped region
 * \param retsize  If non-NULL, filled in with adjusted size of mapped region
 */
static errval_t map(struct pmap *pmap, genvaddr_t vaddr, struct capref frame,
                    size_t offset, size_t size, vregion_flags_t flags,
                    size_t *retoff, size_t *retsize)
{
    errval_t err;
    struct pmap_x86 *x86 = (struct pmap_x86*)pmap;

    struct frame_identity fi;
    err = frame_identify(frame, &fi);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_FRAME_IDENTIFY);
    }

    size_t max_slabs;
    // Adjust the parameters to page boundaries
    // TODO: overestimating needed slabs shouldn't hurt much in the long run,
    // and would keep the code easier to read and possibly faster due to less
    // branching
    if ((flags & VREGION_FLAGS_LARGE) &&
        (vaddr & X86_64_LARGE_PAGE_MASK) == 0 &&
        (fi.base & X86_64_LARGE_PAGE_MASK) == 0 &&
        fi.bytes >= offset+size) {
        //case large pages (2MB)
        size   += LARGE_PAGE_OFFSET(offset);
        size    = ROUND_UP(size, LARGE_PAGE_SIZE);
        offset -= LARGE_PAGE_OFFSET(offset);
        max_slabs = max_slabs_for_mapping_large(size);
    } else if ((flags & VREGION_FLAGS_HUGE) &&
               (vaddr & X86_64_HUGE_PAGE_MASK) == 0 &&
               (fi.base & X86_64_HUGE_PAGE_MASK) == 0 &&
               fi.bytes >= offset+size) {
        // case huge pages (1GB)
        size   += HUGE_PAGE_OFFSET(offset);
        size    = ROUND_UP(size, HUGE_PAGE_SIZE);
        offset -= HUGE_PAGE_OFFSET(offset);
        max_slabs = max_slabs_for_mapping_huge(size);
    } else {
        //case normal pages (4KB)
        size   += BASE_PAGE_OFFSET(offset);
        size    = ROUND_UP(size, BASE_PAGE_SIZE);
        offset -= BASE_PAGE_OFFSET(offset);
        max_slabs = max_slabs_for_mapping(size);
    }

    // Refill slab allocator if necessary
    size_t slabs_free = slab_freecount(&x86->slab);

    max_slabs += 5; // minimum amount required to map a page
    if (slabs_free < max_slabs) {
        struct pmap *mypmap = get_current_pmap();
        if (pmap == mypmap) {
            err = refill_slabs(x86, max_slabs);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_SLAB_REFILL);
            }
        } else {
            size_t bytes = SLAB_STATIC_SIZE(max_slabs - slabs_free,
                                            sizeof(struct vnode));
            void *buf = malloc(bytes);
            if (!buf) {
                return LIB_ERR_MALLOC_FAIL;
            }
            slab_grow(&x86->slab, buf, bytes);
        }
    }

    err = do_map(x86, vaddr, frame, offset, size, flags, retoff, retsize);
    return err;
}

struct find_mapping_info {
    struct vnode *page_table;
    struct vnode *page;
    size_t page_size;
    size_t table_base;
    uint8_t map_bits;
};

/**
 * \brief Find mapping for `vaddr` in `pmap`.
 * \arg pmap the pmap to search in
 * \arg vaddr the virtual address to search for
 * \arg pt the last-level page table meta-data we found if any
 * \arg page the page meta-data we found if any
 * \returns `true` iff we found a mapping for vaddr
 */
static bool find_mapping(struct pmap_x86 *pmap, genvaddr_t vaddr,
                         struct find_mapping_info *info)
{
    struct vnode *pdpt = NULL, *pdir = NULL, *pt = NULL, *page = NULL;

    size_t page_size = 0;
    size_t table_base = 0;
    uint8_t map_bits = 0;

    // find page and last-level page table (can be pdir or pdpt)
    if ((pdpt = find_pdpt(pmap, vaddr)) != NULL) {
        page = find_vnode(pdpt, X86_64_PDPT_BASE(vaddr));
        if (page && page->is_vnode) { // not 1G pages
            pdir = page;
            page = find_vnode(pdir, X86_64_PDIR_BASE(vaddr));
            if (page && page->is_vnode) { // not 2M pages
                pt = page;
                page = find_vnode(pt, X86_64_PTABLE_BASE(vaddr));
                page_size = X86_64_BASE_PAGE_SIZE;
                table_base = X86_64_PTABLE_BASE(vaddr);
                map_bits = X86_64_BASE_PAGE_BITS + X86_64_PTABLE_BITS;
            } else if (page) {
                assert(is_large_page(page));
                pt = pdir;
                page_size = X86_64_LARGE_PAGE_SIZE;
                table_base = X86_64_PDIR_BASE(vaddr);
                map_bits = X86_64_LARGE_PAGE_BITS + X86_64_PTABLE_BITS;
            }
        } else if (page) {
            assert(is_huge_page(page));
            pt = pdpt;
            page_size = X86_64_HUGE_PAGE_SIZE;
            table_base = X86_64_PDPT_BASE(vaddr);
            map_bits = X86_64_HUGE_PAGE_BITS + X86_64_PTABLE_BITS;
        }
    }
    if (info) {
        info->page_table = pt;
        info->page = page;
        info->page_size = page_size;
        info->table_base = table_base;
        info->map_bits = map_bits;
    }
    if (pt && page) {
        return true;
    } else {
        return false;
    }
}

static errval_t do_single_unmap(struct pmap_x86 *pmap, genvaddr_t vaddr,
                                size_t pte_count)
{
    errval_t err;
    struct find_mapping_info info;

    if (!find_mapping(pmap, vaddr, &info)) {
        return LIB_ERR_PMAP_FIND_VNODE;
    }
    assert(info.page_table && info.page_table->is_vnode && info.page && !info.page->is_vnode);

    if (info.page->u.frame.pte_count == pte_count) {
        err = vnode_unmap(info.page_table->u.vnode.cap, info.page->mapping);
        if (err_is_fail(err)) {
            printf("vnode_unmap returned error: %s (%d)\n",
                    err_getstring(err), err_no(err));
            return err_push(err, LIB_ERR_VNODE_UNMAP);
        }

        // delete&free page->mapping after doing vnode_unmap()
        err = cap_delete(info.page->mapping);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CAP_DELETE);
        }
        err = pmap->p.slot_alloc->free(pmap->p.slot_alloc, info.page->mapping);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_SLOT_FREE);
        }
        // Free up the resources
        remove_vnode(info.page_table, info.page);
        slab_free(&pmap->slab, info.page);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Remove page mappings
 *
 * \param pmap     The pmap object
 * \param vaddr    The start of the virtual region to remove
 * \param size     The size of virtual region to remove
 * \param retsize  If non-NULL, filled in with the actual size removed
 */
static errval_t unmap(struct pmap *pmap, genvaddr_t vaddr, size_t size,
                      size_t *retsize)
{
    //printf("[unmap] 0x%"PRIxGENVADDR", %zu\n", vaddr, size);
    errval_t err, ret = SYS_ERR_OK;
    struct pmap_x86 *x86 = (struct pmap_x86*)pmap;

    //determine if we unmap a larger page
    struct find_mapping_info info;

    if (!find_mapping(x86, vaddr, &info)) {
        //TODO: better error --> LIB_ERR_PMAP_NOT_MAPPED
        return LIB_ERR_PMAP_UNMAP;
    }

    assert(!info.page->is_vnode);

    if (info.page->entry > info.table_base) {
        debug_printf("trying to partially unmap region\n");
        // XXX: error code
        return LIB_ERR_PMAP_FIND_VNODE;
    }

    // TODO: match new policy of map when implemented
    size = ROUND_UP(size, info.page_size);
    genvaddr_t vend = vaddr + size;

    if (is_same_pdir(vaddr, vend) ||
        (is_same_pdpt(vaddr, vend) && is_large_page(info.page)) ||
        (is_same_pml4(vaddr, vend) && is_huge_page(info.page)))
    {
        // fast path
        err = do_single_unmap(x86, vaddr, size / info.page_size);
        if (err_is_fail(err) && err_no(err) != LIB_ERR_PMAP_FIND_VNODE) {
            printf("error fast path\n");
            return err_push(err, LIB_ERR_PMAP_UNMAP);
        }
    }
    else { // slow path
        // unmap first leaf
        uint32_t c = X86_64_PTABLE_SIZE - info.table_base;

        err = do_single_unmap(x86, vaddr, c);
        if (err_is_fail(err) && err_no(err) != LIB_ERR_PMAP_FIND_VNODE) {
            printf("error first leaf\n");
            return err_push(err, LIB_ERR_PMAP_UNMAP);
        }

        // unmap full leaves
        vaddr += c * info.page_size;
        while (get_addr_prefix(vaddr, info.map_bits) < get_addr_prefix(vend, info.map_bits)) {
            c = X86_64_PTABLE_SIZE;
            err = do_single_unmap(x86, vaddr, X86_64_PTABLE_SIZE);
            if (err_is_fail(err) && err_no(err) != LIB_ERR_PMAP_FIND_VNODE) {
                printf("error while loop\n");
                return err_push(err, LIB_ERR_PMAP_UNMAP);
            }
            vaddr += c * info.page_size;
        }

        // unmap remaining part
        // subtracting ptable bits from map_bits to get #ptes in last-level table
        // instead of 2nd-to-last.
        c = get_addr_prefix(vend, info.map_bits - X86_64_PTABLE_BITS) -
            get_addr_prefix(vaddr, info.map_bits - X86_64_PTABLE_BITS);
        assert(c < X86_64_PTABLE_SIZE);
        if (c) {
            err = do_single_unmap(x86, vaddr, c);
            if (err_is_fail(err) && err_no(err) != LIB_ERR_PMAP_FIND_VNODE) {
                printf("error remaining part\n");
                return err_push(err, LIB_ERR_PMAP_UNMAP);
            }
        }
    }

    if (retsize) {
        *retsize = size;
    }

    //printf("[unmap] exiting\n");
    return ret;
}

static errval_t do_single_modify_flags(struct pmap_x86 *pmap, genvaddr_t vaddr,
                                       size_t pages, vregion_flags_t flags)
{
    errval_t err = SYS_ERR_OK;

    struct find_mapping_info info;

    if (!find_mapping(pmap, vaddr, &info)) {
        return LIB_ERR_PMAP_FIND_VNODE;
    }

    assert(info.page_table && info.page_table->is_vnode && info.page && !info.page->is_vnode);

    if (inside_region(info.page_table, info.table_base, pages)) {
        // we're modifying part of a valid mapped region
        // arguments to invocation: invoke frame cap, first affected
        // page (as offset from first page in mapping), #affected
        // pages, new flags. Invocation mask flags based on capability
        // access permissions.
        size_t off = info.table_base - info.page->entry;
        paging_x86_64_flags_t pmap_flags = vregion_to_pmap_flag(flags);
        // calculate TLB flushing hint
        genvaddr_t va_hint = 0;
        if (pages == 1) {
            // do assisted selective flush for single page
            va_hint = vaddr & ~(info.page_size - 1);
        }
        err = invoke_mapping_modify_flags(info.page->mapping, off, pages,
                                          pmap_flags, va_hint);
        return err;
    } else {
        // overlaps some region border
        // XXX: need better error
        return LIB_ERR_PMAP_EXISTING_MAPPING;
    }

    return SYS_ERR_OK;
}


/**
 * \brief Modify page mapping
 *
 * \param pmap     The pmap object
 * \param vaddr    The first virtual address for which to change the flags
 * \param size     The length of the region to change in bytes
 * \param flags    New flags for the mapping
 * \param retsize  If non-NULL, filled in with the actual size modified
 */
static errval_t modify_flags(struct pmap *pmap, genvaddr_t vaddr, size_t size,
                             vregion_flags_t flags, size_t *retsize)
{
    errval_t err;
    struct pmap_x86 *x86 = (struct pmap_x86 *)pmap;

    //determine if we unmap a larger page
    struct find_mapping_info info;

    if (!find_mapping(x86, vaddr, &info)) {
        return LIB_ERR_PMAP_NOT_MAPPED;
    }

    assert(info.page && !info.page->is_vnode);

    // TODO: match new policy of map when implemented
    size = ROUND_UP(size, info.page_size);
    genvaddr_t vend = vaddr + size;

    size_t pages = size / info.page_size;

    // vaddr and vend specify begin and end of the region (inside a mapping)
    // that should receive the new set of flags
    if (is_same_pdir(vaddr, vend) ||
        (is_same_pdpt(vaddr, vend) && is_large_page(info.page)) ||
        (is_same_pml4(vaddr, vend) && is_huge_page(info.page))) {
        // fast path
        err = do_single_modify_flags(x86, vaddr, pages, flags);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
        }
    }
    else { // slow path
        // modify first part
        uint32_t c = X86_64_PTABLE_SIZE - info.table_base;
        err = do_single_modify_flags(x86, vaddr, c, flags);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
        }

        // modify full leaves
        vaddr += c * info.page_size;
        while (get_addr_prefix(vaddr, info.map_bits) < get_addr_prefix(vend, info.map_bits)) {
            c = X86_64_PTABLE_SIZE;
            err = do_single_modify_flags(x86, vaddr, X86_64_PTABLE_SIZE, flags);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
            }
            vaddr += c * info.page_size;
        }

        // modify remaining part
        c = get_addr_prefix(vend, info.map_bits - X86_64_PTABLE_BITS) -
                get_addr_prefix(vaddr, info.map_bits - X86_64_PTABLE_BITS);
        if (c) {
            err = do_single_modify_flags(x86, vaddr, c, flags);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
            }
        }
    }

    if (retsize) {
        *retsize = size;
    }

    //printf("[modify_flags] exiting\n");
    return SYS_ERR_OK;
}

/**
 * \brief Query existing page mapping
 *
 * \param pmap     The pmap object
 * \param vaddr    The virtual address to query
 * \param retvaddr Returns the base virtual address of the mapping
 * \param retsize  Returns the actual size of the mapping
 * \param retcap   Returns the cap mapped at this address
 * \param retoffset Returns the offset within the cap that is mapped
 * \param retflags Returns the flags for this mapping
 *
 * All of the ret parameters are optional.
 */
static errval_t lookup(struct pmap *pmap, genvaddr_t vaddr,
                       struct pmap_mapping_info *info)
{
    struct pmap_x86 *x86 = (struct pmap_x86 *)pmap;

    struct find_mapping_info find_info;
    bool found = find_mapping(x86, vaddr, &find_info);

    if (!found) {
        return LIB_ERR_PMAP_FIND_VNODE;
    }

    if (info) {
        info->vaddr = vaddr & ~(genvaddr_t)(find_info.page_size - 1);
        info->size = find_info.page_size;
        info->cap = find_info.page->u.frame.cap;
        info->offset = find_info.page->u.frame.offset;
        info->flags = find_info.page->u.frame.flags;
        info->mapping = find_info.page->mapping;
    }
    return SYS_ERR_OK;
}



static errval_t dump(struct pmap *pmap, struct pmap_dump_info *buf, size_t buflen, size_t *items_written)
{
    struct pmap_x86 *x86 = (struct pmap_x86 *)pmap;
    struct pmap_dump_info *buf_ = buf;

    struct vnode *pml4 = &x86->root;
    struct vnode *pdpt, *pdir, *pt, *frame;
    assert(pml4 != NULL);

    *items_written = 0;

    // iterate over PML4 entries
    size_t pml4_index, pdpt_index, pdir_index;
    for (pdpt = pml4->u.vnode.children; pdpt != NULL; pdpt = pdpt->next) {
        pml4_index = pdpt->entry;
        // iterate over pdpt entries
        for (pdir = pdpt->u.vnode.children; pdir != NULL; pdir = pdir->next) {
            pdpt_index = pdir->entry;
            // iterate over pdir entries
            for (pt = pdir->u.vnode.children; pt != NULL; pt = pt->next) {
                pdir_index = pt->entry;
                // iterate over pt entries
                for (frame = pt->u.vnode.children; frame != NULL; frame = frame->next) {
                    if (*items_written < buflen) {
                        buf_->pml4_index = pml4_index;
                        buf_->pdpt_index = pdpt_index;
                        buf_->pdir_index = pdir_index;
                        buf_->pt_index = frame->entry;
                        buf_->cap = frame->u.frame.cap;
                        buf_->offset = frame->u.frame.offset;
                        buf_->flags = frame->u.frame.flags;
                        buf_++;
                        (*items_written)++;
                    }
                }
            }
        }
    }
    return SYS_ERR_OK;
}

static errval_t determine_addr_raw(struct pmap *pmap, size_t size,
                                   size_t alignment, genvaddr_t *retvaddr)
{
    struct pmap_x86 *x86 = (struct pmap_x86 *)pmap;

    struct vnode *walk_pml4 = x86->root.u.vnode.children;
    assert(walk_pml4 != NULL); // assume there's always at least one existing entry

    if (alignment == 0) {
        alignment = BASE_PAGE_SIZE;
    } else {
        alignment = ROUND_UP(alignment, BASE_PAGE_SIZE);
    }
    size = ROUND_UP(size, alignment);
    assert(size < 512ul * 1024 * 1024 * 1024); // pml4 size

    // try to find free pml4 entry
    bool f[512];
    for (int i = 0; i < 512; i++) {
        f[i] = true;
    }
    //debug_printf("entry: %d\n", walk_pml4->entry);
    f[walk_pml4->entry] = false;
    while (walk_pml4) {
        //debug_printf("looping over pml4 entries\n");
        assert(walk_pml4->is_vnode);
        f[walk_pml4->entry] = false;
        walk_pml4 = walk_pml4->next;
    }
    genvaddr_t first_free = 16;
    for (; first_free < 512; first_free++) {
        //debug_printf("f[%"PRIuGENVADDR"] = %d\n", first_free, f[first_free]);
        if (f[first_free]) {
            break;
        }
    }
    //debug_printf("first_free: %"PRIuGENVADDR"\n", first_free);
    if (first_free < 512) {
        //debug_printf("first_free: %"PRIuGENVADDR"\n", first_free);
        *retvaddr = first_free << 39;
        return SYS_ERR_OK;
    } else {
        return LIB_ERR_OUT_OF_VIRTUAL_ADDR;
    }
}

static struct pmap_funcs pmap_funcs = {
    .determine_addr = pmap_x86_determine_addr,
    .determine_addr_raw = determine_addr_raw,
    .map = map,
    .unmap = unmap,
    .lookup = lookup,
    .modify_flags = modify_flags,
    .serialise = pmap_x86_serialise,
    .deserialise = pmap_x86_deserialise,
    .dump = dump,
};

/**
 * \brief Initialize a x86 pmap object
 *
 * \param pmap Pmap object of type x86
 */
errval_t pmap_x86_64_init(struct pmap *pmap, struct vspace *vspace,
                          struct capref vnode,
                          struct slot_allocator *opt_slot_alloc)
{
    struct pmap_x86 *x86 = (struct pmap_x86*)pmap;

    /* Generic portion */
    pmap->f = pmap_funcs;
    pmap->vspace = vspace;

    if (opt_slot_alloc != NULL) {
        pmap->slot_alloc = opt_slot_alloc;
    } else { /* use default allocator for this dispatcher */
        pmap->slot_alloc = get_default_slot_allocator();
    }

    /* x86 specific portion */
    slab_init(&x86->slab, sizeof(struct vnode), NULL);
    slab_grow(&x86->slab, x86->slab_buffer,
              sizeof(x86->slab_buffer));
    x86->refill_slabs = min_refill_slabs;

    x86->root.is_vnode          = true;
    x86->root.u.vnode.cap       = vnode;
    x86->root.u.vnode.invokable = vnode;
    if (get_croot_addr(vnode) != CPTR_ROOTCN) {
        errval_t err = slot_alloc(&x86->root.u.vnode.invokable);
        assert(err_is_ok(err));
        err = cap_copy(x86->root.u.vnode.invokable, vnode);
        assert(err_is_ok(err));
    }
    assert(!capref_is_null(x86->root.u.vnode.cap));
    assert(!capref_is_null(x86->root.u.vnode.invokable));
    x86->root.u.vnode.children  = NULL;
    x86->root.next              = NULL;

    // choose a minimum mappable VA for most domains; enough to catch NULL
    // pointer derefs with suitably large offsets
    x86->min_mappable_va = 64 * 1024;

    // maximum mappable VA is derived from X86_64_MEMORY_OFFSET in kernel
    x86->max_mappable_va = (genvaddr_t)0xffffff8000000000;

    return SYS_ERR_OK;
}

/**
 * \brief Initialize the current pmap. Reserve space for metadata
 *
 * This code is coupled with #vspace_current_init()
 */
errval_t pmap_x86_64_current_init(bool init_domain)
{
    struct pmap_x86 *x86 = (struct pmap_x86*)get_current_pmap();

    // To reserve a block of virtual address space,
    // a vregion representing the address space is required.
    // We construct a superficial one here and add it to the vregion list.
    struct vregion *vregion = &x86->vregion;
    vregion->vspace = NULL;
    vregion->memobj = NULL;
    vregion->base   = META_DATA_RESERVED_BASE;
    vregion->offset = 0;
    vregion->size   = META_DATA_RESERVED_SIZE;
    vregion->flags  = 0;
    vregion->next = NULL;

    struct vspace *vspace = x86->p.vspace;
    assert(!vspace->head);
    vspace->head = vregion;

    x86->vregion_offset = x86->vregion.base;

    // We don't know the vnode layout for the first part of our address space
    // (which was setup by the kernel), so we avoid mapping there until told it.
    x86->min_mappable_va = META_DATA_RESERVED_BASE;

    return SYS_ERR_OK;
}
