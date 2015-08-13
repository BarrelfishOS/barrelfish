/**
 * \file
 * \brief pmap management
 *
 * x86_32 specific management of page tables
 *
 * Warning: This code is coupled with the code in slot_alloc/. and pinned.c
 *
 * The maximum number of slots required to map a BASE_PAGE_SIZE
 * sized page is the number of page table levels + 1.
 * The sum for x86_32 is 3.
 *
 * Warning: Additional slots will be required to map a BASE_PAGE_SIZE size page,
 * if we also track the actual frames that are mapped.
 * Currently this is not the case.
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

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <stdio.h>
#include "target/x86/pmap_x86.h"


// Location and size of virtual address space reserved for mapping
// frames backing refill_slabs
#define META_DATA_RESERVED_BASE ((lvaddr_t)1UL*1024*1024*1024)
#define META_DATA_RESERVED_SIZE (X86_32_BASE_PAGE_SIZE * 1200)

// flags for large pages
#define FLAGS_LARGE 0x0100

/**
 * \brief Translate generic vregion flags to architecture specific pmap flags
 */
static paging_x86_32_flags_t vregion_to_pmap_flag(vregion_flags_t vregion_flags)
{
    paging_x86_32_flags_t pmap_flags = X86_32_PTABLE_USER_SUPERVISOR |
        X86_32_PTABLE_EXECUTE_DISABLE;

    if (!(vregion_flags & VREGION_FLAGS_GUARD)) {
        if (vregion_flags & VREGION_FLAGS_WRITE) {
            pmap_flags |= X86_32_PTABLE_READ_WRITE;
        }
        if (vregion_flags & VREGION_FLAGS_EXECUTE) {
            pmap_flags &= ~X86_32_PTABLE_EXECUTE_DISABLE;
        }
        if (vregion_flags & VREGION_FLAGS_NOCACHE) {
            pmap_flags |= X86_32_PTABLE_CACHE_DISABLED;
        }
        else if (vregion_flags & VREGION_FLAGS_WRITE_COMBINING) {
            // PA4 is configured as write-combining
            pmap_flags |= PTABLE_ATTR_INDEX;
        }
#ifdef __scc__
        if (vregion_flags & VREGION_FLAGS_MPB) {
            pmap_flags |= SCC_PTABLE_MESSAGE_BUFFER;
            pmap_flags |= X86_32_PTABLE_WRITE_THROUGH;
        }
#endif
    }

    return pmap_flags;
}

static inline bool is_same_pdir(genvaddr_t va1, genvaddr_t va2)
{
    return (va1>>X86_32_LARGE_PAGE_BITS) == (va2>>X86_32_LARGE_PAGE_BITS);
}
static inline bool is_same_pdpt(genvaddr_t va1, genvaddr_t va2)
{
#ifdef CONFIG_PAE
    // PDPT in PAE has 4 entries, uses the topmost two bits
    return (va1>>30) == (va2>>30);
#else
    // since there is no PDPT in 32bit, trivially true
    return true;
#endif
}
static inline genvaddr_t get_addr_prefix(genvaddr_t va)
{
    return va >> X86_32_LARGE_PAGE_BITS;
}

/**
 * \brief Returns the vnode for the page directory mapping a given vspace address
 */
static errval_t get_pdir(struct pmap_x86 *pmap, genvaddr_t base,
                           struct vnode **pdir)
{
#ifdef CONFIG_PAE
    struct vnode *root = &pmap->root;
    assert(root != NULL);

    // PDPTE mapping
    if((*pdir = find_vnode(root, X86_32_PDPTE_BASE(base))) == NULL) {
        errval_t err = alloc_vnode(pmap, root, ObjType_VNode_x86_32_pdir,
                          X86_32_PDPTE_BASE(base), pdir);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
    }
#else
    *pdir = &pmap->root;
#endif

    return SYS_ERR_OK;
}

/**
 * \brief Returns the vnode for the pagetable mapping a given vspace address
 */
static errval_t get_ptable(struct pmap_x86 *pmap, genvaddr_t base,
                           struct vnode **ptable)
{
    errval_t err;
    struct vnode *pdir;
    err = get_pdir(pmap, base, &pdir);
    if (err_is_fail(err)) {
        return err;
    }

    // PDIR mapping
    if((*ptable = find_vnode(pdir, X86_32_PDIR_BASE(base))) == NULL) {
        err = alloc_vnode(pmap, pdir, ObjType_VNode_x86_32_ptable,
                          X86_32_PDIR_BASE(base), ptable);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
    }

    return SYS_ERR_OK;
}

static struct vnode *find_pdir(struct pmap_x86 *pmap, genvaddr_t base)
{
    struct vnode *root = &pmap->root;
    assert(root != NULL);

#ifdef CONFIG_PAE
    // PDPT mapping
    return find_vnode(root, X86_32_PDPTE_BASE(base));
#else
    return root;
#endif
}

static errval_t do_single_map(struct pmap_x86 *pmap, genvaddr_t vaddr,
                              genvaddr_t vend, struct capref frame,
                              size_t offset, size_t pte_count,
                              vregion_flags_t flags)
{
    //printf("[do_single_map] vaddr = 0x%"PRIxGENVADDR"\n", vaddr);
    // translate flags
    paging_x86_32_flags_t pmap_flags = vregion_to_pmap_flag(flags);

    // Get the page table and do mapping specific alterations
    struct vnode *ptable;
    errval_t err;
    size_t base;

    if (flags & VREGION_FLAGS_LARGE) {
        //4M/2M(PAE) mapping
        err = get_pdir(pmap, vaddr, &ptable);
        base = X86_32_PDIR_BASE(vaddr);
    } else {
        //4k mapping
        err = get_ptable(pmap, vaddr, &ptable);
        base = X86_32_PTABLE_BASE(vaddr);
    }    
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_GET_PTABLE);
    }
    assert(ptable->is_vnode);

    // check if there is an overlapping mapping
    if (has_vnode(ptable, base, pte_count, false)) {
        if (has_vnode(ptable, base, pte_count, true)) {
            printf("page already exists in 0x%"
                    PRIxGENVADDR"--0x%"PRIxGENVADDR"\n", vaddr, vend);
            return LIB_ERR_PMAP_EXISTING_MAPPING;
        } else {
            // clean out empty page tables. We do this here because we benefit
            // from having the page tables in place when doing lots of small
            // mappings
            remove_empty_vnodes(pmap, ptable, base, pte_count);
        }
    }

    // setup userspace mapping
    struct vnode *page = slab_alloc(&pmap->slab);
    assert(page);
    page->is_vnode = false;
    page->entry = base;
    page->next  = ptable->u.vnode.children;
    ptable->u.vnode.children = page;
    page->u.frame.cap = frame;
    page->u.frame.offset = offset;
    page->u.frame.flags = flags;
    page->u.frame.pte_count = pte_count;

    // do map
    err = vnode_map(ptable->u.vnode.cap, frame, base,
                    pmap_flags, offset, pte_count);
    if (err_is_fail(err)) {
        printf("error in do_single_map: vnode_map failed\n");
        return err_push(err, LIB_ERR_VNODE_MAP);
    }

    return SYS_ERR_OK;
}

static errval_t do_map(struct pmap_x86 *pmap, genvaddr_t vaddr,
                       struct capref frame, size_t offset, size_t size,
                       vregion_flags_t flags, size_t *retoff, size_t *retsize)
{
    //printf("[do_map] vaddr = 0x%"PRIxGENVADDR", size = %zd\n", vaddr, size);
    errval_t err;

    // figure out mapping parameters
    size_t page_size = X86_32_BASE_PAGE_SIZE;
    size_t base = X86_32_PTABLE_BASE(vaddr);
    if(flags & VREGION_FLAGS_LARGE) {
        //4M/2M (PAE) pages
        page_size = X86_32_LARGE_PAGE_SIZE;
        base = X86_32_PDIR_BASE(vaddr);
    }

    // TODO: needs overhaul for mixed-size mappings
    // TODO: need to make sure we can map that much
    size = ROUND_UP(size, page_size);
    size_t pte_count = DIVIDE_ROUND_UP(size, page_size);
    genvaddr_t vend = vaddr + size;

    if (is_same_pdir(vaddr, vend) ||
        (flags & VREGION_FLAGS_LARGE && is_same_pdpt(vaddr, vend))) {
        // fast path
        err = do_single_map(pmap, vaddr, vend, frame, offset, pte_count, flags);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_DO_MAP);
        }
    }
    else { // multiple leaf page tables
        // first leaf
        uint32_t c = X86_32_PTABLE_SIZE - base;
        genvaddr_t temp_end = vaddr + c * page_size;
        err = do_single_map(pmap, vaddr, temp_end, frame, offset, c, flags);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_DO_MAP);
        }

        // map full leaves
        while (get_addr_prefix(temp_end) < get_addr_prefix(vend)) {
            // update vars
            vaddr = temp_end;
            temp_end = vaddr + X86_32_PTABLE_SIZE * page_size;
            offset += c * page_size;
            c = X86_32_PTABLE_SIZE;
            // copy cap
            struct capref next;
            err = slot_alloc(&next);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_DO_MAP);
            }
            err = cap_copy(next, frame);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_DO_MAP);
            }
            frame = next;

            // do mapping
            err = do_single_map(pmap, vaddr, temp_end, frame, offset,
                    X86_32_PTABLE_SIZE, flags);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_DO_MAP);
            }
        }

        // map remaining part
        offset += c * page_size;
        if(flags & VREGION_FLAGS_LARGE) {
            // 4M/2M (PAE) mapping
            c = X86_32_PDIR_BASE(vend) - X86_32_PDIR_BASE(temp_end);
        } else {
            // 4K mapping
            c = X86_32_PTABLE_BASE(vend) - X86_32_PTABLE_BASE(temp_end);
        }
        if (c) {
            // copy cap
            struct capref next;
            err = slot_alloc(&next);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_DO_MAP);
            }
            err = cap_copy(next, frame);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_DO_MAP);
            }

            // do mapping
            err = do_single_map(pmap, temp_end, vend, next, offset, c, flags);
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

/// Compute upper limit on number of slabs required to perform a mapping
static size_t max_slabs_for_mapping(size_t bytes)
{
    size_t max_pages  = DIVIDE_ROUND_UP(bytes, X86_32_BASE_PAGE_SIZE);
    size_t max_ptable = DIVIDE_ROUND_UP(max_pages, X86_32_PTABLE_SIZE);
    size_t max_pdir   = DIVIDE_ROUND_UP(max_ptable, X86_32_PTABLE_SIZE) + 1;
#ifdef CONFIG_PAE
    size_t max_pdpt   = DIVIDE_ROUND_UP(max_pdir, X86_32_PTABLE_SIZE) + 1;
#else
    size_t max_pdpt   = 0;
#endif
    return max_pages + max_ptable + max_pdir + max_pdpt;
}
static size_t max_slabs_for_mapping_large(size_t bytes)
{
    size_t max_pages  = DIVIDE_ROUND_UP(bytes, X86_32_LARGE_PAGE_SIZE);
    size_t max_pdir   = DIVIDE_ROUND_UP(max_pages, X86_32_PTABLE_SIZE) + 1;
#ifdef CONFIG_PAE
    size_t max_pdpt   = DIVIDE_ROUND_UP(max_pdir, X86_32_PTABLE_SIZE) + 1;
#else
    size_t max_pdpt   = 0;
#endif
    return max_pages + max_pdir + max_pdpt;
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
    //printf("[map] vaddr = 0x%"PRIxGENVADDR", size = %zd\n", vaddr, size);
    errval_t err;
    struct pmap_x86 *x86 = (struct pmap_x86*)pmap;
    
    size_t max_slabs;

    // Adjust the parameters to page boundaries
    if(flags&FLAGS_LARGE) {
        // 4M pages/2M pages(PAE)
        size   += X86_32_LARGE_PAGE_OFFSET(offset);
        size    = ROUND_UP(size, X86_32_LARGE_PAGE_SIZE);
        offset -= X86_32_LARGE_PAGE_OFFSET(offset);
        max_slabs = max_slabs_for_mapping_large(size);
    } else {
        // 4K pages
        size   += X86_32_BASE_PAGE_OFFSET(offset);
        size    = ROUND_UP(size, X86_32_BASE_PAGE_SIZE);
        offset -= X86_32_BASE_PAGE_OFFSET(offset);
        max_slabs = max_slabs_for_mapping(size);
    }

    // Refill slab allocator if necessary
    size_t slabs_free = slab_freecount(&x86->slab);
    max_slabs += 4; // minimum amount required to map a page
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

    //printf("[map call do_map] vaddr = 0x%"PRIxGENVADDR", flag = %x\n", vaddr, (int)flags);
    err = do_map(x86, vaddr, frame, offset, size, flags, retoff, retsize);
    return err;
}

/**
 * \brief Find mapping for `vaddr` in `pmap`.
 * \arg pmap the pmap to search in
 * \arg vaddr the virtual address to search for
 * \arg pt the last-level page table meta-data we found if any
 * \arg page the page meta-data we found if any
 * \returns `true` iff we found a mapping for vaddr
 */
static bool find_mapping(struct pmap_x86 *pmap, genvaddr_t vaddr,
                         struct vnode **outpt, struct vnode **outpage)
{
    struct vnode *pdir = NULL, *pt = NULL, *page = NULL;

    // find page and last-level page table (can be pdir or pdpt)
    if ((pdir = find_pdir(pmap, vaddr)) != NULL) {
        page = find_vnode(pdir, X86_32_PDIR_BASE(vaddr));
        if (page && page->is_vnode) { // not 2M/4M pages
            pt = page;
            page = find_vnode(pt, X86_32_PTABLE_BASE(vaddr));
        } else if (page) {
            pt = pdir;
        }
    }
    if (outpt) {
        *outpt = pt;
    }
    if (outpage) {
        *outpage = page;
    }
    if (pt) {
        return true;
    } else {
        return false;
    }
}

static errval_t do_single_unmap(struct pmap_x86 *pmap, genvaddr_t vaddr,
                                size_t pte_count, bool delete_cap)
{
    errval_t err;
    struct vnode *pt = NULL, *page = NULL;

    find_mapping(pmap, vaddr, &pt, &page);

    if (pt) {
        if (page && page->u.frame.pte_count == pte_count) {
            err = vnode_unmap(pt->u.vnode.cap, page->u.frame.cap, page->entry,
                              page->u.frame.pte_count);
            if (err_is_fail(err)) {
                printf("vnode_unmap returned error: %s (%d)\n",
                        err_getstring(err), err_no(err));
                return err_push(err, LIB_ERR_VNODE_UNMAP);
            }

            // Free up the resources
            if (delete_cap) {
                err = cap_destroy(page->u.frame.cap);
                if (err_is_fail(err)) {
                    return err_push(err, LIB_ERR_PMAP_DO_SINGLE_UNMAP);
                }
            }
            remove_vnode(pt, page);
            slab_free(&pmap->slab, page);
        }
        else {
            printf("couldn't find vnode\n");
            return LIB_ERR_PMAP_FIND_VNODE;
        }
    }

    return SYS_ERR_OK;
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
 * \brief Remove page mappings
 *
 * \param pmap     The pmap object
 * \param vaddr    The start of the virtual addres to remove
 * \param size     The size of virtual address to remove
 * \param retsize  If non-NULL, filled in with the actual size removed
 */
static errval_t unmap(struct pmap *pmap, genvaddr_t vaddr, size_t size,
                      size_t *retsize)
{
    //printf("[unmap] 0x%"PRIxGENVADDR", %zu\n", vaddr, size);
    errval_t err, ret = SYS_ERR_OK;
    struct pmap_x86 *x86 = (struct pmap_x86*)pmap;
    
    //determine if we unmap a larger page
    struct vnode* page = NULL;

    if (!find_mapping(x86, vaddr, NULL, &page)) {
        // TODO: better error
        return LIB_ERR_PMAP_UNMAP;
    }
    assert(!page->is_vnode);

    size_t page_size = X86_32_BASE_PAGE_SIZE;
    if (is_large_page(page)) {
        //large 2M page
        page_size = X86_32_LARGE_PAGE_SIZE;
    }
    
    size = ROUND_UP(size, page_size);
    genvaddr_t vend = vaddr + size;

    if (is_same_pdir(vaddr, vend) ||
        (is_same_pdpt(vaddr, vend) && is_large_page(page))) {
        // fast path
        err = do_single_unmap(x86, vaddr, size / page_size, false);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_UNMAP);
        }
    }
    else { // slow path
        // unmap first leaf
        uint32_t c = X86_32_PTABLE_SIZE - X86_32_PTABLE_BASE(vaddr);
        err = do_single_unmap(x86, vaddr, c, false);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_UNMAP);
        }

        // unmap full leaves
        vaddr += c * page_size;
        while (get_addr_prefix(vaddr) < get_addr_prefix(vend)) {
            c = X86_32_PTABLE_SIZE;
            err = do_single_unmap(x86, vaddr, X86_32_PTABLE_SIZE, true);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_UNMAP);
            }
            vaddr += c * page_size;
        }

        // unmap remaining part
        c = X86_32_PTABLE_BASE(vend) - X86_32_PTABLE_BASE(vaddr);
        if (c) {
            err = do_single_unmap(x86, vaddr, c, true);
            if (err_is_fail(err)) {
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

/*
 * \brief Modify the flags of a single kernel mapping
 *
 * \param pmap x86 pmap
 * \param vaddr start address
 * \param pages number of pages to modify
 * \param flags the new set of flags
 */
static errval_t do_single_modify_flags(struct pmap_x86 *pmap, genvaddr_t vaddr,
                                       size_t pages, vregion_flags_t flags)
{
    errval_t err = SYS_ERR_OK;

    struct vnode *pt = NULL, *page = NULL;

    if (!find_mapping(pmap, vaddr, &pt, &page)) {
        return LIB_ERR_PMAP_FIND_VNODE;
    }

    assert(pt && pt->is_vnode && page && !page->is_vnode);

    uint16_t ptentry = X86_32_PTABLE_BASE(vaddr);
    size_t pagesize = BASE_PAGE_SIZE;
    if (is_large_page(page)) {
        //large 2M page
        ptentry = X86_32_PDIR_BASE(vaddr);
        pagesize = LARGE_PAGE_SIZE;
    }

    if (inside_region(pt, ptentry, pages)) {
        // we're modifying part of a valid mapped region
        // arguments to invocation: invoke frame cap, first affected
        // page (as offset from first page in mapping), #affected
        // pages, new flags. Invocation should check compatibility of
        // new set of flags with cap permissions.
        size_t off = ptentry - page->entry;
        paging_x86_32_flags_t pmap_flags = vregion_to_pmap_flag(flags);
        // calculate TLB flushing hint
        genvaddr_t va_hint = 0;
        if (pages == 1) {
            // do assisted selective flush for single page
            va_hint = vaddr & ~X86_32_BASE_PAGE_MASK;
        }
        err = invoke_frame_modify_flags(page->u.frame.cap, off, pages, pmap_flags, va_hint);
        printf("invoke_frame_modify_flags returned error: %s (%"PRIuERRV")\n",
                err_getstring(err), err);
        return err;
    }
    return SYS_ERR_OK;
}

/**
 * \brief Modify page mapping
 *
 * \param pmap     The pmap object
 * \param vaddr    The virtual address to unmap
 * \param flags    New flags for the mapping
 * \param retsize  If non-NULL, filled in with the actual size modified
 *
 * TODO: fix for large page mappings
 */
static errval_t modify_flags(struct pmap *pmap, genvaddr_t vaddr, size_t size,
                             vregion_flags_t flags, size_t *retsize)
{
    errval_t err;
    struct pmap_x86 *x86 = (struct pmap_x86 *)pmap;

    //determine if we unmap a larger page
    struct vnode* page = NULL;

    if (!find_mapping(x86, vaddr, NULL, &page)) {
        return LIB_ERR_PMAP_NOT_MAPPED;
    }

    assert(page && !page->is_vnode);

    size_t page_size = X86_32_BASE_PAGE_SIZE;
    size_t table_base = X86_32_PTABLE_BASE(vaddr);
    uint8_t map_bits= X86_32_BASE_PAGE_BITS + X86_32_PTABLE_BITS;
    if (is_large_page(page)) {
        //large 2/4M page
        page_size = X86_32_LARGE_PAGE_SIZE;
        table_base = X86_32_PDIR_BASE(vaddr);
        map_bits = X86_32_LARGE_PAGE_BITS + X86_32_PTABLE_BITS;
    }

    // TODO: match new policy of map when implemented
    size = ROUND_UP(size, page_size);
    genvaddr_t vend = vaddr + size;

    size_t pages = size / page_size;

    if (is_same_pdir(vaddr, vend) ||
        (is_same_pdpt(vaddr, vend) && is_large_page(page)))
    {
        // fast path
        err = do_single_modify_flags(x86, vaddr, pages, flags);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
        }
    } else { // slow path
        // unmap first leaf
        uint32_t c = X86_32_PTABLE_SIZE - X86_32_PTABLE_BASE(vaddr);
        err = do_single_modify_flags(x86, vaddr, c, flags);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
        }

        // unmap full leaves
        vaddr += c * page_size;
        while (get_addr_prefix(vaddr) < get_addr_prefix(vend)) {
            c = X86_32_PTABLE_SIZE;
            err = do_single_modify_flags(x86, vaddr, X86_32_PTABLE_SIZE, flags);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
            }
            vaddr += c * page_size;
        }

        // unmap remaining part
        c = X86_32_PTABLE_BASE(vend) - X86_32_PTABLE_BASE(vaddr);
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
                       genvaddr_t *retvaddr, size_t *retsize,
                       struct capref *retcap, genvaddr_t *retoffset,
                       vregion_flags_t *retflags)
{
    USER_PANIC("NYI");
    return 0;
}

static errval_t dump(struct pmap *pmap, struct pmap_dump_info *buf, size_t buflen, size_t *items_written)
{
    struct pmap_x86 *x86 = (struct pmap_x86 *)pmap;
    struct pmap_dump_info *buf_ = buf;

#ifdef CONFIG_PAE
    struct vnode *pdpt = &x86->root, *pdir;
    size_t pdpt_index;
    assert(pdpt != NULL);
#else
    struct vnode *pdir = &x86->root;
    assert(pdir != NULL);
#endif
    struct vnode *pt, *frame;

    *items_written = 0;

    // iterate over pdpt entries
    size_t pdir_index;
#if CONFIG_PAE
    for (pdir = pdpt->u.vnode.children; pdir != NULL; pdir = pdir->next) {
        pdpt_index = pdir->entry;
        // iterate over pdir entries
#endif
        for (pt = pdir->u.vnode.children; pt != NULL; pt = pt->next) {
            pdir_index = pt->entry;
            // iterate over pt entries
            for (frame = pt->u.vnode.children; frame != NULL; frame = frame->next) {
                if (*items_written < buflen) {
#if CONFIG_PAE
                    buf_->pdpt_index = pdpt_index;
#endif
                    buf_->pdir_index = pdir_index;
                    buf_->pt_index = frame->entry;
                    buf_->cap = frame->u.frame.cap;
                    buf_->offset = frame->u.frame.offset;
                    buf_->flags = frame->u.frame.flags;
                    buf_++;
                    (*items_written)++;
                }
            }
#if CONFIG_PAE
        }
#endif
    }
    return SYS_ERR_OK;
}

/** \brief Retrieves an address that can currently be used for large mappings
  *
  */
static errval_t determine_addr_raw(struct pmap *pmap, size_t size,
                                   size_t alignment, genvaddr_t *retvaddr)
{
    struct pmap_x86 *x86 = (struct pmap_x86 *)pmap;

    struct vnode *walk_pdir = x86->root.u.vnode.children;
    assert(walk_pdir != NULL); // assume there's always at least one existing entry

    if (alignment == 0) {
        alignment = BASE_PAGE_SIZE;
    } else {
        alignment = ROUND_UP(alignment, BASE_PAGE_SIZE);
    }
    size = ROUND_UP(size, alignment);

    size_t free_count = DIVIDE_ROUND_UP(size, LARGE_PAGE_SIZE);
    //debug_printf("need %zu contiguous free pdirs\n", free_count);

    // compile pdir free list
    bool f[1024];
    for (int i = 0; i < 1024; i++) {
        f[i] = true;
    }
    f[walk_pdir->entry] = false;
    while (walk_pdir) {
        assert(walk_pdir->is_vnode);
        f[walk_pdir->entry] = false;
        walk_pdir = walk_pdir->next;
    }
    genvaddr_t first_free = 384;
    // XXX: breaks for PAE
    for (; first_free < 512; first_free++) {
        if (f[first_free]) {
            for (int i = 1; i < free_count; i++) {
                if (!f[first_free + i]) {
                    // advance pointer
                    first_free = first_free+i;
                    goto next;
                }
            }
            break;
        }
next:
        assert(1 == 1);// make compiler shut up about label
    }
    //printf("first free: %li\n", (uint32_t)first_free);
    if (first_free + free_count <= 512) {
        *retvaddr = first_free << 22;
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
    .modify_flags = modify_flags,
    .lookup = lookup,
    .serialise = pmap_x86_serialise,
    .deserialise = pmap_x86_deserialise,
    .dump = dump,
};

/**
 * \brief Initialize a x86 pmap object
 */
errval_t pmap_x86_32_init(struct pmap *pmap, struct vspace *vspace,
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

    x86->root.u.vnode.cap       = vnode;
    x86->root.u.vnode.children  = NULL;
    x86->root.is_vnode  = true;
    x86->root.next      = NULL;

    // choose a minimum mappable VA for most domains; enough to catch NULL
    // pointer derefs with suitably large offsets
    x86->min_mappable_va = 64 * 1024;

    // maximum mappable VA is derived from X86_32_MEMORY_OFFSET in kernel
    x86->max_mappable_va = (genvaddr_t)2 * 1024 * 1024 * 1024;

    return SYS_ERR_OK;
}

/**
 * \brief Initialize the current pmap. Reserve space for metadata
 *
 * This code is coupled with #vspace_current_init()
 */
errval_t pmap_x86_32_current_init(bool init_domain)
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
