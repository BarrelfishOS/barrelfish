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
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include "target/x86/pmap_x86.h"

// Location and size of virtual address space reserved for mapping
// frames backing refill_slabs
#define META_DATA_RESERVED_BASE ((lvaddr_t)1UL*1024*1024*1024)
#define META_DATA_RESERVED_SIZE (X86_32_BASE_PAGE_SIZE * 1200)

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
#ifdef __scc__
        if (vregion_flags & VREGION_FLAGS_MPB) {
            pmap_flags |= SCC_PTABLE_MESSAGE_BUFFER;
            pmap_flags |= X86_32_PTABLE_WRITE_THROUGH;
        }
#endif
    }

    return pmap_flags;
}

/**
 * \brief Starting at a given root, return the vnode with entry equal to #entry
 */
static struct vnode *find_vnode(struct vnode *root, uint16_t entry)
{
    assert(root != NULL);
    assert(root->is_vnode);
    struct vnode *n;

    for(n = root->u.vnode.children; n != NULL; n = n->next) {
        if(n->entry == entry) {
            return n;
        }
    }
    return NULL;
}

static void remove_vnode(struct vnode *root, struct vnode *item)
{
    assert(root->is_vnode);
    struct vnode *walk = root->u.vnode.children;
    struct vnode *prev = NULL;
    while (walk) {
        if (walk == item) {
            if (prev) {
                prev->next = walk->next;
                return;
            } else {
                root->u.vnode.children = walk->next;
                return;
            }
        }
        prev = walk;
        walk = walk->next;
    }
    USER_PANIC("Should not get here");
}

/**
 * \brief Allocates a new VNode, adding it to the page table and our metadata
 */
static errval_t alloc_vnode(struct pmap_x86 *pmap, struct vnode *root,
                            enum objtype type, uint32_t entry,
                            struct vnode **retvnode)
{
    errval_t err;

    struct vnode *newvnode = slab_alloc(&pmap->slab);
    if (newvnode == NULL) {
        return LIB_ERR_SLAB_ALLOC_FAIL;
    }

    // The VNode capability
    err = pmap->p.slot_alloc->alloc(pmap->p.slot_alloc, &newvnode->u.vnode.cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    err = vnode_create(newvnode->u.vnode.cap, type);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VNODE_CREATE);
    }

    // Map it
    err = vnode_map(root->u.vnode.cap, newvnode->u.vnode.cap, entry,
                    X86_32_PTABLE_ACCESS_DEFAULT, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VNODE_MAP);
    }

    // The VNode meta data
    newvnode->is_vnode  = true;
    newvnode->entry     = entry;
    newvnode->next      = root->u.vnode.children;
    root->u.vnode.children      = newvnode;
    newvnode->u.vnode.children  = NULL;

    *retvnode = newvnode;
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

#ifdef CONFIG_PAE
    struct vnode *root = &pmap->root;
    assert(root != NULL);

    // PDPTE mapping
    if((pdir = find_vnode(root, X86_32_PDPTE_BASE(base))) == NULL) {
        err = alloc_vnode(pmap, root, ObjType_VNode_x86_32_pdir,
                          X86_32_PDPTE_BASE(base), &pdir);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
    }
#else
    pdir = &pmap->root;
#endif

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

static errval_t do_map(struct pmap_x86 *pmap, genvaddr_t vaddr,
                       struct capref frame, size_t offset, size_t size,
                       vregion_flags_t flags, size_t *retoff, size_t *retsize)
{
    errval_t err;
    paging_x86_32_flags_t pmap_flags = vregion_to_pmap_flag(flags);

    for (size_t i = offset; i < offset + size; i += X86_32_BASE_PAGE_SIZE) {
        // Get the page table
        struct vnode *ptable;
        err = get_ptable(pmap, vaddr, &ptable);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_GET_PTABLE);
        }

        // Create user level datastructure for the mapping
        struct vnode *page = find_vnode(ptable, X86_32_PTABLE_BASE(vaddr));
        assert(!page);
        page = slab_alloc(&pmap->slab);
        assert(page);
        page->is_vnode = false;
        page->entry = X86_32_PTABLE_BASE(vaddr);
        page->next  = ptable->u.vnode.children;
        ptable->u.vnode.children = page;
        page->u.frame.cap = frame;
        page->u.frame.offset = i;
        page->u.frame.flags = flags;

        // Map entry into the page table
        uint32_t entry = X86_32_PTABLE_BASE(vaddr);
        err = vnode_map(ptable->u.vnode.cap, frame, entry, pmap_flags, i);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_VNODE_MAP);
        }

        vaddr += BASE_PAGE_SIZE;
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

    // Adjust the parameters to page boundaries
    size   += X86_32_BASE_PAGE_OFFSET(offset);
    size    = ROUND_UP(size, X86_32_BASE_PAGE_SIZE);
    offset -= X86_32_BASE_PAGE_OFFSET(offset);

    // Refill slab allocator if necessary
    size_t slabs_free = slab_freecount(&x86->slab);
    size_t max_slabs = max_slabs_for_mapping(size);
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

    err = do_map(x86, vaddr, frame, offset, size, flags, retoff, retsize);
    return err;
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
    errval_t err;
    struct pmap_x86 *x86 = (struct pmap_x86*)pmap;
    size = ROUND_UP(size, X86_32_BASE_PAGE_SIZE);

    for (size_t i = 0; i < size; i+=X86_32_BASE_PAGE_SIZE) {
        // Find the page table
        struct vnode *ptable;
        err = get_ptable(x86, vaddr + i, &ptable);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_GET_PTABLE);
        }

        // Find the page
        struct vnode *page = find_vnode(ptable, X86_32_PTABLE_BASE(vaddr + i));
        if (!page) {
            return LIB_ERR_PMAP_FIND_VNODE;
        }

        // Unmap it in the kernel
        err = vnode_unmap(ptable->u.vnode.cap, page->entry);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_VNODE_UNMAP);
        }

        // Free up the resources
        remove_vnode(ptable, page);
        slab_free(&x86->slab, page);
    }

    if (retsize) {
        *retsize = size;
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
 */
static errval_t modify_flags(struct pmap *pmap, genvaddr_t vaddr, size_t size,
                             vregion_flags_t flags, size_t *retsize)
{
    errval_t err, ret;
    struct pmap_x86 *x86 = (struct pmap_x86 *)pmap;
    size = ROUND_UP(size, X86_32_BASE_PAGE_SIZE);

    for (size_t i = 0; i < size; i += X86_32_BASE_PAGE_SIZE) {
        // Find the page table
        struct vnode* ptable;
        err = get_ptable(x86, vaddr+i, &ptable);
        if (err_is_fail(err) || (ptable == NULL)) { // not mapped
            ret = LIB_ERR_PMAP_FIND_VNODE;
            continue;
        }

        // Find the page
        struct vnode *vn = find_vnode(ptable, X86_32_PTABLE_BASE(vaddr + i));
        if (vn == NULL) { // not mapped
            ret = LIB_ERR_PMAP_FIND_VNODE;
            continue;
        }

        // Unmap it in the kernel
        err = vnode_unmap(ptable->u.vnode.cap, vn->entry);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_VNODE_UNMAP);
        }

        // Remap with changed flags
        paging_x86_32_flags_t pmap_flags = vregion_to_pmap_flag(flags);
        err = vnode_map(ptable->u.vnode.cap, vn->u.frame.cap, vn->entry,
                        pmap_flags, vn->u.frame.offset);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_VNODE_MAP);
        }

        vn->u.frame.flags = flags;
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

static struct pmap_funcs pmap_funcs = {
    .determine_addr = pmap_x86_determine_addr,
    .map = map,
    .unmap = unmap,
    .modify_flags = modify_flags,
    .lookup = lookup,
    .serialise = pmap_x86_serialise,
    .deserialise = pmap_x86_deserialise,
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
