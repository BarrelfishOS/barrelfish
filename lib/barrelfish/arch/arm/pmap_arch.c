/**
 * \file
 * \brief pmap management
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * There was some minor difficulty here with mapping the cpus native
 * page table arrangement onto Barrelfish. The problem lies with
 * resource bootstrapping. The bootstrap ram allocator allocates pages.
 *
 *
 * The natural division of bits is 12/10/12, corresponding to 4K
 * L1 entries in the L1 table and 256 L2 entries per L2
 * table. Unfortunately 256 entries consumes 1KB rather than a
 * page (4KB) so we pretend here and in the kernel caps page
 * code that the L1 has 1024 entries and L2 tables are 4KB in
 * size. The 4KB constraint comes from ram_alloc_fixed
 * allocating single pages and the difficulty in bootstrapping
 * cap slots (alloc_node takes a single slot.
 *
 * For now this suffices, but might need to be revisited in future.
 *
 * An earlier cut at this, used the first 1KB from each
 * allocation made from ram_alloc_fixed and wasted the remaining
 * space. Aside from the space wasted it entailed a couple of minor
 * platform ifdefs to work around the discrepency.
 *
 * Alternative fixes discussed include:
 *
 * 1. avoid the need to create vnodes before connecting to a
 *    real allocator (probably not plausible).
 *
 * 2. somehow make ram_alloc_fixed handle sub-page allocations
 *    (it's clunky, but perhaps we can give each domain a separate
 *     cnode full of 1k- sized RAM caps?)
 *
 * 3. handle the problem at the level of vnode_create (can't see how to
 *    do this)
 *
 * 4. waste the space -- doing this cleanly will require a new parameter
 * to retype to prevent all 4 caps being created
 *
 * 5. introduce a new arm-specific version of vnode_create that creates
 * 4 1k vnodes, and is only called from the ARM VM code.
 *
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>

// Location of VSpace managed by this system.
#define VSPACE_BEGIN   ((lvaddr_t)1UL*1024*1024*1024)

// Amount of virtual address space reserved for mapping frames
// backing refill_slabs.
//#define META_DATA_RESERVED_SPACE (BASE_PAGE_SIZE * 128) // 64
#define META_DATA_RESERVED_SPACE (BASE_PAGE_SIZE * 256)
// increased above value from 128 for pandaboard port

static inline uintptr_t
vregion_flags_to_kpi_paging_flags(vregion_flags_t flags)
{
    STATIC_ASSERT(0x2f == VREGION_FLAGS_MASK, "");
    STATIC_ASSERT(0x0f == KPI_PAGING_FLAGS_MASK, "");
    STATIC_ASSERT(VREGION_FLAGS_READ    == KPI_PAGING_FLAGS_READ,    "");
    STATIC_ASSERT(VREGION_FLAGS_WRITE   == KPI_PAGING_FLAGS_WRITE,   "");
    STATIC_ASSERT(VREGION_FLAGS_EXECUTE == KPI_PAGING_FLAGS_EXECUTE, "");
    STATIC_ASSERT(VREGION_FLAGS_NOCACHE == KPI_PAGING_FLAGS_NOCACHE, "");
    if ((flags & VREGION_FLAGS_MPB) != 0) {
        // XXX: ignore MPB flag on ARM, otherwise the assert below fires -AB
        flags &= ~VREGION_FLAGS_MPB;
    }
    if ((flags & VREGION_FLAGS_GUARD) != 0) {
        flags = 0;
    }
    assert(0 == (~KPI_PAGING_FLAGS_MASK & (uintptr_t)flags));
    return (uintptr_t)flags;
}

/**
 * \brief Starting at a given root, return the vnode with entry equal to #entry
 */
static struct vnode *find_vnode(struct vnode *root, uint32_t entry)
{
    assert(root != NULL);
    struct vnode *n;

    for(n = root->children; n != NULL; n = n->next) {
        if(n->entry == entry) {
            return n;
        }
    }
    return NULL;
}

static bool has_vnode(struct vnode *root, uint32_t entry, size_t len)
{
    assert(root != NULL);
    assert(root->is_vnode);
    struct vnode *n;

    uint32_t end_entry = entry + len;

    for (n = root->u.vnode.children; n; n = n->next) {
        if (n->is_vnode && n->entry == entry) {
            return true;
        }
        // n is frame
        uint32_t end = n->entry + n->pte_count;
        if (n->entry < entry && end > end_entry) {
            return true;
        }
        if (n->entry >= entry && n->entry < end_entry) {
            return true;
        }
    }

    return false;
}

static void remove_vnode(struct vnode *root, struct vnode *item)
{
    struct vnode *walk = root->children;
    struct vnode *prev = NULL;
    while (walk) {
        if (walk == item) {
            if (prev) {
                prev->next = walk->next;
                return;
            } else {
                root->children = walk->next;
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
static errval_t alloc_vnode(struct pmap_arm *pmap_arm, struct vnode *root,
                            enum objtype type, uint32_t entry,
                            struct vnode **retvnode)
{
    errval_t err;

    struct vnode *newvnode = slab_alloc(&pmap_arm->slab);
    if (newvnode == NULL) {
        return LIB_ERR_SLAB_ALLOC_FAIL;
    }

    // The VNode capability
    err = slot_alloc(&newvnode->cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    err = vnode_create(newvnode->cap, type);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VNODE_CREATE);
    }

    err = vnode_map(root->cap, newvnode->cap, entry,
                    KPI_PAGING_FLAGS_READ | KPI_PAGING_FLAGS_WRITE, 0, 1);

    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VNODE_MAP);
    }

    // The VNode meta data
    newvnode->entry     = entry;
    newvnode->next      = root->children;
    root->children      = newvnode;
    newvnode->children  = NULL;

    if (retvnode) {
        *retvnode = newvnode;
    }
    return SYS_ERR_OK;
}

/**
 * \brief Returns the vnode for the pagetable mapping a given vspace address
 */
static errval_t get_ptable(struct pmap_arm  *pmap,
                           genvaddr_t        vaddr,
                           struct vnode    **ptable)
{
    // NB Strictly there are 12 bits in the ARM L1, but allocations unit
    // of L2 is 1 page of L2 entries (4 tables) so
    uintptr_t index = ((uintptr_t)vaddr) >> 22;
    if ((*ptable = find_vnode(&pmap->root, index)) == NULL)
    {
        // L1 table entries point to L2 tables so allocate an L2
        // table for this L1 entry.

        struct vnode *tmp = NULL; // Tmp variable for passing to alloc_vnode

        errval_t err = alloc_vnode(pmap, &pmap->root, ObjType_VNode_ARM_l2,
                                   index, &tmp);
        assert(tmp != NULL);
        *ptable = tmp; // Set argument to received value


        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
    }

    return SYS_ERR_OK;
}

static errval_t do_single_map(struct pmap_arm *pmap, genvaddr_t vaddr,
                              struct capref frame, size_t offset, size_t pte_count,
                              vregion_flags_t flags)
{
    // Get the page table
    struct vnode *ptable;
    err = get_ptable(pmap, vaddr, &ptable);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_GET_PTABLE);
    }
    uintptr_t index = (((uintptr_t)vaddr) >> 12) & 0x3ffu;
    // Create user level datastructure for the mapping
    bool has_page = has_vnode(ptable, index);
    assert(!has_page);
    struct vnode *page = slab_alloc(&pmap->slab);
    assert(page);
    page->entry = index;
    page->next  = ptable->children;
    ptable->children = page;
    page->children = NULL;
    page->cap = frame;
    page->pte_count = pte_count;

    // Map entry into the page table
    err = vnode_map(ptable->cap, frame, index,
                    pmap_flags, offset, pte_count);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VNODE_MAP);
    }
}

static errval_t do_map(struct pmap_arm *pmap, genvaddr_t vaddr,
                       struct capref frame, size_t offset, size_t size,
                       vregion_flags_t flags, size_t *retoff, size_t *retsize)
{
    errval_t err;

    size = ROUND_UP(size, BASE_PAGE_SIZE);
    size_t pte_count = DIVIDE_ROUND_UP(size, BASE_PAGE_SIZE);
    genvaddr_t vend = vaddr + size;

    if (ARM_L1_OFFSET(vaddr) == ARM_L1_OFFSET(vend)) {
        // fast path
        do_single_map(pmap, vaddr, vend, frame, offset, pte_count, flags);
    }
    else { // multiple leaf page tables
        // first leaf
        uint32_t c = ARM_PTABLE_SIZE - ARM_L2_OFFSET(vaddr);
        genvaddr_t temp_end = vaddr + c * ARM_BASE_PAGE_SIZE;
        err = do_single_map(pmap, vaddr, temp_end, frame, offset, c, flags);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_DO_MAP);
        }

        // map full leaves
        while (ARM_L1_OFFSET(temp_end) < ARM_L1_OFFSET(vend)) { // update vars
            vaddr = temp_end;
            temp_end = vaddr + ARM_PTABLE_SIZE * BASE_PAGE_SIZE;
            offset += c * ARM_BASE_PAGE_SIZE;
            c = ARM_PTABLE_SIZE;
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
            err = do_single_map(pmap, vaddr, temp_end, frame, offset, ARM_PTABLE_SIZE, flags);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_DO_MAP);
            }
        }

        // map remaining part
        offset += c * BASE_PAGE_SIZE;
        c = ARM_L2_OFFSET(vend) - ARM_L2_OFFSET(temp_end);
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
#if 0
    errval_t err;
    uintptr_t pmap_flags = vregion_flags_to_kpi_paging_flags(flags);

    for (size_t i = offset; i < offset + size; i += BASE_PAGE_SIZE) {

        vaddr += BASE_PAGE_SIZE;
    }

    if (retoff) {
        *retoff = offset;
    }
    if (retsize) {
        *retsize = size;
    }
    return SYS_ERR_OK;
#endif
}

static size_t
max_slabs_required(size_t bytes)
{
    // Perform a slab allocation for every page (do_map -> slab_alloc)
    size_t pages     = DIVIDE_ROUND_UP(bytes, BASE_PAGE_SIZE);
    // Perform a slab allocation for every L2 (get_ptable -> find_vnode)
    size_t l2entries = DIVIDE_ROUND_UP(pages, 256 * 4);
    // Perform a slab allocation for every L1 (do_map -> find_vnode)
    size_t l1entries = DIVIDE_ROUND_UP(l2entries, 1024);
    return pages + l2entries + l1entries;
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
#include <stdio.h>
static errval_t refill_slabs(struct pmap_arm *pmap, size_t request)
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
        size_t required_slabs_for_frame = max_slabs_required(bytes);
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

        // if this assert fires, increase META_DATA_RESERVED_SPACE
        assert(pmap->vregion_offset < (vregion_get_base_addr(&pmap->vregion) +
               vregion_get_size(&pmap->vregion)));

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
static errval_t
map(struct pmap     *pmap,
    genvaddr_t       vaddr,
    struct capref    frame,
    size_t           offset,
    size_t           size,
    vregion_flags_t  flags,
    size_t          *retoff,
    size_t          *retsize)
{
    struct pmap_arm *pmap_arm = (struct pmap_arm *)pmap;

    size   += BASE_PAGE_OFFSET(offset);
    size    = ROUND_UP(size, BASE_PAGE_SIZE);
    offset -= BASE_PAGE_OFFSET(offset);

    const size_t slabs_reserve = 3; // == max_slabs_required(1)
    uint64_t  slabs_free       = slab_freecount(&pmap_arm->slab);
    size_t    slabs_required   = max_slabs_required(size) + slabs_reserve;

    if (slabs_required > slabs_free) {
        if (get_current_pmap() == pmap) {
            errval_t err = refill_slabs(pmap_arm, slabs_required);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_SLAB_REFILL);
            }
        }
        else {
            size_t bytes = SLAB_STATIC_SIZE(slabs_required - slabs_free,
                                            sizeof(struct vnode));
            void *buf = malloc(bytes);
            if (!buf) {
                return LIB_ERR_MALLOC_FAIL;
            }
            slab_grow(&pmap_arm->slab, buf, bytes);
        }
    }

    return do_map(pmap_arm, vaddr, frame, offset, size, flags,
                  retoff, retsize);
}

/**
 * \brief Remove page mappings
 *
 * \param pmap     The pmap object
 * \param vaddr    The start of the virtual addres to remove
 * \param size     The size of virtual address to remove
 * \param retsize  If non-NULL, filled in with the actual size removed
 */
static errval_t
unmap(struct pmap *pmap,
      genvaddr_t   vaddr,
      size_t       size,
      size_t      *retsize)
{
	errval_t err;
    size = ROUND_UP(size, BASE_PAGE_SIZE);
    struct pmap_arm* pmap_arm = (struct pmap_arm*)pmap;


    for (size_t i = 0; i < size; i+=BASE_PAGE_SIZE) {
            // Find the page table
            struct vnode *ptable;
            err = get_ptable(pmap_arm, vaddr + i, &ptable);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_GET_PTABLE);
            }

            // Find the page
            uintptr_t index = (((uintptr_t)vaddr+i) >> 12) & 0x3ffu;
            struct vnode *page = find_vnode(ptable, index);
            if (!page) {
                return LIB_ERR_PMAP_FIND_VNODE;
            }

            // Unmap it in the kernel
            err = vnode_unmap(ptable->cap, page->entry);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_VNODE_UNMAP);
            }

            // Free up the resources
            remove_vnode(ptable, page);
            slab_free(&pmap_arm->slab, page);
        }

        if (retsize) {
            *retsize = size;
        }
        return SYS_ERR_OK;
}

/**
 * \brief Determine a suitable address for a given memory object
 *
 * \param pmap    The pmap object
 * \param memobj  The memory object to determine the address for
 * \param alignment Minimum alignment
 * \param vaddr   Pointer to return the determined address
 *
 * Relies on vspace.c code maintaining an ordered list of vregions
 */
static errval_t
determine_addr(struct pmap   *pmap,
               struct memobj *memobj,
               size_t        alignment,
               genvaddr_t    *vaddr)
{
    assert(pmap->vspace->head);

    assert(alignment <= BASE_PAGE_SIZE); // NYI

    struct vregion *walk = pmap->vspace->head;
    while (walk->next) { // Try to insert between existing mappings
        genvaddr_t walk_base = vregion_get_base_addr(walk);
        genvaddr_t walk_size = vregion_get_size(walk);
        genvaddr_t next_base = vregion_get_base_addr(walk->next);

        if (next_base > walk_base + walk_size + memobj->size &&
            walk_base + walk_size > VSPACE_BEGIN) { // Ensure mappings are larger than VSPACE_BEGIN
            *vaddr = walk_base + walk_size;
            return SYS_ERR_OK;
        }
        walk = walk->next;
    }

    *vaddr = vregion_get_base_addr(walk) + vregion_get_size(walk);
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
static errval_t
modify_flags(struct pmap     *pmap,
             genvaddr_t       vaddr,
             size_t           size,
             vregion_flags_t  flags,
             size_t          *retsize)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
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


static errval_t
serialise(struct pmap *pmap, void *buf, size_t buflen)
{
    // Unimplemented: ignored
    return SYS_ERR_OK;
}

static errval_t
deserialise(struct pmap *pmap, void *buf, size_t buflen)
{
    // Unimplemented: we start with an empty pmap, and avoid the bottom of the A/S
    return SYS_ERR_OK;
}

static struct pmap_funcs pmap_funcs = {
    .determine_addr = determine_addr,
    .map = map,
    .unmap = unmap,
    .modify_flags = modify_flags,
    .lookup = lookup,
    .serialise = serialise,
    .deserialise = deserialise,
};

/**
 * \brief Initialize the pmap object
 */
errval_t
pmap_init(struct pmap   *pmap,
          struct vspace *vspace,
          struct capref  vnode,
          struct slot_allocator *opt_slot_alloc)
{
    struct pmap_arm* pmap_arm = (struct pmap_arm*)pmap;

    /* Generic portion */
    pmap->f = pmap_funcs;
    pmap->vspace = vspace;

    // Slab allocator for vnodes
    slab_init(&pmap_arm->slab, sizeof(struct vnode), NULL);
    slab_grow(&pmap_arm->slab,
              pmap_arm->slab_buffer,
              sizeof(pmap_arm->slab_buffer));

    pmap_arm->root.cap      = vnode;
    pmap_arm->root.next     = NULL;
    pmap_arm->root.children = NULL;

    return SYS_ERR_OK;
}

errval_t pmap_current_init(bool init_domain)
{
    struct pmap_arm *pmap_arm = (struct pmap_arm*)get_current_pmap();

    // To reserve a block of virtual address space,
    // a vregion representing the address space is required.
    // We construct a superficial one here and add it to the vregion list.
    struct vregion *vregion = &pmap_arm->vregion;
    assert((void*)vregion > (void*)pmap_arm);
    assert((void*)vregion < (void*)(pmap_arm + 1));
    vregion->vspace = NULL;
    vregion->memobj = NULL;
    vregion->base   = VSPACE_BEGIN;
    vregion->offset = 0;
    vregion->size   = META_DATA_RESERVED_SPACE;
    vregion->flags  = 0;
    vregion->next = NULL;

    struct vspace *vspace = pmap_arm->p.vspace;
    assert(!vspace->head);
    vspace->head = vregion;

    pmap_arm->vregion_offset = pmap_arm->vregion.base;

    return SYS_ERR_OK;
}
