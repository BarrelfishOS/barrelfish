/**
 * \file
 * \brief pmap management -- generic slab management parts
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */


#include <barrelfish/barrelfish.h>
#include <pmap_priv.h>

// Size of virtual region mapped by a single PML4 entry
#define VROOT_ENTRY_MAPPING_SIZE \
    ((genvaddr_t)PTABLE_ENTRIES*PTABLE_ENTRIES*PTABLE_ENTRIES*BASE_PAGE_SIZE)

// where to put metadata: this is currently assuming that we have a four-level
// ptable tree where each level has the same amount of table entries.
#define META_DATA_RESERVED_BASE (VROOT_ENTRY_MAPPING_SIZE * (disp_get_core_id() + 1))
// Amount of virtual address space reserved for mapping frames
// backing refill_slabs. Need way more with pmap_array.
#define META_DATA_RESERVED_SIZE (BASE_PAGE_SIZE * 256000)
// increased above value from 128 for pandaboard port

/**
 * \brief Refill slabs using pages from fixed allocator, used if we need to
 * refill the slab allocator before we've established a connection to
 * the memory server.
 *
 * \arg pmap the pmap whose slab allocator we want to refill
 * \arg bytes the refill buffer size in bytes
 */
static errval_t refill_slabs_fixed_allocator(struct pmap *pmap,
                                             struct slab_allocator *slab, size_t bytes)
{
    size_t pages = DIVIDE_ROUND_UP(bytes, BASE_PAGE_SIZE);

    struct pmap_vnode_mgmt *m = pmap->m;

    genvaddr_t vbase = m->vregion_offset;

    // Allocate and map buffer using base pages
    for (int i = 0; i < pages; i++) {
        struct capref cap;
        size_t retbytes;
        // Get page
        errval_t err = frame_alloc(&cap, BASE_PAGE_SIZE, &retbytes);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_FRAME_ALLOC);
        }
        assert(retbytes == BASE_PAGE_SIZE);

        // Map page
        genvaddr_t genvaddr = m->vregion_offset;
        m->vregion_offset += (genvaddr_t)BASE_PAGE_SIZE;
        assert(m->vregion_offset < vregion_get_base_addr(&m->vregion) +
                vregion_get_size(&m->vregion));

        err = do_map(pmap, genvaddr, cap, 0, BASE_PAGE_SIZE,
                VREGION_FLAGS_READ_WRITE, NULL, NULL);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_DO_MAP);
        }
    }

    /* Grow the slab */
    lvaddr_t buf = vspace_genvaddr_to_lvaddr(vbase);
    //debug_printf("%s: Calling slab_grow with %#zx bytes\n", __FUNCTION__, bytes);
    slab_grow(slab, (void*)buf, bytes);

    return SYS_ERR_OK;
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
bool debug_refill = false;
static errval_t refill_slabs(struct pmap *pmap, struct slab_allocator *slab, size_t request)
{
    errval_t err;

    struct pmap_vnode_mgmt *m = pmap->m;

    /* Keep looping till we have #request slabs */
    while (slab_freecount(slab) < request) {
        // Amount of bytes required for #request
        size_t slabs_req = request - slab_freecount(slab);
        size_t bytes = SLAB_STATIC_SIZE(slabs_req,
                                        slab->blocksize);
        bytes = ROUND_UP(bytes, BASE_PAGE_SIZE);

        if (debug_refill) {
        debug_printf("%s: req=%zu, bytes=%zu, slab->blocksize=%zu, slab->freecount=%zu\n",
                __FUNCTION__, slabs_req, bytes, slab->blocksize, slab_freecount(slab));
        }

        /* Get a frame of that size */
        struct capref cap;
        size_t retbytes = 0;
        err = frame_alloc(&cap, bytes, &retbytes);
        if (err_is_fail(err)) {
            if (err_no(err) == LIB_ERR_RAM_ALLOC_MS_CONSTRAINTS &&
                err_no(err_pop(err)) == LIB_ERR_RAM_ALLOC_WRONG_SIZE) {
                /* only retry with fixed allocator if we get
                 * LIB_ERR_RAM_ALLOC_WRONG_SIZE.
                 */
                return refill_slabs_fixed_allocator(pmap, slab, bytes);
            }
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_FRAME_ALLOC);
            }
        }
        bytes = retbytes;

        /* If we do not have enough slabs to map the frame in, recurse */
        size_t required_slabs_for_frame = max_slabs_required(bytes);
        // Here we need to check that we have enough vnode slabs, not whatever
        // slabs we're refilling
        if (slab_freecount(&m->slab) < required_slabs_for_frame) {
            if (debug_refill) {
                debug_printf("%s: called from %p -- need to recurse\n", __FUNCTION__,
                        __builtin_return_address(0));
            }
            // If we recurse, we require more slabs than to map a single page
            assert(required_slabs_for_frame > max_slabs_required(BASE_PAGE_SIZE));
            if (required_slabs_for_frame <= max_slabs_required(BASE_PAGE_SIZE)) {
                USER_PANIC(
                    "%s: cannot handle this recursion: required slabs = %zu > max slabs for a mapping (%zu)\n",
                    __FUNCTION__, required_slabs_for_frame, max_slabs_required(BASE_PAGE_SIZE));
            }

            err = refill_slabs(pmap, slab, required_slabs_for_frame);
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_SLAB_REFILL);
            }
        }

        /* Perform mapping */
        genvaddr_t genvaddr = m->vregion_offset;
        m->vregion_offset += (genvaddr_t)bytes;
        assert(m->vregion_offset < vregion_get_base_addr(&m->vregion) +
               vregion_get_size(&m->vregion));

        err = do_map(pmap, genvaddr, cap, 0, bytes,
                     VREGION_FLAGS_READ_WRITE, NULL, NULL);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_DO_MAP);
        }

        /* Grow the slab */
        lvaddr_t buf = vspace_genvaddr_to_lvaddr(genvaddr);
        slab_grow(slab, (void*)buf, bytes);
    }

    return SYS_ERR_OK;
}

errval_t refill_vnode_slabs(struct pmap *pmap, size_t count)
{
    return refill_slabs(pmap, &pmap->m->slab, count);
}

#ifdef PMAP_ARRAY
errval_t refill_pt_slabs(struct pmap *pmap, size_t count)
{
    return refill_slabs(pmap, &pmap->m->ptslab, count);
}
#endif

errval_t pmap_vnode_mgmt_current_init(struct pmap *pmap)
{
    // To reserve a block of virtual address space,
    // a vregion representing the address space is required.
    // We construct a superficial one here and add it to the vregion list.
    struct vregion *vregion = &pmap->m->vregion;
    vregion->vspace = NULL;
    vregion->memobj = NULL;
    vregion->base   = META_DATA_RESERVED_BASE;
    vregion->offset = 0;
    vregion->size   = META_DATA_RESERVED_SIZE;
    vregion->flags  = 0;
    vregion->next = NULL;

    struct vspace *vspace = pmap->vspace;
    assert(!vspace->head);
    vspace->head = vregion;

    pmap->m->vregion_offset = pmap->m->vregion.base;

    return SYS_ERR_OK;
}
