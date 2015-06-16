/**
 * \file
 * \brief memory object of anonymous type.
 * The object maintains a list of frames.
 *
 * The object maintains a list of frames and a list of vregions.
 * The lists are backed by slabs.
 * The slabs may have to be grown,
 * in which case the object will use #vspace_pinned_alloc.
 *
 * morecore uses this memory object so it cannot use malloc for its lists.
 * Therefore, this uses slabs and grows them using the pinned memory.
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>

#include <barrelfish/barrelfish.h>
#include "vspace_internal.h"

#define CHECK_FRAME_SIZE 0


/**
 * \brief Map the memory object into a region
 *
 * \param memobj   The memory object
 * \param region  The region to add
 */
static errval_t map_region(struct memobj *memobj, struct vregion *vregion)
{
    struct memobj_numa *mo_numa = (struct memobj_numa*) memobj;

    /* make sure we are not overshooting the end */
    assert(memobj->size >= (vregion->offset + vregion->size));

    /* the vregion must start at one of the backed frames */
    if (vregion->offset % mo_numa->stride) {
        return LIB_ERR_MEMOBJ_MAP_REGION;
    }

    if (mo_numa->vregion) {
        return LIB_ERR_MEMOBJ_VREGION_ALREADY_MAPPED;
    }

    mo_numa->vregion = vregion;

    return SYS_ERR_OK;
}

/**
 * \brief Unmap the memory object from a region
 *
 * \param memobj   The memory object
 * \param region  The region to remove
 */
static errval_t unmap_region(struct memobj *memobj, struct vregion *vregion)
{
    struct memobj_numa *mo_numa = (struct memobj_numa*) memobj;
    errval_t err;

    if (mo_numa->vregion != vregion) {
        return LIB_ERR_VSPACE_VREGION_NOT_FOUND;
    }

    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap = vspace_get_pmap(vspace);

    genvaddr_t vreg_base = vregion_get_base_addr(vregion);
    genvaddr_t vreg_offset = vregion_get_offset(vregion);

    err = pmap->f.unmap(pmap, vreg_base + vreg_offset, vregion->size, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_UNMAP);
    }

    mo_numa->vregion = NULL;

    return SYS_ERR_OK;
}

/**
 * \brief Set the protection on a range
 *
 * \param memobj  The memory object
 * \param region  The vregion to modify the mappings on
 * \param offset  Offset into the memory object
 * \param range   The range of space to set the protection for
 * \param flags   The protection flags
 */
static errval_t protect(struct memobj *memobj,
                        struct vregion *vregion,
                        genvaddr_t offset,
                        size_t range,
                        vs_prot_flags_t flags)
{
    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap = vspace_get_pmap(vspace);
    genvaddr_t base = vregion_get_base_addr(vregion);
    genvaddr_t vregion_offset = vregion_get_offset(vregion);
    errval_t err;
    size_t ret_size;
    err = pmap->f.modify_flags(pmap, base + offset + vregion_offset, range,
                               flags, &ret_size);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Pin a range
 *
 * \param memobj  The memory object
 * \param region  The vregion to modify the state on
 * \param offset  Offset into the memory object
 * \param range   The range of space to pin
 */
static errval_t pin(struct memobj *memobj,
                    struct vregion *vregion,
                    genvaddr_t offset,
                    size_t range)
{
    USER_PANIC("NYI");

    return SYS_ERR_OK;
}

/**
 * \brief Unpin a range
 *
 * \param memobj  The memory object
 * \param region  The vregion to modify the state on
 * \param offset  Offset into the memory object
 * \param range   The range of space to unpin
 */
static errval_t unpin(struct memobj *memobj,
                      struct vregion *vregion,
                      genvaddr_t offset,
                      size_t range)
{
    USER_PANIC("NYI");

    return SYS_ERR_OK;
}

/**
 * \brief Set a frame for an offset into the memobj
 *
 * \param memobj  The memory object
 * \param offset  Offset into the memory object
 * \param frame   The frame cap for the offset
 * \param offset  The offset into the frame cap
 *
 * Pagefault relies on frames inserted in order
 */
static errval_t fill(struct memobj *memobj,
                     genvaddr_t offset,
                     struct capref frame,
                     size_t frame_offset)
{
    struct memobj_numa *mo_numa = (struct memobj_numa*) memobj;

    /* we take a single capability per node using offset as the node ID */
    if (offset >= mo_numa->node_count) {
        return LIB_ERR_MEMOBJ_WRONG_OFFSET;
    }

    /* check if we already have a capability for that node */
    if (!capref_is_null((mo_numa->frames[offset]))) {
        return LIB_ERR_MEMOBJ_DUPLICATE_FILL;
    }

#if CHECK_FRAME_SIZE
    errval_t err;

    /* we must make sure the frame is big enough */
    struct frame_identity fid;

    err = invoke_frame_identify(frame, &fid);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_FILL);
    }

    if ((1UL << fid.bits) < memobj->size / mo_numa->node_count) {\
        return LIB_ERR_MEMOBJ_FILL;
    }

#endif

    mo_numa->frames[offset] = frame;

    return SYS_ERR_OK;
}

/**
 * \brief Unmap/remove one frame from the end of the memobj
 *
 * \param memobj     The memory object
 * \param offset     The offset from which to remove a frame from
 * \param ret_frame  Pointer to return the removed frame
 *
 */
static errval_t unfill(struct memobj *memobj,
                       genvaddr_t offset,
                       struct capref *ret_frame,
                       genvaddr_t *ret_offset)
{
    struct memobj_numa *mo_numa = (struct memobj_numa*) memobj;

    /* we take a single capability per node using offset as the node ID */
    if (offset >= mo_numa->node_count) {
        return LIB_ERR_MEMOBJ_WRONG_OFFSET;
    }

    /* check if we already have a capability for that node */
    if (capref_is_null((mo_numa->frames[offset]))) {
        return LIB_ERR_MEMOBJ_UNFILL_TOO_HIGH_OFFSET;
    }

    if (ret_frame) {
        *ret_frame = mo_numa->frames[offset];
    }
    mo_numa->frames[offset] = NULL_CAP;

    return SYS_ERR_OK;
}


/**
 * \brief Page fault handler
 *
 * \param memobj  The memory object
 * \param region  The associated vregion
 * \param offset  Offset into memory object of the page fault
 * \param type    The fault type
 *
 * Locates the frame for the offset and maps it in.
 * Relies on fill inserting frames in order.
 */
static errval_t pagefault(struct memobj *memobj,
                          struct vregion *vregion,
                          genvaddr_t offset,
                          vm_fault_type_t type)
{
    errval_t err;

    struct memobj_numa *mo_numa = (struct memobj_numa*) memobj;

    assert(mo_numa->vregion == vregion);

    size_t size = memobj->size;

    for (uint32_t i = 0; i < mo_numa->node_count; ++i) {
        if (capref_is_null(mo_numa->frames[i])) {
            return LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER;
        }
    }

    uint32_t node = 0;

    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap = vspace_get_pmap(vspace);
    genvaddr_t base = vregion_get_base_addr(vregion);
    genvaddr_t vregion_offset = vregion_get_offset(vregion);
    vregion_flags_t flags = vregion_get_flags(vregion);

    lvaddr_t map_offset = 0;
    lvaddr_t map_size = mo_numa->stride;

    while(size) {
        if (size < map_size) {
            map_size = size;
        }

        //debug_printf("mapping: vaddr=%lx, node=%u, frameoffset=%lx\n",
        //             base + vregion_offset + offset, node, map_offset);

        /*
         * create a copy of the capability to map it as a capability can only
         * be mapped once
         */
        struct capref copy;
        err = slot_alloc(&copy);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        }

        err = cap_copy(copy, mo_numa->frames[node]);
        if (err_is_fail(err)){
            return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        }

        /* map the copy */
        err = pmap->f.map(pmap, base + vregion_offset + offset, copy,
                          map_offset, map_size, flags, NULL, NULL);
        if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_MAP);
        }

        ++node;
        size -= map_size;
        offset += map_size;

        if (node == mo_numa->node_count) {
            node = 0;
            map_offset += mo_numa->stride;
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Free up some pages by placing them in the backing storage
 *
 * \param memobj      The memory object
 * \param size        The amount of space to free up
 * \param frames      An array of capref frames to return the freed pages
 * \param num_frames  The number of frames returned
 *
 * This will affect all the vregions that are associated with the object
 */
static errval_t pager_free(struct memobj *memobj,
                           size_t size,
                           struct capref *frames,
                           size_t num_frames)
{
    USER_PANIC("NYI");

    return SYS_ERR_OK;
}

/**
 * \brief Initialize
 *
 * \param memobj  The memory object
 * \param size    Size of the memory region
 * \param flags   Memory object specific flags
 *
 * This object handles multiple frames.
 * The frames are mapped in on demand.
 */
errval_t memobj_create_numa(struct memobj_numa *mo_numa,
                            size_t size,
                            memobj_flags_t flags,
                            size_t node_count,
                            size_t stride)
{
    struct memobj *memobj = &mo_numa->m;

    assert((stride % BASE_PAGE_SIZE)==0);

    /* Generic portion */
    memobj->f.map_region = map_region;
    memobj->f.unmap_region = unmap_region;
    memobj->f.protect = protect;
    memobj->f.pin = pin;
    memobj->f.unpin = unpin;
    memobj->f.fill = fill;
    memobj->f.unfill = unfill;
    memobj->f.pagefault = pagefault;
    memobj->f.pager_free = pager_free;

    memobj->size = size;
    memobj->flags = flags;
    memobj->type = MEMOBJ_NUMA;

    /* specific portion */
    mo_numa->node_count = node_count;
    mo_numa->stride = stride;
    mo_numa->vregion = NULL;

    mo_numa->frames = calloc(node_count, sizeof(struct capref));
    if (mo_numa->frames == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Destroy the object
 *
 */
errval_t memobj_destroy_numa(struct memobj *memobj)
{
    struct memobj_numa *mo_numa = (struct memobj_numa *) memobj;

    errval_t err = SYS_ERR_OK;

    struct vregion *vregion = mo_numa->vregion;

    err = vregion_destroy(vregion);
    if (mo_numa->frames) {
        free(mo_numa->frames);
    }

    return err;
}
