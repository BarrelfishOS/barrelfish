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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>

#include <barrelfish/barrelfish.h>
#include "vspace_internal.h"

/**
 * \brief Map the memory object into a region
 *
 * \param memobj   The memory object
 * \param region  The region to add
 */
static errval_t map_region(struct memobj *memobj, struct vregion *vregion)
{
    struct memobj_append *append = (struct memobj_append*) memobj;

    /* make sure we are not overshooting the end */
    assert(memobj->size >= (vregion->offset + vregion->size));

    /* the vregion must start at the beginning of the memobj */
    if (vregion->offset != 0) {
        return LIB_ERR_MEMOBJ_MAP_REGION;
    }

    if (append->vregion) {
        return LIB_ERR_MEMOBJ_VREGION_ALREADY_MAPPED;
    }

    append->vregion = vregion;

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
    struct memobj_append *append = (struct memobj_append*) memobj;
    errval_t err;

    if (append->vregion != vregion) {
        return LIB_ERR_VSPACE_VREGION_NOT_FOUND;
    }

    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap = vspace_get_pmap(vspace);

    genvaddr_t vregion_base = vregion_get_base_addr(vregion);
    genvaddr_t vregion_offset = vregion_get_offset(vregion);

    err = pmap->f.unmap(pmap, vregion_base + vregion_offset, vregion->size,
                        NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_UNMAP);
    }

    append->vregion = NULL;

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
    struct memobj_append *append = (struct memobj_append *) memobj;

    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap = vspace_get_pmap(vspace);
    genvaddr_t base = vregion_get_base_addr(vregion);
    assert(vregion_get_offset(vregion) == 0);
    errval_t err;
    size_t ret_size;

    // short-circuit small requests
    if (range <= BASE_PAGE_SIZE) {
        err = pmap->f.modify_flags(pmap, base + offset, range,
                                   flags, &ret_size);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
        }
        return SYS_ERR_OK;
    } else if (offset == 0 && range > append->offsets[append->first_free_frame-1]) {
        // need to call modify_flags on every frame
        for (int i = 0; i < append->first_free_frame && range; i++) {
            size_t fsize = append->frame_sizes[i];
            size_t to_protect = fsize < range ? fsize : range;

            assert(to_protect > 0);
            err = pmap->f.modify_flags(pmap, base + offset, to_protect,
                                       flags, &ret_size);
            /*
            err = invoke_frame_modify_flags(append->frames[i], 0,
                    to_protect / BASE_PAGE_SIZE, flags, base + offset);
                    */
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
            }
            range -= ret_size;
            offset += ret_size;
        }
        return SYS_ERR_OK;
    } else {

#if 1
        // binary search
        int imin = 0;
        int imax = append->first_free_frame - 1;
        int first_idx = -1;
        int found = 0;
        while (!found && (imin <= imax)) {
            int imid = (imin + imax) / 2;
//            debug_printf("imin=%d, imid=%d, imax=%d\n", imin, imid, imax);
            genvaddr_t mid_offset = append->offsets[imid];
            if (mid_offset < offset) {
                imin = imid + 1;
            } else if (mid_offset > offset) {
                imax = imid - 1;
            } else {
                first_idx = imid;
                found = 1;
            }
        }
        if (!found) {
            first_idx = imax;
        }

#else
        // linear search
        int first_idx = -1;
        for (first_idx = 0;
             first_idx < append->first_free_frame && append->offsets[first_idx] < offset;
             first_idx++);
        // XXX Bug: Do not adjust if we find last frame
        first_idx -= 1;
#endif

#if 0
        debug_printf("offset -> %zx range -> %zx flags -> %zu\n", offset, range, (size_t) flags);
        for (int i = 0; i < append->first_free_frame; i++) {
            debug_printf("offsets[%d] = %"PRIxGENVADDR"\n", i, append->offsets[i]);
        }
        debug_printf("offset = %zx --> index = %d -> %zx, %zx\n", offset, first_idx,
                append->offsets[first_idx], first_idx < append->first_free_frame-1 ? append->offsets[first_idx+1] : 0x0);
#endif

        while(range) {
            size_t fsize = append->frame_sizes[first_idx] - (offset - append->offsets[first_idx]);
            size_t to_protect = fsize < range ? fsize : range;

            /// XXX: offset % BASE_PAGE_SIZE != 0?
            err = pmap->f.modify_flags(pmap, base + offset, to_protect,
                                       flags, &ret_size);
            /*
            err = invoke_frame_modify_flags(append->frames[i], 0,
                    to_protect / BASE_PAGE_SIZE, flags, base + offset);
                    */
            if (err_is_fail(err)) {
                return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
            }
            range -= ret_size;
            offset += ret_size;
            first_idx++;
        }
        //USER_PANIC("memobj_append: protect: searching for frame NYI\n");
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
}

/**
 * \brief Set a frame for an offset into the memobj
 *
 * \param memobj  The memory object
 * \param offset  Offset into the memory object
 * \param frame   The frame cap for the offset
 * \param frame_offset  The offset into the frame cap
 *
 * Pagefault relies on frames inserted in order
 */
static errval_t fill(struct memobj *memobj,
                     genvaddr_t offset,
                     struct capref frame,
                     size_t frame_offset)
{
    struct memobj_append *append = (struct memobj_append*) memobj;

    size_t slot = append->first_free_frame;
    assert(slot <= append->frame_count);

    // XXX: better error checking
    if (offset > 0 && offset < append->offsets[slot-1]) {
        return LIB_ERR_MEMOBJ_FILL;
    }

    if (slot == append->frame_count) {
        // grow arrays
        size_t new_count = append->frame_count * 2;
        void *f  = realloc(append->frames, new_count * sizeof(struct capref));
        void *o  = realloc(append->offsets, new_count * sizeof(genvaddr_t));
        void *s  = realloc(append->frame_sizes, new_count * sizeof(size_t));
        void *fo = realloc(append->frame_offsets, new_count * sizeof(size_t));
        if (!f || !o || !s || !fo) {
            return LIB_ERR_MALLOC_FAIL;
        }
        append->frames = f;
        append->offsets = o;
        append->frame_sizes = s;
        append->frame_offsets = fo;
        memset(&append->frames[append->first_free_frame],
                0, append->frame_count * sizeof(struct capref));
        memset(&append->offsets[append->first_free_frame],
                0, append->frame_count * sizeof(genvaddr_t));
        memset(&append->frame_sizes[append->first_free_frame],
                0, append->frame_count * sizeof(size_t));
        memset(&append->frame_offsets[append->first_free_frame],
                0, append->frame_count * sizeof(size_t));
        append->frame_count = new_count;
    }

    if (!capref_is_null((append->frames[slot]))) {
        return LIB_ERR_MEMOBJ_DUPLICATE_FILL;
    }

    struct frame_identity fi;
    errval_t err = frame_identify(frame, &fi);
    assert(err_is_ok(err));

    append->frames[slot] = frame;
    append->offsets[slot] = offset;
    append->frame_sizes[slot] = fi.bytes;
    append->frame_offsets[slot] = frame_offset;
    append->first_free_frame++;

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
    USER_PANIC("NYI\n");
    // this might be implemented as removing the last frame in the obj
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
    struct memobj_append *append = (struct memobj_append*) memobj;

    assert(!(offset % BASE_PAGE_SIZE));
    assert(offset == append->already_faulted);

    size_t first_unfaulted = append->first_unfaulted;

    if (capref_is_null(append->frames[first_unfaulted])) {
        return LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER;
    }

    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap = vspace_get_pmap(vspace);

    genvaddr_t base = vregion_get_base_addr(vregion);
    assert(vregion_get_offset(vregion) == 0);
    vregion_flags_t flags = vregion_get_flags(vregion);

    size_t retsize;
    err = pmap->f.map(pmap, base + offset, append->frames[first_unfaulted],
                      append->frame_offsets[first_unfaulted],
                      append->frame_sizes[first_unfaulted], flags,
                      NULL, &retsize);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_MAP);
    }
    // update metadata
    append->first_unfaulted++;
    append->already_faulted += retsize;

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
errval_t memobj_create_append(struct memobj_append *append,
                              size_t size,
                              memobj_flags_t flags)
{
    struct memobj *memobj = &append->m;

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

    memobj->type = MEMOBJ_APPEND;

    /* specific portion */
    append->vregion = NULL;

#define INIT_FRAME_COUNT 8
    size_t count = append->frame_count = INIT_FRAME_COUNT;
    append->first_free_frame = 0;
    append->first_unfaulted = 0;

    append->frames = malloc(count * sizeof(struct capref));
    if (!append->frames) {
        return LIB_ERR_MALLOC_FAIL;
    }
    memset(append->frames, 0, count * sizeof(struct capref));

    append->offsets = malloc(count * sizeof(genvaddr_t));
    if (!append->offsets) {
        free(append->frames);
        return LIB_ERR_MALLOC_FAIL;
    }
    memset(append->offsets, 0, count * sizeof(genvaddr_t));

    append->frame_sizes = malloc(count * sizeof(size_t));
    if (!append->frame_sizes) {
        free(append->frames);
        free(append->offsets);
        return LIB_ERR_MALLOC_FAIL;
    }
    memset(append->frame_sizes, 0, count * sizeof(size_t));

    append->frame_offsets = malloc(count * sizeof(size_t));
    if (!append->frame_offsets) {
        free(append->frames);
        free(append->offsets);
        free(append->frame_sizes);
        return LIB_ERR_MALLOC_FAIL;
    }
    memset(append->frame_offsets, 0, count * sizeof(size_t));

    return SYS_ERR_OK;
}

/**
 * \brief Destroy the object
 *
 */
errval_t memobj_destroy_append(struct memobj *memobj)
{
    struct memobj_append *m = (struct memobj_append *) memobj;

    errval_t err = SYS_ERR_OK;

    err = vregion_destroy(m->vregion);
    free(m->frames);
    free(m->offsets);
    free(m->frame_sizes);
    return err;
}
