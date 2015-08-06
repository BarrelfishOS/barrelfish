/**
 * \file
 * \brief Creates mapping based on the MMU type
 *
 * If the MMU supports translations, then anonymous type is used
 * (more efficient) else non contiguous memory is used.
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * Copyright (c) 2014, HP Labs.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/vspace_mmu_aware.h>
#include <barrelfish/core_state.h>
#include <string.h>

/// Minimum free memory before we return it to memory server
#define MIN_MEM_FOR_FREE        (1 * 1024 * 1024)

void vspace_mmu_aware_set_slot_alloc(struct vspace_mmu_aware *state,
                                     struct slot_allocator *slot_allocator)
{
    if (slot_allocator) {
        state->slot_alloc = slot_allocator;
    } else {
        state->slot_alloc = get_default_slot_allocator();
    }
}

/**
 * \brief Initialize vspace_mmu_aware struct
 *
 * \param state   The struct to initialize
 * \param init    The buffer to use to initialize the struct
 * \param size    The size of anon memobj to create
 *
 * Initializes the struct according to the type of MMU
 */
errval_t vspace_mmu_aware_init(struct vspace_mmu_aware *state, size_t size)
{
    return vspace_mmu_aware_init_aligned(state, NULL, size, 0,
            VREGION_FLAGS_READ_WRITE);
}

errval_t vspace_mmu_aware_init_aligned(struct vspace_mmu_aware *state,
                                       struct slot_allocator *slot_allocator,
                                       size_t size, size_t alignment,
                                       vregion_flags_t flags)
{
    state->size = size;
    state->consumed = 0;
    state->alignment = alignment;

    if (slot_allocator) {
        state->slot_alloc = slot_allocator;
    } else {
        state->slot_alloc = get_default_slot_allocator();
    }

    errval_t err;

    size = ROUND_UP(size, BASE_PAGE_SIZE);
    err = memobj_create_anon(&state->memobj, size, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_CREATE_ANON);
    }

    err = vregion_map_aligned(&state->vregion, get_current_vspace(),
                              &state->memobj.m, 0, size,
                              flags, alignment);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VREGION_MAP);
    }
    state->offset = state->mapoffset = 0;

    return SYS_ERR_OK;
}

/**
 * \brief Create mappings
 *
 * \param state     The object metadata
 * \param frame     An empty slot to place the frame capability in
 * \param req_size  The required amount by the application
 * \param retbuf    Pointer to return the mapped buffer
 * \param retsize   The actual size returned
 *
 * This function will returns a special error code if frame_create
 * fails due to the constrains to the memory server (amount of memory
 * or region of memory). This is to facilitate retrying with different
 * constraints.
 */
errval_t vspace_mmu_aware_map(struct vspace_mmu_aware *state, size_t req_size,
                              void **retbuf, size_t *retsize)
{
    errval_t err;

    struct capref frame;

    // Calculate how much still to map in
    size_t origsize = req_size;
    assert(state->mapoffset >= state->offset);
    if(state->mapoffset - state->offset > req_size) {
        req_size = 0;
    } else {
        req_size -= state->mapoffset - state->offset;
    }
    size_t alloc_size = ROUND_UP(req_size, BASE_PAGE_SIZE);
    size_t ret_size = 0;

    if (req_size > 0) {
#if __x86_64__
        if ((state->vregion.flags & VREGION_FLAGS_HUGE) &&
            (state->mapoffset & HUGE_PAGE_MASK) == 0)
        {
            // this is an opportunity to switch to 1G pages if requested.
            // we know that we can use large pages without jumping through hoops
            // if state->vregion.flags has VREGION_FLAGS_HUGE set and
            // mapoffset is aligned to at least HUGE_PAGE_SIZE.
            alloc_size = ROUND_UP(req_size, HUGE_PAGE_SIZE);

            // goto allocation directly so we can avoid nasty code interaction
            // between #if __x86_64__ and the size checks, we want to be able
            // to use 2M pages on x86_64 also. -SG, 2015-04-30.
            goto allocate;
        }
#endif
        if ((state->vregion.flags & VREGION_FLAGS_LARGE) &&
            (state->mapoffset & LARGE_PAGE_MASK) == 0)
        {
            // this is an opportunity to switch to 2M pages if requested.
            // we know that we can use large pages without jumping through hoops
            // if state->vregion.flags has VREGION_FLAGS_LARGE set and
            // mapoffset is aligned to at least LARGE_PAGE_SIZE.
            alloc_size = ROUND_UP(req_size, LARGE_PAGE_SIZE);
        }
        // Create frame of appropriate size
allocate:
        err = state->slot_alloc->alloc(state->slot_alloc, &frame);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_SLOT_ALLOC_NO_SPACE);
        }

        err = frame_create(frame, alloc_size, &ret_size);
        if (err_is_fail(err)) {
            if (err_no(err) == LIB_ERR_RAM_ALLOC_MS_CONSTRAINTS) {
                // we can only get 4k frames for now; retry with 4k
                if (alloc_size > BASE_PAGE_SIZE && req_size <= BASE_PAGE_SIZE) {
                    alloc_size = BASE_PAGE_SIZE;
                    goto allocate;
                }
                return err_push(err, LIB_ERR_FRAME_CREATE_MS_CONSTRAINTS);
            }
            return err_push(err, LIB_ERR_FRAME_CREATE);
        }
        assert(ret_size >= req_size);
        origsize += ret_size - req_size;
        req_size = ret_size;

        if (state->consumed + req_size > state->size) {
            err = cap_delete(frame);
            if (err_is_fail(err)) {
                debug_err(__FILE__, __func__, __LINE__, err,
                          "cap_delete failed");
            }
            state->slot_alloc->free(state->slot_alloc, frame);
            return LIB_ERR_VSPACE_MMU_AWARE_NO_SPACE;
        }

        // Map it in
        err = state->memobj.m.f.fill(&state->memobj.m, state->mapoffset, frame,
                                     req_size);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_FILL);
        }
        err = state->memobj.m.f.pagefault(&state->memobj.m, &state->vregion,
                                          state->mapoffset, 0);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        }
    }

    // Return buffer
    genvaddr_t gvaddr = vregion_get_base_addr(&state->vregion) + state->offset;
    *retbuf = (void*)vspace_genvaddr_to_lvaddr(gvaddr);
    *retsize = origsize;
    state->mapoffset += req_size;
    state->offset += origsize;
    state->consumed += origsize;

    return SYS_ERR_OK;
}

errval_t vspace_mmu_aware_reset(struct vspace_mmu_aware *state,
                                struct capref frame, size_t size)
{
    errval_t err;
    struct vregion *vregion;
    struct capref oldframe;
    void *vbuf;

    // create copy of new region
    err = slot_alloc(&oldframe);
    if (err_is_fail(err)) {
        return err;
    }
    err = cap_copy(oldframe, frame);
    if (err_is_fail(err)) {
        return err;
    }
    err = vspace_map_one_frame_attr_aligned(&vbuf, size, oldframe,
            VREGION_FLAGS_READ_WRITE | VREGION_FLAGS_LARGE, LARGE_PAGE_SIZE,
            NULL, &vregion);
    if (err_is_fail(err)) {
        return err;
    }
    // copy over data to new frame
    genvaddr_t gen_base = vregion_get_base_addr(&state->vregion);
    memcpy(vbuf, (void*)(lvaddr_t)gen_base, state->mapoffset);

    err = vregion_destroy(vregion);
    if (err_is_fail(err)) {
        return err;
    }

    genvaddr_t offset = 0;
    // Unmap backing frames for [0, size) in state.vregion
    do {
        err = state->memobj.m.f.unfill(&state->memobj.m, 0, &oldframe,
                &offset);
        if (err_is_fail(err) &&
            err_no(err) != LIB_ERR_MEMOBJ_UNFILL_TOO_HIGH_OFFSET)
        {
            return err_push(err, LIB_ERR_MEMOBJ_UNMAP_REGION);
        }
        struct frame_identity fi;
        // increase address
        err = invoke_frame_identify(oldframe, &fi);
        if (err_is_fail(err)) {
            return err;
        }
        offset += (1UL<<fi.bits);
        err = cap_destroy(oldframe);
        if (err_is_fail(err)) {
            return err;
        }
    } while(offset < state->mapoffset);

    // Map new frame in
    err = state->memobj.m.f.fill(&state->memobj.m, 0, frame, size);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_FILL);
    }
    err = state->memobj.m.f.pagefault(&state->memobj.m, &state->vregion, 0, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
    }

    state->mapoffset = size;
    return SYS_ERR_OK;
}

errval_t vspace_mmu_aware_unmap(struct vspace_mmu_aware *state,
                                lvaddr_t base, size_t bytes)
{
    errval_t err;
    struct capref frame;
    genvaddr_t gvaddr = vregion_get_base_addr(&state->vregion) + state->offset;
    lvaddr_t eaddr = vspace_genvaddr_to_lvaddr(gvaddr);
    genvaddr_t offset;
    genvaddr_t gen_base = vspace_lvaddr_to_genvaddr(base)
        - vregion_get_base_addr(&state->vregion);
    genvaddr_t min_offset = 0;
    bool success = false;

    assert(vspace_lvaddr_to_genvaddr(base) >= vregion_get_base_addr(&state->vregion));
    assert(base + bytes == (lvaddr_t)eaddr);

    assert(bytes <= state->consumed);
    assert(bytes <= state->offset);

    // Reduce offset
    state->offset -= bytes;
    state->consumed -= bytes;

    // Free only in bigger blocks
    if(state->mapoffset - state->offset > MIN_MEM_FOR_FREE) {
        do {
            // Unmap and return (via unfill) frames from base
            err = state->memobj.m.f.unfill(&state->memobj.m, gen_base,
                                           &frame, &offset);
            if(err_is_fail(err) && err_no(err) != LIB_ERR_MEMOBJ_UNFILL_TOO_HIGH_OFFSET) {
                return err_push(err, LIB_ERR_MEMOBJ_UNMAP_REGION);
            }

            // Delete frame cap
            if(err_is_ok(err)) {
                success = true;
                if (min_offset == 0 || min_offset > offset) {
                    min_offset = offset;
                }

                err = cap_destroy(frame);
                if(err_is_fail(err)) {
                    return err;
                }
            }
        } while(err != LIB_ERR_MEMOBJ_UNFILL_TOO_HIGH_OFFSET);

//    state->consumed -= bytes;
        if (success) {
            state->mapoffset = min_offset;
        }
    }

    return SYS_ERR_OK;
}
