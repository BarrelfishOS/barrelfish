/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <virtio/virtio_ring.h>


/*
 * We layout the vring structure in memory as follows:
 *
 * struct vring {
 *      // The actual descriptors (16 bytes each)
 *      struct vring_desc desc[num];
 *
 *      // A ring of available descriptor heads with free-running index.
 *      uint16_t avail_flags;
 *      uint16_t avail_idx;
 *      uint16_t available[num];
 *      uint16_t used_event_idx;
 *
 *      // Padding to the next align boundary.
 *      char pad[];
 *
 *      // A ring of used descriptor heads with free-running index.
 *      uint16_t used_flags;
 *      uint16_t used_idx;
 *      struct vring_used_elem used[num];
 *      uint16_t avail_event_idx;
 * };
 */

/**
 * \brief Maps the given capability and initializes the vring on the memory
 *        backed by the supplied capability
 *
 * \param vr    pointer to the vring structure to be initialized
 * \param num   the number of elements in the ring
 * \param align alignment constraints for the vring
 * \param cap   frame capability used as backing memory for the structure
 *
 * \return SYS_ERR_OK on success
 *         errno      on failure
 */
errval_t vring_init_from_cap(struct vring *vr,
                             uint16_t num,
                             uintptr_t align,
                             struct capref cap)
{
    errval_t err;

    /* num must be a power of two */
    assert(((num != 0) && ((num & (~num + 1)) == num)));

    size_t size = vring_size(num, align);

    struct frame_identity id;
    err = invoke_frame_identify(cap, &id);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_IDENTIFY);
    }

    /* check if we have enough space in the given cap */
    if ((1UL << id.bits) < size) {
        return SYS_ERR_INVALID_SIZE_BITS;
    }

    void *addr;
    err = vspace_map_one_frame(&addr, (1UL << id.bits), cap, NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    vring_init(vr, num, align, addr);

    return SYS_ERR_OK;
}

/**
 * \brief allocates a new vring structure
 *
 * \param vr        pointer to the vring structure
 * \param num       the number of queue elements
 * \param align     the alignment constraints for the vring
 * \param ret_frame returned frame capability
 *
 * \return SYS_ERR_OK on success
 *         errno      on failure
 */
errval_t vring_alloc(struct vring *vr,
                     uint16_t num,
                     uintptr_t align,
                     struct capref *ret_frame)
{
    errval_t err;

    /* num must be a power of two */
    assert(((num != 0) && ((num & (~num + 1)) == num)));

    size_t size = vring_size(num, align);

    struct capref frame;
    err = frame_alloc(&frame, size, &size);
    if (err_is_fail(err)) {
        return err;
    }

    err = vring_init_from_cap(vr, num, align, frame);
    if (err_is_fail(err)) {
        return err;
    }

    if (ret_frame) {
        *ret_frame = frame;
    }

    return SYS_ERR_OK;
}

/**
 * \brief frees the resources used by the vring structure
 *
 * \param vr the vring to be freed
 *
 * \return SYS_ERR_OK on success
 *         errno      on failure
 */
errval_t vring_free(struct vring *vr)
{
    errval_t err;

    err = vspace_unmap(vr->desc);
    if (err_is_fail(err)) {
        return err;
    }

    assert(!"NYI: returning the cap to the origin");
    return SYS_ERR_OK;
}

