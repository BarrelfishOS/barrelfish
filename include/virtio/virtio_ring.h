/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_VIRTIO_RING_H
#define VIRTIO_VIRTIO_RING_H

#include <barrelfish/barrelfish.h>

/*
 * This file contains the Virtio VRing description as defined in the VirtIO
 * specification.
 *
 * Extracted from the Virtio Specification 1.0
 * http://docs.oasis-open.org/virtio/virtio/v1.0/virtio-v1.0.pdf
 */

/**
 * 2.4.5 The Virtqueue Descriptor Table
 *
 * The descriptor table refers to the buffers the driver is using for the device.
 *
 * Alignment Constraint: 16 byte boundary
 */
struct vring_desc
{
    lpaddr_t addr;   ///< Address (guest-physical).
    uint32_t length; ///< Length of the data in the buffer
    uint16_t flags;  ///< The flags, see VRING_DESC_F_*
    uint16_t next;   ///< Next field used for chaining if flags & NEXT
};

/// This marks a buffer as continuing via the next field.
#define VIRTIO_RING_DESC_F_NEXT       0x01

/// This marks a buffer as device write-only (otherwise device read-only).
#define VIRTIO_RING_DESC_F_WRITE      0x02

/// This means the buffer contains a list of buffer descriptors.
#define VIRTIO_RING_DESC_F_INDIRECT   0x04



/**
 * 2.4.5.3 Indirect Descriptors
 */
struct indirect_vring_desc
{
    struct vring_desc desc[0];  ///< The actual descriptors (16 bytes each)
};

/// gets the number of Virtqueue descriptors of the table
#define VIRTIO_RING_NUM_INDIRECT(vdesc) \
    ((vdesc)->length)/16;

#define VIRTIO_RING_MAX_INDIRECT ((uint32_t) (BASE_PAGE_SIZE / 16))

/**
 * 2.4.6 The Virtqueue Available Ring
 *
 * The driver uses the available ring to offer buffers to the device.
 * It is only written by the driver and read by the device.
 *
 * Alignment Constraint: 2 byte boundary
 */

struct vring_avail
{
    uint16_t flags;        ///< Ring flags see VRING_AVAIL_F_*
    uint16_t idx;          ///< where the driver would put the next descriptor
    uint16_t ring[0];       ///< refers to a head of descriptor chain
    /* Only if VIRTIO_RING_F_EVENT_IDX uint16_t used_event;  */
};

/// disable sending interrupts when the host consumes a buffer
#define VIRTIO_RING_AVAIL_F_NO_INTERRUPT 1

/**
 * is an element of the used ring
 */
struct vring_used_elem
{
    uint32_t id;        ///< index of start of used descriptor chain
    uint32_t length;    ///< total length of descriptor chain
};

/**
 * 2.4.8 The Virtqueue Used Ring
 *
 * The used ring is where the device returns buffers once it is done with them:
 * it is only written to by the device, and read by the driver.
 *
 * Alignment Constraint: 4 byte boundary
 */
struct vring_used
{
    uint16_t flags;                 ///< see VRING_USED_F*
    uint16_t idx;                   ///< where the driver would put next desc
    struct vring_used_elem ring[0]; ///< refers to a head of a descriptor chain
    /* Only if VIRTIO_RING_F_EVENT_IDX uint16_t avail_event; */
};



/// disable the notification when the guest adds a buffer
#define VIRTIO_RING_USED_F_NO_NOTIFY 1

/**
 * 2.4 Virtqueues
 *
 * Queue Size corresponds to the maximum number of buffers in the virtqueue.
 * Queue Size value is always a power of 2. The maximum Queue Size value is 32768.
 * This value is specified in a bus-specific way.
 */
struct vring
{
    uint16_t num;               ///< the number of buffers in the queue
    struct vring_desc *desc;    ///< the actual descriptors (16 bytes each)
    struct vring_avail *avail;  ///< ring of available descriptor heads
    struct vring_used *used;    ///< ring of used descriptor heads
};

/**
 * \brief Calculates the size of a vring structure in memory aligned
 *
 * \param num   the queue size
 * \param align the alignment constraints
 */
static inline size_t vring_size(uint16_t num, uintptr_t align)
{
    // calcualte the size of the descriptor table
    size_t size = num * sizeof(struct vring_desc);

    // calculate the size of the available ring:
    // flags + idx + num * ring + used_event
    size += sizeof(uint16_t) * (2 + num + 1);

    // do the alignment
    size = (size + align - 1) & ~(align - 1);

    // calculate the size of the used ring:
    // flags + idx + num *  used_element + avail_event
    size += sizeof(uint16_t) * 3 + sizeof(struct vring_used_elem) * num;

    return size;
}

/**
 *
 */
static inline uint16_t vring_need_event(uint16_t event_idx,
                                        uint16_t new_idx,
                                        uint16_t old_idx)
{
    return (uint16_t) (new_idx - event_idx - 1) < (uint16_t) (new_idx - old_idx);
}

/**
 * \brief Returns the index of the used event (Only if  VIRTIO_F_EVENT_IDX)
 *
 * Note: This field is located at the very end of of the available ring data
 *       structure.
 */
static inline uint16_t *vring_get_used_event(struct vring *vr)
{
    return &vr->avail->ring[vr->num];
}

/**
 * \brief Returns the index of the available event (Only if  VIRTIO_F_EVENT_IDX)
 *
 * Note: This field is located at the very end of of the used ring data
 *       structure.
 */
static inline uint16_t *vring_get_avail_event(struct vring *vr)
{
    return (uint16_t *) &vr->used->ring[vr->num];
}

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
 * \brief Initializes a vring structure
 *
 * \param vr    vring structure to initialize
 * \param num   the number of vring descriptors
 * \param addr  pointer to a contiguous memory range for the rings
 * \param align alignment constraints for the vring
 *
 */
static inline void vring_init(struct vring *vr,
                              uint16_t num,
                              uintptr_t align,
                              void *addr)
{
    /* num must be a power of two */
    assert(((num != 0) && ((num & (~num + 1)) == num)));

    uintptr_t p = (uintptr_t)addr;

    vr->num = num;
    vr->desc = (struct vring_desc *) p;

    vr->avail = (struct vring_avail *) (p + num * sizeof(struct vring_desc));

    p = (uintptr_t)&vr->avail->ring[num];
    vr->used = (void *) ((p + align-1) & ~(align-1));

}

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
                             struct capref cap);

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
                     struct capref *ret_frame);

/**
 * \brief frees the resources used by the vring structure
 *
 * \param vr the vring to be freed
 *
 * \return SYS_ERR_OK on success
 *         errno      on failure
 */
errval_t vring_free(struct vring *vr);


#endif // VIRTIO_VIRTIO_RING_H
