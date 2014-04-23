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

/**
 * 2.4.5 The Virtqueue Descriptor Table
 *
 * Alignment Constraint: 16 byte boundary
 */
struct vring_desc
{
    lpaddr_t addr;   ///< Address (guest-physical).
    uint32_t length; ///< Length
    uint16_t flags;  ///< The flags, see VIRTQ_DESC_F_*
    uint16_t next;   ///< Next field if flags & NEXT
};

/// This marks a buffer as continuing via the next field.
#define VIRTQ_DESC_F_NEXT       0x01

/// This marks a buffer as device write-only (otherwise device read-only).
#define VIRTQ_DESC_F_WRITE      0x02

/// This means the buffer contains a list of buffer descriptors. */
#define VIRTQ_DESC_F_INDIRECT   0x04

/**
 * 2.4.5.3 Indirect Descriptors
 */
struct indirect_vring_desc
{
    struct vring_desc desc[];  ///< The actual descriptors (16 bytes each)
};

/// gets the number of Virtqueue descriptors of the table
#define VIRTQ_NUM_INDIRECT(vdesc) \
    ((vdesc)->length)/16;

#define VIRTIO_MAX_INDIRECT ((uint32_t) (PAGE_SIZE / 16))

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
    uint16_t flags;        ///< Ring flags for disabling interrupts
    uint16_t idx;          ///< where the driver would put the next descriptor
    uint16_t ring[];       ///< refers to a head of descriptor chain
    /* Only if VIRTIO_F_EVENT_IDX uint16_t used_event;  */
};

/// disable interrupts on this queue
#define VIRTQ_AVAIL_F_NO_INTERRUPT 1

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
    uint16_t flags;                 ///< disabling the notification
    uint16_t idx;                   ///< where the driver would put next desc
    struct virtq_used_elem ring[];  ///< refers to a head of a descriptor chain
    /* Only if VIRTIO_F_EVENT_IDX uint16_t avail_event; */
};

struct virtq_used_elem
{
    uint32_t id;        ///< index of start of used descriptor chain
    uint32_t length;    ///< total length of descriptor chain
};

/// disable the notification
#define VIRTQ_USED_F_NO_NOTIFY 1

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
    struct capref desc_cap;     ///< capability for the descriptor table
    struct capref avail_cap;    ///< capability for the available ring
    struct capref used_cap;     ///< capability for the used ring
    struct capref buf_cap[];    ///< capabilities for the used buffers
};

/**
 * \brief Calculates the size of a virtqueue structure in memory aligned
 *
 * \param num   the queue size
 * \param align the alignment constraints
 */
static inline uintptr_t
virtq_size(uint16_t num,
           uintptr_t align)
{
    return ((sizeof(struct vring_desc) * num + sizeof(uint16_t) * (2 + num) + align
            - 1)
            & ~(align - 1))
           + sizeof(uint16_t) * 2 + sizeof(struct vring_used_elem) * num;
}

/**
 *
 */
static inline int
vring_need_event(uint16_t event_idx,
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
static inline uint16_t *
vring_used_event(struct vring *vr)
{
    return &vr->avail->ring[vr->num];
}

/**
 * \brief Returns the index of the available event (Only if  VIRTIO_F_EVENT_IDX)
 *
 * Note: This field is located at the very end of of the used ring data
 *       structure.
 */
static inline uint16_t *
vring_avail_event(struct vring *vr)
{
    return (uint16_t *) &vr->used->ring[vr->num];
}


/**
 * \brief allocates a new vring structure
 *
 * \param vr    pointer to the vring structure
 * \param num   the number of queue elements
 * \param align the alignment constraints for the vring
 */
errval_t
vring_alloc(struct vring *vr,
            uint16_t num,
            uintptr_t align);

/**
 * \brief frees the resources used by the vring structure
 */
errval_t
vring_free(struct vring *vr);

/**
 * \brief    Initializes a vring structure with the given caps
 *
 * \param vr The vring to initialize
 *
 * The capabilities must already been set in the vring structure. The caps
 * will be mapped into the vspace and the addresses set accordingly
 */
errval_t
vring_init(struct vring *vr);

#endif // VIRTIO_VIRTIO_RING_H
