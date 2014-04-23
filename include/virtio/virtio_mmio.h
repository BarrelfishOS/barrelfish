/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_H
#define VIRTIO_H

#include <barrelfish/barrelfish.h>

/*
 * 2.1 Device Status Field
 */

/// The device is in the reset state (not discovered by the guest)
#define VIRTIO_DEVICE_STATUS_RESET       0x00

/// Guest OS has found the device and recognized it as a valid virtio device.
#define VIRTIO_DEVICE_STATUS_ACKNOWLEDGE 0x01

/// Guest OS knows how to drive the device i.e. recognized as valid virtio device.
#define VIRTIO_DEVICE_STATUS_DRIVER      0x02

/// Driver is set up and ready to drive the device.
#define VIRTIO_DEVICE_STATUS_DRIVER_OK   0x04

/// Driver has acknowledged all the features it understands
#define VIRTIO_DEVICE_STATUS_FEATURES_OK 0x08

/// Something went wrong in the guest, and it has given up on the device.
#define VIRTIO_DEVICE_STATUS_FAILED      0x80

/*
 * 5.0 Device Types
 * The following device IDs are used to identify different types of virtio
 * devices. Some device IDs are reserved for devices which are not currently
 * defined in this standard.
 */

/// Invalid device identifier
#define VIRTIO_DEVICE_TYPE_INVALID   0x00

/// Device type for network interface cards
#define VIRTIO_DEVICE_TYPE_NET       0x01

/// Device type for block devices
#define VIRTIO_DEVICE_TYPE_BLOCK     0x02

/// Device type for console devices
#define VIRTIO_DEVICE_TYPE_CONSOLE   0x03

/// Device type for entorpy devices
#define VIRTIO_DEVICE_TYPE_ENTORPY   0x04

//#define VIRTIO_DEVICE_TYPE_LEGACY_BALLOON 5

/// Device type for IO memory devices
#define VIRTIO_DEVICE_TYPE_IOMEM     0x06

/// Device type for rpmgs devices
#define VIRTIO_DEVICE_TYPE_RPMSG     0x07

/// Device type for SCSI host devices
#define VIRTIO_DEVICE_TYPE_SCSIHOST  0x08

/// Device type for 9P transport devices
#define VIRTIO_DEVICE_TYPE_9PTRANSP  0x09

/// Device type for MAC 802.11 WLAn devices
#define VIRTIO_DEVICE_TYPE_WLAN      0x0A

/// Device type for RPROC serial devices
#define VIRTIO_DEVICE_TYPE_SERIAL    0x0B

/// Device type for virtio CAIF devices
#define VIRTIO_DEVICE_TYPE_CAIF      0x0C

/// Device type for memory ballooning devices
#define VIRTIO_DEVICE_TYPE_BALLOON   0x0D

/// Device type for GPU devices
#define VIRTIO_DEVICE_TYPE_GPU       0x0E

/// Device type for timer / clock devices
#define VIRTIO_DEVICE_TYPE_TIMER     0x0F

/**
 * 2.4.5 The Virtqueue Descriptor Table
 *
 * Alignment Constraint: 16 byte boundary
 */
struct virtq_desc
{
    lpaddr_t addr;   ///< Address (guest-physical).
    uint32_t length;  ///< Length
    uint16_t flags;  ///< The flags, see VIRTQ_DESC_F_*
    uint16_t next;   ///< Next field if flags & NEXT
};

/// This marks a buffer as continuing via the next field.
#define VIRTQ_DESC_F_NEXT 0x01

/// This marks a buffer as device write-only (otherwise device read-only).
#define VIRTQ_DESC_F_WRITE 0x02

/// This means the buffer contains a list of buffer descriptors. */
#define VIRTQ_DESC_F_INDIRECT 0x04

/**
 * 2.4.5.3 Indirect Descriptors
 */
struct indirect_descriptor_table
{
    struct virtq_desc desc[];  ///< The actual descriptors (16 bytes each)
};

/// gets the number of Virtqueue descriptors of the table
#define VIRTQ_NUM_INDIRECT_DESCRIPTORS(vdesc) \
    ((vdesc)->length)/16;

/**
 * 2.4.6 The Virtqueue Available Ring
 *
 * The driver uses the available ring to offer buffers to the device.
 * It is only written by the driver and read by the device.
 *
 * Alignment Constraint: 2 byte boundary
 */

struct virtq_avail
{
    uint16_t flags;        ///< Ring flags for disabling interrupts
    uint16_t idx;          ///< where the driver would put the next descriptor
    uint16_t ring[];       ///< refers to a head of descriptor chain
    //uint16_t used_event;   ///< Only if VIRTIO_F_EVENT_IDX */
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
struct virtq_used
{
    uint16_t flags;                 ///< disabling the notification
    uint16_t idx;                   ///< where the driver would put next desc
    struct virtq_used_elem ring[];  ///< refers to a head of a descriptor chain
    //uint16_t avail_event;           ///< Only if VIRTIO_F_EVENT_IDX
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
struct virtq
{
    uint16_t num;               ///< the number of buffers in the queue
    struct virtq_desc *desc;    ///< the actual descriptors (16 bytes each)
    struct virtq_avail *avail;  ///< ring of available descriptor heads
    struct virtq_used *used;    ///< ring of used descriptor heads
    struct capref desc_cap;     ///< capability for the descriptor table
    struct capref avail_cap;    ///< capability for the available ring
    struct capref used_cap;     ///< capability for the used ring
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

static inline int
vring_need_event(uint16_t event_idx,
                 uint16_t new_idx,
                 uint16_t old_idx)
{
    return (uint16_t) (new_idx - event_idx - 1) < (uint16_t) (new_idx - old_idx);
}

/* Get location of event indices (only with VIRTIO_RING_F_EVENT_IDX) */
static inline uint16_t *
vring_used_event(struct vring *vr)
{
    /* For backwards compat, used event index is at *end* of avail ring. */
    return &vr->avail->ring[vr->num];
}

static inline uint16_t *
vring_avail_event(struct vring *vr)
{
    /* For backwards compat, avail event index is at *end* of used ring. */
    return (uint16_t *) &vr->used->ring[vr->num];
}

errval_t
virtq_alloc(struct virtq *vq,
            uint16_t num,
            uintptr_t align);

#endif // VIRTIO_H
