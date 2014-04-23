/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_VIRTIO_BLOCK_H
#define VIRTIO_VIRTIO_BLOCK_H

#include <barrelfish/barrelfish.h>

/*
 * 5.2 Block Device
 * The virtio block device is a simple virtual block device (ie. disk). Read
 * and write requests (and other exotic requests) are placed in the queue, and
 * serviced (probably out of order) by the device except where noted.
 *
 * Device ID = 2
 *
 * There is only a single virtio IO queue for this device
 */

/*
 * --------------------------------------------------------------------------
 * 5.2.3 Feature Bits
 * --------------------------------------------------------------------------
 */

/// Request barriers are supported
#define VIRTIO_BLK_F_BARRIER    (1<<0)

/// Maximum segment size is indicated in configuration structure
#define VIRTIO_BLK_F_SIZE_MAX   (1<<1)

/// Maximum number of segments is indicated in configuration structure
#define VIRTIO_BLK_F_SEG_MAX    (1<<2)

/// There is support for legacy geometry
#define VIRTIO_BLK_F_GEOMETRY   (1<<4)

/// The disk is read only
#define VIRTIO_BLK_F_RO         (1<<5)

/// The block size is indicated in configuration structure
#define VIRTIO_BLK_F_BLK_SIZE   (1<<6)

/// SCSI command passthrough is supported
#define VIRTIO_BLK_F_SCSI       (1<<7)

/// The cache flush command is supported
#define VIRTIO_BLK_F_FLUSH      (1<<9)

/// the topology information is available
#define VIRTIO_BLK_F_TOPOLOGY   (1<<10)

/// the length of the ID string
#define VIRTIO_BLK_ID_BYTES      20

/*
 * --------------------------------------------------------------------------
 * Device Configuration Layout
 * --------------------------------------------------------------------------
 */

/**
 * The device configuration layout as specified by 5.2.3.2
 */
struct virtio_blk_config {
    uint64_t capacity;          ///< the capacity in 512 byte sectors
    uint32_t size_max;          ///< the maximum segment size
    uint32_t seg_max;           ///< the maximum number of segments
    struct virtio_blk_geometry {
        uint16_t cylinders;         ///< number of cylinders
        uint8_t heads;              ///< number of heads
        uint8_t sectors;            ///< number of sectors
    } geometry;                 ///< device geometry
    uint32_t  blk_size;
    struct virtio_blk_topology {
        uint8_t physical_block_exp; ///< num logical blocks per physical
        uint8_t alignment_offset;   ///< the alginment offset
        uint16_t min_io_size;       ///< suggested minimum IO size
        uint32_t opt_io_size;       ///< suggested maximum IO size
    } topology;                 ///< topology information
    uint8_t reserved;           ///< reserved field originally write back
} __attribute__((packed));



/*
 * --------------------------------------------------------------------------
 * Device Operations
 * --------------------------------------------------------------------------
 *
 * The driver queues requests to the virtqueue, and they are used by the device
 * (not necessarily in order).
 */

/**
 * Block device request as defined in 5.2.5 Device Operation
 */
struct virtio_blk_reqhdr {
    uint32_t type;          ///< Type of the request. One of VIRTIO_BLK_T_*
    uint32_t ioprio;        ///< Priviously called ioprio (legacy)
    uint64_t sector;        ///< Offset (multiplied by 512) where to read/write
    /* the data and status follow */
};

/// Virtio Block Device Request Type IN
#define VIRTIO_BLK_T_IN     0

/// Virtio Block Device Request Type OUT
#define VIRTIO_BLK_T_OUT    1

/// Virtio Block Device Request Type Flush
#define VIRTIO_BLK_T_FLUSH  4

/// Get device ID command
#define VIRTIO_BLK_T_GET_ID 8


/// Virtio Block Device Request Status OK
#define VIRTIO_BLK_S_OK     0

/// Virtio Block Device Request Status IO Error
#define VIRTIO_BLK_S_IOERR  1

/// Virtio Block Device Request Status Unsupported
#define VIRTIO_BLK_S_UNSUPP 2


/**
 * 5.2.7.1 Legacy Interface: Device Operation
 */
struct virtio_scsi_reqhdr {
    uint32_t errors;
    uint32_t data_len;  ///< SHOULD be ignored by the driver.
    uint32_t sense_len; ///< number of bytes actually written to the sense buffer.
    uint32_t residual;  ///<residual size, length - bytes actually transferred.
    uint8_t status;
};


#endif // VIRTIO_VIRTIO_BLOCK_H
