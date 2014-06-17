/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_DEVICES_VIRTIO_BLOCK_H
#define VIRTIO_DEVICES_VIRTIO_BLOCK_H

#include <dev/virtio/virtio_blk_dev.h>

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

/// the size of the VirtIO block device configuration spaces
#define VIRTIO_BLOCK_CONFIG_SIZE 0x20

/// the number of virtqueues for this device type
#define VIRTIO_BLOCK_NUM_VIRTQUEUES 1

#define VIRTIO_BLOCK_FLOUNDER_IFACE "vblk_host"

/*
 * --------------------------------------------------------------------------
 * 5.2.3 Feature Bits
 * --------------------------------------------------------------------------
 */

/// Request barriers are supported
#define VIRTIO_BLOCK_F_BARRIER    0

/// Maximum segment size is indicated in configuration structure
#define VIRTIO_BLOCK_F_SIZE_MAX   1

/// Maximum number of segments is indicated in configuration structure
#define VIRTIO_BLOCK_F_SEG_MAX    2

/// There is support for legacy geometry
#define VIRTIO_BLOCK_F_GEOMETRY   4

/// The disk is read only
#define VIRTIO_BLOCK_F_RO         5

/// The block size is indicated in configuration structure
#define VIRTIO_BLOCK_F_BLK_SIZE   6

/// SCSI command passthrough is supported
#define VIRTIO_BLOCK_F_SCSI       7

/// The cache flush command is supported
#define VIRTIO_BLOCK_F_FLUSH      9

/// the topology information is available
#define VIRTIO_BLOCK_F_TOPOLOGY   10

/// the length of the ID string
#define VIRTIO_BLOCK_ID_BYTES      20

/*
 * ---------------------------------------------------------------------------
 * Configuration Space
 * ---------------------------------------------------------------------------
 */

/**
 * \brief VirtIO block device topology information
 */
struct virtio_block_topology
{
    uint8_t num_logic_per_phys;  ///< number of logical blocks per physical
    uint8_t alignment_offset;   ///< alignment offset
    uint16_t min_io_size;       ///< suggested minimum IO size
    uint32_t opt_io_size;       ///< suggested maximum IO size
};

/**
 * \brief VirtIO block device geometry information
 */
struct virtio_block_geometry
{
    uint16_t cylinders;         ///< number of cylinders
    uint8_t heads;              ///< number of heads
    uint8_t sectors;            ///< number of sectors
};

/*
 * --------------------------------------------------------------------------
 * Device Configuration Layout
 * --------------------------------------------------------------------------
 */

/**
 * The device configuration layout as specified by 5.2.3.2
 */
struct virtio_block_config
{
    uint64_t capacity;          ///< the capacity in 512 byte sectors
    uint32_t size_max;          ///< the maximum segment size
    uint32_t seg_max;           ///< the maximum number of segments
    struct virtio_block_geometry geometry;                 ///< device geometry
    uint32_t blk_size;
    struct virtio_block_topology topology;                 ///< topology information
    uint8_t writeback;           ///< write back flag
}__attribute__((packed));

/*
 * --------------------------------------------------------------------------
 * Device Operations
 * --------------------------------------------------------------------------
 *
 * The driver queues requests to the virtqueue, and they are used by the device
 * (not necessarily in order).
 */

/*
 * VirtIO Block Command types
 */

/// Virtio Block Device Request Type IN
#define VIRTIO_BLOCK_T_IN     0x00000000

/// Virtio Block Device Request Type OUT
#define VIRTIO_BLOCK_T_OUT    0x00000001

/// Virtio Block Device Request Type Flush
#define VIRTIO_BLOCK_T_FLUSH  0x00000004

/// Get device ID command
#define VIRTIO_BLOCK_T_GET_ID 0x00000008

/// Memory barrier
#define VIRTIO_BLOCK_T_BARRIER 0x80000000

/*
 * VirtIO Status Field values
 */

/// Virtio Block Device Request Status OK
#define VIRTIO_BLOCK_S_OK     0x0

/// Virtio Block Device Request Status IO Error
#define VIRTIO_BLOCK_S_IOERR  0x1

/// Virtio Block Device Request Status Unsupported
#define VIRTIO_BLOCK_S_UNSUPP 0x2

/**
 * 5.2.7.1 Legacy Interface: Device Operation
 */
struct virtio_scsi_reqhdr
{
    uint32_t errors;
    uint32_t data_len;  ///< SHOULD be ignored by the driver.
    uint32_t sense_len;  ///< number of bytes actually written to the sense buffer.
    uint32_t residual;  ///< residual size, length - bytes actually transferred.
};

/**
 * Block device request as defined in 5.2.5 Device Operation
 */
struct virtio_block_reqhdr
{
    uint32_t type;          ///< Type of the request. One of VIRTIO_BLOCK_T_*
    uint32_t ioprio;        ///< Legacy devices only
    uint64_t sector;        ///< Offset (multiplied by 512) where to read/write
    /* the data and status follow */
};

/**
 * stores additional information for the VirtIO block device
 */
struct virtio_device_blk
{
    struct virtio_device *vdev;
    virtio_blk_t config_space;
    void *config_addr;
    struct virtqueue *vq;
};

/**
 * \brief reads the device configuration and copies it into the local memory
 *
 * \param dev the block device to read the configuration space.
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_block_config_read(struct virtio_device_blk *dev);

/**
 * \brief   returns the block size of the device
 *
 * \param   dev the virtio block device
 *
 * \returns block size in bytes
 */
static inline uint32_t virtio_block_get_block_size(struct virtio_device_blk *dev)
{
    if (!virtio_device_has_feature(dev->vdev, VIRTIO_BLOCK_F_BLK_SIZE)) {
        return 0;
    }

    return virtio_blk_block_size_size_rdf(&dev->config_space);
}

/**
 * \brief   returns the capacity of the VirtIO block device in 512 segments
 *
 * \param   dev the virtio block device
 *
 * \returns capacity in 512-byte sectors
 */
static inline uint64_t virtio_block_get_capacity(struct virtio_device_blk *dev)
{
    return virtio_blk_capacity_sectors_rdf(&dev->config_space);
}

/**
 * \brief   returns the maximum number of segments
 *
 * \param   dev the virtio block device
 *
 * \returns maximum number of segments if VIRTIO_BLOCK_F_SEG_MAX
 *          0 otherwise
 */
static inline uint32_t virtio_block_get_segment_num(struct virtio_device_blk *dev)
{
    if (!virtio_device_has_feature(dev->vdev, VIRTIO_BLOCK_F_SEG_MAX)) {
        return 0;
    }
    return virtio_blk_seg_num_max_rdf(&dev->config_space);
}

/**
 * \brief   returns the maximum segment size
 *
 * \param   dev the virtio block device
 *
 * \returns maximum segment size if VIRTIO_BLOCK_F_SEG_MAX
 *          0 otherwise
 */
static inline uint32_t virtio_block_get_segment_size(struct virtio_device_blk *dev)
{
    if (!virtio_device_has_feature(dev->vdev, VIRTIO_BLOCK_F_SIZE_MAX)) {
        return 0;
    }
    return virtio_blk_seg_size_max_rdf(&dev->config_space);
}

/**
 * \brief   returns the topology information
 *
 * \param   dev  the virtio block device
 * \param   topo memory region to fill the topology information in
 *               (only valid if VIRTIO_BLOCK_F_TOPOLOGY)
 *
 * \returns true if VIRTIO_BLOCK_F_TOPOLOGY
 *          false otherwise
 */
bool virtio_block_get_topology(struct virtio_device_blk *dev,
                               struct virtio_block_topology *topo);

/**
 * \brief   returns the blocksize of
 *
 * \param   dev the virtio block device
 * \param   geo memory region to fill the geometry information in
 *              (only valid if VIRTIO_BLOCK_F_GEOMETRY)
 *
 * \returns true if VIRTIO_BLOCK_F_GEOMETRY
 *          false otherwise
 */
bool virtio_block_get_geometry(struct virtio_device_blk *dev,
                               struct virtio_block_geometry *geo);

/**
 * \brief   handles the VirtIO block device common initialization.
 *
 * \param   dev     the VirtIO block device
 * \param   setup   the setup information
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_block_init_device(struct virtio_device_blk *dev,
                                  struct virtio_device_setup *setup);

/**
 * \brief   handles the VirtIO block device common initialization.
 *
 * \param   dev     the VirtIO block device
 * \param   setup   the setup information
 * \param   dev_cap the frame capability backing the device registers
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_block_init_device_with_cap(struct virtio_device_blk *dev,
                                           struct virtio_device_setup *setup,
                                           struct capref dev_cap);


/*
 * ----------------------------------------------------------------------------
 * Additional functions for the host
 * ----------------------------------------------------------------------------
 */

#ifdef __VIRTIO_HOST__
/**
 * stores additional information for the host side of the VirtIO
 * block device
 */
struct virtio_host_blk
{
    struct virtio_device *host;           ///< the vhost of the block device
    virtio_blk_t config_space;          ///< configuration space (mackerel)
    void *config_addr;                  ///< raw pointer to the config space
    void *config_tmp;                   ///< pointer to temporary config space
    struct virtio_block_config config;  ///< configuration space values
    struct virqueue_host *vq;           ///< the single virtqueue
};

/**
 * \brief writes the configuration space values to the device configuration space
 *
 * \param bdev  the VirtIO host block device
 *
 * \return SYS_ERR_OK on success
 */
errval_t virtio_block_host_write_config(struct virtio_host_blk *bdev);

#endif // __VIRTIO_HOST__

#endif // VIRTIO_DEVICES_VIRTIO_BLOCK_H
