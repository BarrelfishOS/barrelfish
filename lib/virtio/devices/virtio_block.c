/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <virtio/virtio.h>
#include <virtio/virtio_device.h>
#include <virtio/devices/virtio_block.h>

#include <dev/virtio/virtio_blk_dev.h>

#include "device.h"
#include "debug.h"

/**
 * \brief   returns the topology information
 *
 * \param   dev  the virtio block device
 * \param   topo memory region to fill the topology information in
 *               (only valid if VIRTIO_BLK_F_TOPOLOGY)
 *
 * \returns true if VIRTIO_BLK_F_TOPOLOGY
 *          false otherwise
 */
bool virtio_block_get_topology(struct virtio_device_blk *dev,
                               struct virtio_block_topology *topo)
{
    /* can't return any data */
    if (!topo) {
        return 0;
    }

    /* the device does not support topology */
    if (!virtio_device_has_feature(dev->vdev, VIRTIO_BLOCK_F_TOPOLOGY)) {
        return 0;
    }

    topo->alignment_offset = virtio_blk_topo_blocks_offset_aligned_rdf(&dev
                    ->config_space);
    topo->min_io_size = virtio_blk_topo_io_size_min_rdf(&dev->config_space);
    topo->opt_io_size = virtio_blk_topo_io_size_opt_rdf(&dev->config_space);
    topo->num_logic_per_phys = virtio_blk_topo_blocks_logic_per_phys_rdf(&dev
                    ->config_space);

    return 1;
}

/**
 * \brief   returns the blocksize of
 *
 * \param   dev the virtio block device
 * \param   geo memory region to fill the geometry information in
 *              (only valid if VIRTIO_BLK_F_GEOMETRY)
 *
 * \returns true if VIRTIO_BLK_F_GEOMETRY
 *          false otherwise
 */
bool virtio_block_get_geometry(struct virtio_device_blk *dev,
                               struct virtio_block_geometry *geo)
{
    if (!geo) {
        return 0;
    }

    if (!!virtio_device_has_feature(dev->vdev, VIRTIO_BLOCK_F_GEOMETRY)) {
        return 0;
    }

    geo->cylinders = virtio_blk_geometry_cylinders_rdf(&dev->config_space);
    geo->heads = virtio_blk_geometry_heads_rdf(&dev->config_space);
    geo->sectors = virtio_blk_geometry_sectors_rdf(&dev->config_space);

    return 0;
}

/**
 * \brief reads the device configuration and copies it into the local memory
 *
 * \param dev the block device to read the configuration space.
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_block_config_read(struct virtio_device_blk *dev)
{
    if (dev->config_addr == NULL) {
        dev->config_addr = malloc(VIRTIO_BLOCK_CONFIG_SIZE);
        if (dev->config_addr == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }
    }

    return virtio_device_config_read(dev->vdev,
                                     dev->config_addr,
                                     VIRTIO_BLOCK_CONFIG_SIZE);
}

/**
 * \brief   handles the VirtIO block device common initialization.
 *
 * \param   dev     the VirtIO block device
 * \param   setup   the setup information
 *
 * \returns SYS_ERR_OK on success
 */
static errval_t virtio_block_init_common(struct virtio_device_blk *dev,
                                         struct virtio_device_setup *setup)
{
    errval_t err;

    /* read the device configuration */
    err = virtio_block_config_read(dev);
    if (err_is_fail(err)) {
        return err;
    }

    if (setup->vq_num != VIRTIO_BLOCK_NUM_VIRTQUEUES) {
        /*
         * TODO: handle this case.
         */
        assert(setup->vq_num == VIRTIO_BLOCK_NUM_VIRTQUEUES);
    }

    /*
     * allocate the virtqueues
     */
    err = virtio_device_virtqueue_alloc(dev->vdev, setup->vq_setup, setup->vq_num);
    if (err_is_fail(err)) {
        return err;
    }

    dev->vq = virtio_device_get_virtq(dev->vdev, 0);
    assert(dev->vq);

    dev->vdev->state = VIRTIO_DEVICE_S_READY;

    return SYS_ERR_OK;
}


/**
 * \brief   handles the VirtIO block device specific initialization.
 *          with the device registers already mapped
 *
 * \param   dev     the VirtIO block device
 * \param   setup   the setup information
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_block_init_device(struct virtio_device_blk *dev,
                                  struct virtio_device_setup *setup)
{
    errval_t err;

    if (setup->type != VIRTIO_DEVICE_TYPE_BLOCK) {
        VIRTIO_DEBUG_DT("ERROR: Device type was not VIRTIO_DEVICE_TYPE_BLOCK\n");
        return VIRTIO_ERR_DEVICE_TYPE;
    }

    /* initialize the VirtIO device */
    err = virtio_device_open(&dev->vdev, setup);
    if (err_is_fail(err)) {
        return err;
    }

    return virtio_block_init_common(dev, setup);
}

/**
 * \brief   handles the VirtIO block device specific initialization.
 *          the device registers are not mapped yet
 *
 * \param   dev     the VirtIO block device
 * \param   setup   the setup information
 * \param   dev_cap the frame capability backing the device registers
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_block_init_device_with_cap(struct virtio_device_blk *dev,
                                           struct virtio_device_setup *setup,
                                           struct capref dev_cap)
{
    errval_t err;

    if (setup->type != VIRTIO_DEVICE_TYPE_BLOCK) {
        return VIRTIO_ERR_DEVICE_TYPE;
    }

    /* initialize the VirtIO device */
    err = virtio_device_open_with_cap(&dev->vdev, setup, dev_cap);
    if (err_is_fail(err)) {
        return err;
    }

    return virtio_block_init_common(dev, setup);
}
