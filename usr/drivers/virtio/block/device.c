/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <virtio/virtio.h>
#include <virtio/virtqueue.h>
#include <virtio/virtio_device.h>
#include <virtio/devices/virtio_block.h>

#include "device.h"
#include "request.h"
#include "debug.h"


/**
 * \brief handles the VirtIO config change interrupt
 *
 * \param pointer to the virtio device
 *
 * \returns SYS_ERR_OK on success
 */
void vblock_device_config_changed_intr(struct virtio_device *vdev)
{
    struct virtio_device_blk *bdev;

    bdev = virtio_device_get_struct(vdev);
    if (bdev == NULL) {
        return;
    }

    virtio_block_config_read(bdev);
}

/**
 * \brief handles the VirtIO interrupts
 *
 * \param arg the argument passed to the virtqueue when allocating it
 */
void vblock_device_virtqueue_intr(void *arg)
{
    errval_t err;

    struct vblock_device *dev = arg;
    struct virtqueue *vq = dev->blk.vq;

    bool again = true;

    while(again) {
        err = vblock_request_finish_completed(dev);
        if (err_is_fail(err)) {
            /*todo error recovery */
        }

        again = !virtio_virtqueue_intr_enable(vq);
        if (again) {
            virtio_virtqueue_intr_disable(vq);
        }
    }
}



errval_t vblock_device_init(struct virtio_device_blk *blk,
                            void *dev_regs,
                            size_t reg_size)
{
    errval_t err;

    VIRTIO_DEBUG_DEV("Initializing vblock device [%016lx, %lx]\n",
                     (uintptr_t)dev_regs, (uint64_t)reg_size);

    struct virtqueue_setup vq_setup =  {
        .name = "Request Virtqueue",
        .vring_ndesc = 0,
        .vring_align = 0,
        .intr_handler = 0,
        .intr_arg = 0,
        .max_indirect =0
    };

    struct virtio_device_setup setup = {
        .name = "VirtIO Block Device",
        .dev_reg = dev_regs,
        .dev_reg_size = reg_size,
        .driver_features = VBLOCK_DRIVER_FEATURES,
        .backend = VBLOCK_DRIVER_BACKEND,
        .type = VIRTIO_DEVICE_TYPE_BLOCK,
        .vq_setup = &vq_setup,
        .vq_num = 1
    };

    err = virtio_block_init_device(blk, &setup);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}
