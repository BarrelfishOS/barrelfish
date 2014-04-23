/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_VIRTIO_MMIO_H
#define VIRTIO_VIRTIO_MMIO_H

#include <barrelfish/barrelfish.h>
#include <virtio/virtio.h>

/**
 * \brief initializes a new virtio device structure
 */
errval_t virtio_mmio_device_init(struct virtio_device **vdev,
                                 void *device_config,
                                 struct capref mmio);

/**
 * \brief de-initialization of the device structure and freeing of resources
 */
errval_t virtio_mmio_device_free(struct virtio_device **vdev);

/**
 * \brief initializes a new virtio driver structure
 */
errval_t virtio_mmio_driver_init(struct virtio_driver **vdrv,
                                 struct capref mmio);

/**
 * \brief de-initialization of the driver structure and freeing of resources
 */
errval_t virtio_mmio_driver_free(struct virtio_driver **vdrv);

#endif // VIRTIO_VIRTIO_MMIO_H
