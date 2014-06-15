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

#include <virtio/virtio_device.h>

#include <dev/virtio/virtio_mmio_dev.h>

#define VIRTIO_MMIO_DEVICE_SIZE 0x100

struct virtio_device_mmio
{
    struct virtio_device dev;
    virtio_mmio_t regs;
};

struct virtio_host_mmio
{
    struct virtio_host host;
    virtio_mmio_t regs;
};


/**
 * \brief initializes and allocates a VirtIO device structure for the MMIO backend
 *
 * \param dev   returns a pointer to the newly allocated device structure
 * \param info  initialization parameters
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_device_mmio_init(struct virtio_device **dev,
                                 struct virtio_device_setup *info);


/**
 * \brief initializes a VirtIO device on the host side using the MMIO transpot
 *
 * \param dev   returns a pointer to the newly allocated device structure
 * \param info  initialization parameters
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_device_mmio_init_host(struct virtio_host **host,
                                      struct virtio_host_setup *setup);



#endif // VIRTIO_VIRTIO_MMIO_H
