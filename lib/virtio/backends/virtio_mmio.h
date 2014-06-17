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
    virtio_mmio_t regs;      ///< mackerel device registers
    void *dev_base;          ///< mapped virtual base address of this device
    size_t dev_size;         ///< size of the mapped device region
#ifdef __VIRTIO_HOST__
    struct mmio_dev_regs {
        uint8_t status;
        uint8_t dev_feature_sel : 4;
        uint8_t driv_feature_sel : 4;
        uint32_t driv_features[2];
        uint16_t queue_sel;
    }dev_reg;
#endif
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
errval_t virtio_device_mmio_init_host(struct virtio_device **dev,
                                      struct virtio_device_setup *setup);

#endif // VIRTIO_VIRTIO_MMIO_H
