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

#include "backends/virtio_mmio.h"

/**
 * \brief initializes and allocates a VirtIO device structure for the MMIO backend
 */
errval_t virtio_device_mmio_init(struct virtio_device **dev,
                                 struct virtio_device_init *info)
{
    return SYS_ERR_OK;
}



