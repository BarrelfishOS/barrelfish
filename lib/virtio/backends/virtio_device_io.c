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

#ifdef __VIRTIO_HOST__
#include <virtio/virtio_host.h>
#endif

#include "backends/virtio_pci.h"


errval_t virtio_device_pci_alloc(struct virtio_device **dev)
{
    return SYS_ERR_OK;
}

errval_t virtio_device_pci_free(struct virtio_device **dev)
{
    return SYS_ERR_OK;
}



