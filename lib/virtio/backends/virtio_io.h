/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_VIRTIO_PCI_H
#define VIRTIO_VIRTIO_PCI_H

#include <barrelfish/barrelfish.h>



#define VIRTIO_PCI_VRING_ALIGNMENT 4096

errval_t virtio_device_pci_alloc(struct virtio_device **dev);

errval_t virtio_device_pci_free(struct virtio_device **dev);


#endif // VIRTIO_VIRTIO_PCI_H
