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

/*
 * 4.1.2 PCI Device Discovery
 *
 * Any PCI device with Vendor ID 0x1AF4, and Device ID 0x1000 through 0x103F
 * inclusive is a virtio device. The Subsystem Device ID indicates which virtio
 * device is supported by the device.
 */

#define VIRTIO_PCI_VENDOR_ID 0x1AF4
#define VIRTIO_PCI_DEVICE_ID 0x1000
#define VIRTIO_PCI_DEVICE_ID2 0x103F



#endif // VIRTIO_VIRTIO_PCI_H
