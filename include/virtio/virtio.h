/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_H
#define VIRTIO_H

#include <barrelfish/barrelfish.h>

/*
 * 2.1 Device Status Field
 */

/// The device is in the reset state (not discovered by the guest)
#define VIRTIO_DEVICE_STATUS_RESET       0x00

/// Guest OS has found the device and recognized it as a valid virtio device.
#define VIRTIO_DEVICE_STATUS_ACKNOWLEDGE 0x01

/// Guest OS knows how to drive the device i.e. recognized as valid virtio device.
#define VIRTIO_DEVICE_STATUS_DRIVER      0x02

/// Driver is set up and ready to drive the device.
#define VIRTIO_DEVICE_STATUS_DRIVER_OK   0x04

/// Driver has acknowledged all the features it understands
#define VIRTIO_DEVICE_STATUS_FEATURES_OK 0x08

/// Something went wrong in the guest, and it has given up on the device.
#define VIRTIO_DEVICE_STATUS_FAILED      0x80

/*
 * 5.0 Device Types
 * The following device IDs are used to identify different types of virtio
 * devices. Some device IDs are reserved for devices which are not currently
 * defined in this standard.
 */

/// Invalid device identifier
#define VIRTIO_DEVICE_TYPE_INVALID   0x00

/// Device type for network interface cards
#define VIRTIO_DEVICE_TYPE_NET       0x01

/// Device type for block devices
#define VIRTIO_DEVICE_TYPE_BLOCK     0x02

/// Device type for console devices
#define VIRTIO_DEVICE_TYPE_CONSOLE   0x03

/// Device type for entorpy devices
#define VIRTIO_DEVICE_TYPE_ENTORPY   0x04

//#define VIRTIO_DEVICE_TYPE_LEGACY_BALLOON 5

/// Device type for IO memory devices
#define VIRTIO_DEVICE_TYPE_IOMEM     0x06

/// Device type for rpmgs devices
#define VIRTIO_DEVICE_TYPE_RPMSG     0x07

/// Device type for SCSI host devices
#define VIRTIO_DEVICE_TYPE_SCSIHOST  0x08

/// Device type for 9P transport devices
#define VIRTIO_DEVICE_TYPE_9PTRANSP  0x09

/// Device type for MAC 802.11 WLAn devices
#define VIRTIO_DEVICE_TYPE_WLAN      0x0A

/// Device type for RPROC serial devices
#define VIRTIO_DEVICE_TYPE_SERIAL    0x0B

/// Device type for virtio CAIF devices
#define VIRTIO_DEVICE_TYPE_CAIF      0x0C

/// Device type for memory ballooning devices
#define VIRTIO_DEVICE_TYPE_BALLOON   0x0D

/// Device type for GPU devices
#define VIRTIO_DEVICE_TYPE_GPU       0x0E

/// Device type for timer / clock devices
#define VIRTIO_DEVICE_TYPE_TIMER     0x0F


/*
 * Generic Feature Bits
 */
/// Generate interrupt if queue is completely used
#define VIRTIO_F_NOTIFY_ON_EMPTY (1 << 24)

/// guest should never set this feature. This indicates faulty drivers
#define VIRTIO_F_BAD_FEATURE (1 << 30)

/// range of the transport related feature bits
#define VIRTIO_TRANSPORT_F_START    28
#define VIRTIO_TRANSPORT_F_END      32


#endif // VIRTIO_H
