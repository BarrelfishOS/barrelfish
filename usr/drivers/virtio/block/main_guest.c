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
#include <barrelfish/barrelfish.h>

#include <virtio/virtio.h>
#include <virtio/virtio_device.h>
#include <virtio/devices/virtio_block.h>
#include <virtio/virtio_guest.h>

#include <dev/virtio/virtio_blk_dev.h>

#include "device.h"
#include "request.h"
#include "service.h"

struct virtio_device_blk blk_dev;

int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("VirtIO block device driver started.\n");

    err = virtio_guest_init(VIRTIO_GUEST_CHAN_FLOUNDER,
                            VIRTIO_BLOCK_FLOUNDER_IFACE);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize the library\n");
    }

    struct capref dev_frame;
    err = virtio_guest_open_device(VIRTIO_DEVICE_BACKEND_MMIO, &dev_frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not open the device\n");
    }

    struct frame_identity id;
    err = invoke_frame_identify(dev_frame, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "identifying the frame failed\n");
    }

    size_t dev_size = (1UL<<id.bits);

    void *dev_regs;
    err = vspace_map_one_frame_attr(&dev_regs, dev_size, dev_frame, VIRTIO_VREGION_FLAGS_DEVICE, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to map the device frame");
    }

    vblock_device_init(&blk_dev, dev_regs, dev_size);

    debug_printf("VirtIO block device driver terminated.\n");
}
