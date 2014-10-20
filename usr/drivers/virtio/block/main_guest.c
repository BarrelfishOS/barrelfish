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

struct vblock_device blk_dev;

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

    uint32_t count = 0;
    while(count < 32) {
        debug_printf("\n\n------------------------------------Round: %u\n", count++);
        struct vblock_req *req = vblock_request_alloc(&blk_dev);
        if (!req) {
            USER_PANIC("no request available\n");
            break;
        }

        struct virtio_buffer *buf = virtio_buffer_alloc(blk_dev.alloc);
        if (!buf) {
            vblock_request_free(&blk_dev, req);
            USER_PANIC("no buffer available\n");
            break;
        }

        snprintf(buf->buf, buf->length, "Hello world!!\n");

        err = virtio_blist_append(&req->bl, buf);
        assert(err_is_ok(err));

        assert(req->bl.length == 1);

        err = vblock_request_exec(&blk_dev, req);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "execute the request");
        }

        struct virtio_buffer *ret_buf = virtio_blist_head(&req->bl);
        assert(req->bl.length == 0);

        assert(ret_buf == buf);

        debug_printf("data=[%s]", (char*)ret_buf->buf);


        virtio_buffer_free(buf);

        vblock_request_free(&blk_dev, req);
    }

    debug_printf("VirtIO block device driver terminated.\n");
}
