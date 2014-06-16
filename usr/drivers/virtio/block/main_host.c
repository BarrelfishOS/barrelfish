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
#include <barrelfish/waitset.h>

#include <virtio/virtio.h>
#include <virtio/virtio_device.h>
#include <virtio/virtio_host.h>
#include <virtio/devices/virtio_block.h>

#include "host.h"
#include "device.h"
#include "request.h"
#include "service.h"

struct virtio_host *host;

static errval_t handle_open(struct capref *ret_frame, void **st)
{
    assert(host);

    virtio_host_get_device_cap(host, ret_frame);

    return SYS_ERR_OK;
}


static struct virtio_host_cb host_cb = {
  .open = handle_open
};

int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("VirtIO block device host started.\n");

    uint16_t queue_num_max = 32;

    struct virtio_host_setup setup = {
        .device_features = 0xFFFFFFFFFFFFFFFF,
        .num_queues = 1,
        .queue_num_max = &queue_num_max,
        .callback = &host_cb,
        .channel_type = VIRTIO_HOST_CHAN_FLOUNDER,
        .backend = VIRTIO_DEVICE_BACKEND_MMIO,
        .iface = VIRTIO_BLOCK_FLOUNDER_IFACE,
        .device_type = VIRTIO_DEVICE_TYPE_BLOCK
    };

    err = virtio_host_init(&host,
                           &setup);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Service initialization failed.\n");
    }

    struct waitset *ws = get_default_waitset();
    while(1) {
        uint8_t idle_count = 0;
        err = virtio_host_poll_device(host);
        if (err_is_fail(err)) {
            if (err_no(err) == VIRTIO_ERR_DEVICE_IDLE) {
                idle_count++;
            } else {
                USER_PANIC_ERR(err, "error while polling device");
            }
        }
        err = event_dispatch_non_block(ws);
        if (err_is_fail(err)) {
            if (err_no(err) == LIB_ERR_NO_EVENT) {
                idle_count++;
            } else {
                USER_PANIC_ERR(err, "error while dispatching events");
            }
        }
        if (idle_count == 2) {
            thread_yield();
        }
    }

    debug_printf("VirtIO block device host terminated.\n");
}

