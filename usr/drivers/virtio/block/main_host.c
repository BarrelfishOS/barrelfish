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
#include <virtio/virtio_host.h>
#include <virtio/devices/virtio_block.h>

#include "host.h"
#include "device.h"
#include "request.h"
#include "service.h"

struct virtio_host *host;

static errval_t handle_open(struct capref *ret_frame, void **st)
{
    return SYS_ERR_OK;
}


static struct virtio_host_cb host_cb = {
  .open = handle_open
};

int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("VirtIO block device host started.\n");

    struct virtio_host_setup setup = {
        .callback = &host_cb,
        .channel_type = VIRTIO_HOST_CHAN_FLOUNDER,
        .iface = VIRTIO_BLOCK_FLOUNDER_IFACE
    };

    err = virtio_host_init(&host,
                           &setup);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Service initialization failed.\n");
    }


    messages_handler_loop();

    debug_printf("VirtIO block device host terminated.\n");
}

