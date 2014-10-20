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
#include <virtio/virtqueue.h>
#include <virtio/virtqueue_host.h>
#include <virtio/virtio_device.h>
#include <virtio/virtio_host.h>
#include <virtio/devices/virtio_block.h>

#include "host.h"
#include "device.h"
#include "request.h"
#include "service.h"

struct virtio_device *host;

static errval_t handle_open(struct virtio_device *vdev, uint8_t backend, struct capref *ret_frame)
{
    assert(host);

    virtio_host_get_device_cap(vdev, ret_frame);

    assert(!capref_is_null(*ret_frame));

    return SYS_ERR_OK;
}

static errval_t handle_add(struct virtio_device *vdev, struct capref ring,
                    uint16_t ndesc, uint8_t buf_bits,uint16_t vq_id)
{
    debug_printf("handle_add\n");
    return SYS_ERR_OK;
}


static struct virtio_host_cb host_cb = {
  .open = handle_open,
  .add = handle_add
};

static void virtq_do_work(struct virtqueue_host *vqh,
                   void *arg,
                   struct virtio_host_buf *buf,
                   uint16_t idx)
{
    debug_printf("virtq_do_work");



    virtio_vq_host_desc_enqueue(vqh, buf, idx);
}

int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("VirtIO block device host started.\n");

    struct virtqueue_setup vq_setup =  {
            .name = "Request Virtqueue",
            .vring_ndesc = 16,
            .max_indirect =0,
            .worker_arg = NULL,
            .worker_fn = virtq_do_work
        };

    struct virtio_device_setup setup = {
        .features = 0xFFFFFFFFFFFFFFFF,
        .vq_num = 1,
        .vq_setup = &vq_setup,
        .hc_cb = &host_cb,
        .hc_type = VIRTIO_HOST_CHAN_FLOUNDER,
        .backend = {
            .type = VIRTIO_DEVICE_BACKEND_MMIO,
            .args = {}
        },
        .hc_iface = VIRTIO_BLOCK_FLOUNDER_IFACE,
        .dev_type = VIRTIO_DEVICE_TYPE_BLOCK
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

