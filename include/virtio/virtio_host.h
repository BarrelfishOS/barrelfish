/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_HOST_H
#define VIRTIO_HOST_H

struct virtio_host;

/**
 * represents the channel backend to be used for this VirtIO guest library
 */
enum virtio_host_channel
{
    VIRTIO_HOST_CHAN_INVALID,  ///< the channel type is invalid
    VIRTIO_HOST_CHAN_FLOUNDER, ///< the flounder RPC backend should be used
    VIRTIO_HOST_CHAN_XEON_PHI, ///< the xeon phi messaging backend should be used
};


struct virtio_host_cb
{
    errval_t (*open)(struct capref *ret_frame, void **st);
};

struct virtio_host_setup
{
    uint8_t device_type;
    void *dev_reg;
    lpaddr_t dev_size;
    struct capref dev_cap;
    enum virtio_host_channel channel_type;
    enum virtio_device_backend backend;
    char *iface;
    struct virtio_host_cb *callback;
};


/**
 *
 */
errval_t virtio_host_init(struct virtio_host **host,
                          struct virtio_host_setup *setup);


errval_t virtio_host_poll_device(struct virtio_host *host);

errval_t virtio_host_get_device_cap(struct virtio_host *host,
                                    struct capref *ret_cap);

#endif // VIRTIO_GUEST_H
