/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_GUEST_H
#define VIRTIO_GUEST_H

struct virtqueue;

/**
 * represents the channel backend to be used for this VirtIO guest library
 */
enum virtio_guest_channel
{
    VIRTIO_GUEST_CHAN_INVALID,  ///< the channel type is invalid
    VIRTIO_GUEST_CHAN_FLOUNDER, ///< the flounder RPC backend should be used
    VIRTIO_GUEST_CHAN_XEON_PHI, ///< the xeon phi messaging backend should be used
};

/**
 * holds the function pointers to the respective channel backends
 */
struct virtio_guest_chan_fn
{
    errval_t (*open)(uint8_t backend, struct capref *ret_frame);
    errval_t (*close)(void);
    errval_t (*add)(struct virtqueue *vq);
    errval_t (*ext)(uint16_t vq_id, struct capref vbuf);
    errval_t (*req)(uint64_t size, struct capref *cap);
};

extern struct virtio_guest_chan_fn *vguest_chan_fn;

/**
 *
 */
errval_t virtio_guest_init(enum virtio_guest_channel backend,
                           char *iface);

/**
 *
 */
static inline errval_t virtio_guest_open_device(uint8_t backend,
                                                struct capref *ret_frame)
{
    return vguest_chan_fn->open(backend, ret_frame);
}

/**
 *
 */
static inline errval_t virtio_guest_close_device(void)
{
    return vguest_chan_fn->close();
}

/**
 *
 */
static inline errval_t virtio_guest_add_virtq(struct virtqueue *vq)
{
    return vguest_chan_fn->add(vq);
}

/**
 *
 */
static inline errval_t virtio_guest_extend_vring(uint16_t vq_id,
                                                 struct capref vbuf)
{
    return vguest_chan_fn->ext(vq_id, vbuf);
}

/**
 *
 */
static inline errval_t virtio_guest_request_mem(uint64_t size,
                                                struct capref *cap)
{
    return vguest_chan_fn->req(size, cap);
}

#endif // VIRTIO_GUEST_H
