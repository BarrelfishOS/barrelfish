/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_DEVICE_H
#define VIRTIO_DEVICE_H

#include <virtio/virtio_device.h>
#include <virtio/virtio_host.h>

// forward declaration
struct virtqueue;

struct virtio_host
{
    uint8_t device_type;
    struct capref dev_frame;
    lpaddr_t dev_size;
    void *device_base;
    enum virtio_device_backend backend;
    struct virtio_host_cb *callback;
    errval_t (*poll)(struct virtio_host *);
};

/**
 * represents a virtio device, this data structure is only valid with the
 * backend specific extentions and should not be allocated directly
 */
struct virtio_device
{
    char name[VIRTIO_DEVICE_NAME_MAX];
    uint32_t devid;

    uint8_t type;
    void *device_type;

    uint8_t status_flags;
    enum virtio_device_status state;

    uint64_t features;
    enum virtio_device_backend backend;
    struct virtio_device_fn *f;

    uint16_t vq_num;
    struct virtqueue **vq;
};

/**
 * contains function pointers to backend specific functions
 */
struct virtio_device_fn
{
    errval_t (*reset)(struct virtio_device *dev);
    errval_t (*set_status)(struct virtio_device *dev, uint8_t status);
    errval_t (*negotiate_features)(struct virtio_device *dev, uint64_t driv_features);
    errval_t (*setup)(struct virtio_device *dev);
    errval_t (*get_queue_num_max)(struct virtio_device *dev, uint16_t queue_index, uint16_t *num_max);
    errval_t (*set_virtq)(struct virtio_device *dev, struct virtqueue *vq);
};






#endif // VIRTIO_VIRTIO_DEVICE_H
