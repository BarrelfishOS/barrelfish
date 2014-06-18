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

// forward declaration
#ifdef __VIRTIO_HOST__
struct virtqueue_host;
struct virtio_host_cb;
#else
struct virtqueue;
#endif

/**
 * represents a virtio device, this data structure is only valid with the
 * backend specific extentions and should not be allocated directly
 */
struct virtio_device
{
    uint8_t  dev_type;        ///< VirtIO device type
    uint8_t  dev_status;      ///< VirtIO device status flags
    char     dev_name[VIRTIO_DEVICE_NAME_MAX];
    void    *dev_t_st;        ///< pointer to device type specific state
    struct capref dev_cap;
    uint64_t features;        ///< negotiated VirtIO features

    uint64_t device_features; ///< supported features by the device
    uint64_t driver_features; ///< supported features by the driver

    enum virtio_state state;
    enum virtio_backend backend;
    struct virtio_device_fn *f;

    virtio_device_setup_t setup_fn;
    void                 *setup_arg;

    config_intr_handler_t config_intr_fn;

    uint16_t vq_num;
#ifdef __VIRTIO_HOST__
    char hc_iface[VIRTIO_DEVICE_HC_IFACE_MAX];
    struct virtqueue_host **vqh;
    struct virtio_host_cb *cb_h;
#else
    struct virtqueue **vq;
#endif
};

/**
 * contains function pointers to backend specific functions
 */
struct virtio_device_fn
{
    errval_t (*reset)(struct virtio_device *dev);
    errval_t (*set_status)(struct virtio_device *dev, uint8_t status);
    errval_t (*get_status)(struct virtio_device *dev, uint32_t *status);
    errval_t (*negotiate_features)(struct virtio_device *dev, uint64_t driv_features);
    errval_t (*get_queue_num_max)(struct virtio_device *dev, uint16_t queue_index, uint16_t *num_max);
    errval_t (*set_virtq)(struct virtio_device *dev, struct virtqueue *vq);
    errval_t (*get_config)(struct virtio_device *vdev, void *buf,size_t len);
    errval_t (*set_config)(struct virtio_device *dev,void *config,size_t offset, size_t length);
    errval_t (*notify)(struct virtio_device *dev, uint16_t vq_id);
#ifdef __VIRTIO_HOST__
    errval_t (*poll)(struct virtio_device *vdev);
#endif
};


#endif // VIRTIO_VIRTIO_DEVICE_H
