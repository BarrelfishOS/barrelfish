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


struct virtqueue_host;
struct virtio_device;
struct virtio_device_setup;
struct virtqueue_setup;

struct virtio_host_cb
{
    errval_t (*open)(struct virtio_device *vdev, uint8_t backend, struct capref *ret_frame);
    errval_t (*close)(void);
    errval_t (*add)(struct virtio_device *vdev, struct capref ring,
                    uint16_t ndesc, uint8_t has_buffers, uint16_t vq_id);
    errval_t (*ext)(struct virtio_device *vdev);
    errval_t (*req)(struct virtio_device *vdev, struct capref *cap);
    errval_t (*notify)(struct virtio_device *vq, uint16_t index);
};

/**
 * stores the information about the buffer received from the guest
 */
struct virtio_host_buf
{
    lvaddr_t vaddr;
    size_t   size;
    uint16_t flags;
    struct virtio_host_buf *next;

};

/// workhandler for the virtqueue (host side)
typedef void (*virtq_work_handler_t)(struct virtqueue_host *,
                                     void *,
                                     struct virtio_host_buf *,
                                     uint16_t idx);



/**
 * \brief initializes the VirtIO device and the host side services
 *
 * \param host  where the host structure pointer will be stored
 * \param setup information needed to configure the host
 */
errval_t virtio_host_init(struct virtio_device **host,
                          struct virtio_device_setup *setup);

/**
 * \brief polls the device to see if the driver has written something to the
 *        registers
 *
 * \param host the virtio host device
 *
 * \returns SYS_ERR_OK if there was an event on the device
 *          VIRTIO_ERR_DEVICE_IDLE if there was nothing new
 *          VIRTIO_ERR_* on failure
 */
errval_t virtio_host_poll_device(struct virtio_device *host);

/**
 *
 */
errval_t virtio_host_get_device_cap(struct virtio_device *host,
                                    struct capref *ret_cap);


lpaddr_t virtio_host_translate_host_addr(lpaddr_t host_phys);

lpaddr_t virtio_host_translate_guest_addr(lpaddr_t guest_phys);

#endif // VIRTIO_GUEST_H
