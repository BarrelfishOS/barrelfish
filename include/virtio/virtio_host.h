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
    errval_t (*close)(void);
    errval_t (*add)(void);
    errval_t (*ext)(void);
    errval_t (*req)(void);
};

#if 0
struct virtio_host_setup
{
    uint8_t        dev_type;        ///< which device we are emulating
    void          *dev_vbase;
    size_t         dev_size;
    struct capref  dev_cap;

    uint64_t       features;

    uint16_t       vq_num;
    uint16_t      *vq_max;    ///<

    enum virtio_host_channel hc_type;
    char                    *hc_iface;
    struct virtio_host_cb   *hc_callback;

    enum virtio_device_backend backend;
};
#endif

struct virtio_host_setup
{
    uint8_t device_type;        ///< which device we are emulating
    uint64_t device_features;   ///< VirtIO device features bits we support

    uint16_t num_queues;        ///< number of queues we have
    uint16_t *queue_num_max;    ///<

    void *dev_reg;
    lpaddr_t dev_size;
    struct capref dev_cap;

    enum virtio_host_channel channel_type;
    enum virtio_device_backend backend;
    char *iface;
    struct virtio_host_cb *callback;
    uint32_t flags;
};


/**
 * \brief initializes the VirtIO device and the host side services
 *
 * \param host  where the host structure pointer will be stored
 * \param setup information needed to configure the host
 */
errval_t virtio_host_init(struct virtio_host **host,
                          struct virtio_host_setup *setup);

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
errval_t virtio_host_poll_device(struct virtio_host *host);

/**
 *
 */
errval_t virtio_host_get_device_cap(struct virtio_host *host,
                                    struct capref *ret_cap);

#endif // VIRTIO_GUEST_H
