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
#include <virtio/virtqueue.h>
#include <virtio/virtio_ring.h>
#include <virtio/virtio_device.h>

#include <dev/virtio/virtio_mmio_dev.h>


#include "virtio_device.h"
#include "backends/virtio_mmio.h"
#include "debug.h"

#define REG_WAIT_MAX 0xFFFF


#define REGISTER_WAIT_READY(_reg,_dev)      \
    do {                                    \
        uint32_t wait = REG_WAIT_MAX;       \
        while (!_reg(_dev) && (--wait))     \
            ;                               \
    } while(0);                             \


/**
 * \brief   updates the device status field of the VirtIO device
 *
 * \param dev        device to set the status
 * \param new_status the status to set the device to
 *
 * \returns SYS_ERR_OK on success
 *          VIRTIO_ERR_DEVICE_STATUS if the status change does not make sense
 */
static errval_t device_set_status(struct virtio_device *dev,
                                  uint8_t new_status)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *)dev;

    virtio_mmio_status_t status = virtio_mmio_status_default;

    switch(new_status) {
        case VIRTIO_DEVICE_STATUS_RESET:
            virtio_mmio_reset_wr(&mmio_dev->regs, virtio_mmio_device_reset);
            return SYS_ERR_OK;
            break;
        case VIRTIO_DEVICE_STATUS_FAILED:
            status = virtio_mmio_status_failed_insert(status, 0x1);
            break;
        case VIRTIO_DEVICE_STATUS_ACKNOWLEDGE :
            status = virtio_mmio_status_rd(&mmio_dev->regs);
            if (status != 0x0) {
                return VIRTIO_ERR_DEVICE_STATUS;
            }
            status = virtio_mmio_status_acknowledge_insert(status, 0x1);
            break;
        case VIRTIO_DEVICE_STATUS_DRIVER :
            status = virtio_mmio_status_rd(&mmio_dev->regs);
            if (!virtio_mmio_status_acknowledge_extract(status)) {
                return VIRTIO_ERR_DEVICE_STATUS;
            }
            status = virtio_mmio_status_driver_insert(status, 0x1);
            break;

        case VIRTIO_DEVICE_STATUS_FEATURES_OK :
            status = virtio_mmio_status_rd(&mmio_dev->regs);
            if (!virtio_mmio_status_driver_extract(status)) {
                return VIRTIO_ERR_DEVICE_STATUS;
            }
            status = virtio_mmio_status_features_ok_insert(status, 0x1);
            break;

        case VIRTIO_DEVICE_STATUS_DRIVER_OK :
            status = virtio_mmio_status_rd(&mmio_dev->regs);
            if (!virtio_mmio_status_features_ok_extract(status)) {
                return VIRTIO_ERR_DEVICE_STATUS;
            }
            status = virtio_mmio_status_driver_ok_insert(status, 0x1);
            break;
    }

    virtio_mmio_status_wr(&mmio_dev->regs, status);

    return SYS_ERR_OK;
}

/**
 * \brief resets the VirtIO deivces
 *
 * \param dev the device to reset
 *
 * \returns SYS_ERR_OK on success
 */
static errval_t device_reset(struct virtio_device *dev)
{
    /*
     * TODO: is there some clean up needed ?
     */
    return device_set_status(dev, VIRTIO_DEVICE_STATUS_RESET);
}


static errval_t device_get_device_features(struct virtio_device *dev,
                                           uint64_t *ret_features)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *)dev;

    uint64_t features = 0;

    virtio_mmio_dev_features_sel_wr(&mmio_dev->regs, 0x1);

    uint32_t wait = REG_WAIT_MAX;
    while (!virtio_mmio_dev_features_sel_ready_rdf(&mmio_dev->regs) && (--wait))
        ;

    features = virtio_mmio_dev_features_features_rdf(&mmio_dev->regs);

    features <<= 32;

    virtio_mmio_dev_features_sel_wr(&mmio_dev->regs, 0x0);

    wait = REG_WAIT_MAX;
    while (!virtio_mmio_dev_features_sel_ready_rdf(&mmio_dev->regs) && (--wait))
            ;

    /*
     * TODO: when do we know when the new values in the features
     *       are ready??
     */
    features |= virtio_mmio_dev_features_features_rdf(&mmio_dev->regs);

    return SYS_ERR_OK;
}

static errval_t device_set_driver_features(struct virtio_device *dev,
                                          uint64_t features)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *)dev;

    virtio_mmio_driv_features_sel_wr(&mmio_dev->regs, 0x0);

    uint32_t wait = REG_WAIT_MAX;
    while (!virtio_mmio_driv_features_sel_ready_rdf(&mmio_dev->regs) && (--wait))
         ;

    virtio_mmio_driv_features_wr(&mmio_dev->regs, (uint32_t)(features));

    wait = REG_WAIT_MAX;
    while (!virtio_mmio_driv_features_sel_ack_rdf(&mmio_dev->regs) && (--wait))
        ;

    virtio_mmio_driv_features_sel_wr(&mmio_dev->regs, 0x1);
    wait = REG_WAIT_MAX;
    while (!virtio_mmio_driv_features_sel_ready_rdf(&mmio_dev->regs) && (--wait))
        ;

    virtio_mmio_driv_features_wr(&mmio_dev->regs, (uint32_t)(features >> 32));

    wait = REG_WAIT_MAX;
    while (!virtio_mmio_driv_features_sel_ack_rdf(&mmio_dev->regs) && (--wait))
            ;

    return SYS_ERR_OK;
}

static errval_t device_set_virtq(struct virtio_device *dev,
                                 struct virtqueue *vq)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *)dev;

    uint16_t queue_index = virtio_virtqueue_get_queue_index(vq);

    /* todo: wait till the previous request has been seen... */
    uint32_t wait = REG_WAIT_MAX;
    while (!virtio_mmio_queue_ready_ack_rdf(&mmio_dev->regs) && (--wait))
            ;

    virtio_mmio_queue_sel_wr(&mmio_dev->regs, queue_index);
    wait = REG_WAIT_MAX;
    while (!virtio_mmio_queue_sel_ready_rdf(&mmio_dev->regs) && (--wait))
        ;


    /* TODO> wait till queue has been selected */

    if (virtio_mmio_queue_ready_ready_rdf(&mmio_dev->regs)) {
        /* queue has already been activated... */
        return VIRTIO_ERR_QUEUE_ACTIVE;
    }

    if (virtio_mmio_queue_max_size_rdf(&mmio_dev->regs) == 0x0) {
        /* the queue is not implemented */
        return VIRTIO_ERR_QUEUE_INVALID;
    }

    uint16_t size = virtio_virtqueue_get_num_desc(vq);
    virtio_mmio_queue_num_size_wrf(&mmio_dev->regs, size);

    lpaddr_t paddr = virtio_virtqueue_get_vring_paddr(vq);
    lpaddr_t align = virtio_virtqueue_get_vring_align(vq);


    virtio_mmio_queue_desc_hi_addr_wrf(&mmio_dev->regs, (uint32_t)(paddr >> 32));
    virtio_mmio_queue_desc_lo_addr_wrf(&mmio_dev->regs, (uint32_t)(paddr));

    paddr += size * sizeof(struct vring_desc);

    virtio_mmio_queue_used_hi_addr_wrf(&mmio_dev->regs, (uint32_t)(paddr >> 32));
    virtio_mmio_queue_used_lo_addr_wrf(&mmio_dev->regs, (uint32_t)(paddr));

    paddr += sizeof(uint16_t) * (2 + size + 1);
    paddr = (paddr + align - 1) & ~(align - 1);

    virtio_mmio_queue_avail_hi_addr_wrf(&mmio_dev->regs, (uint32_t)(paddr >> 32));
    virtio_mmio_queue_avail_lo_addr_wrf(&mmio_dev->regs, (uint32_t)(paddr));

    /* signal the host that the queue is ready */
    virtio_mmio_queue_ready_ready_wrf(&mmio_dev->regs, 0x1);

    return SYS_ERR_OK;
}

static errval_t device_get_queue_num_max(struct virtio_device *dev,
                                         uint16_t queue_index,
                                         uint16_t *num_max)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *)dev;

    virtio_mmio_queue_sel_selector_wrf(&mmio_dev->regs, queue_index);

    /* TODO: wait till data ready */

    uint16_t qmax = (uint16_t)virtio_mmio_queue_max_size_rdf(&mmio_dev->regs);

    if (num_max) {
        *num_max = qmax;
    }

    return SYS_ERR_OK;
}

static errval_t device_negotiate_features(struct virtio_device *dev,
                                          uint64_t driver_features)
{
    uint64_t device_features = 0x0;
    device_get_device_features(dev, &device_features);

    driver_features &= device_features;

    device_set_driver_features(dev, driver_features);

    dev->features = driver_features;

    return SYS_ERR_OK;
}

struct virtio_device_fn virtio_mmio_fn = {
    .reset = device_reset,
    .set_status = device_set_status,
    .negotiate_features = device_negotiate_features,
    .set_virtq = device_set_virtq,
    .get_queue_num_max = device_get_queue_num_max
};


/**
 * \brief initializes and allocates a VirtIO device structure for the MMIO backend
 */
errval_t virtio_device_mmio_init(struct virtio_device **dev,
                                 struct virtio_device_setup *info)
{
    struct virtio_device_mmio *mmio_dev;
    errval_t err;
    mmio_dev = malloc(sizeof(*mmio_dev));
    if (mmio_dev == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    virtio_mmio_initialize(&mmio_dev->regs, (mackerel_addr_t) (info->dev_reg));


    /**
     * 4.2.3.1.1 Driver Requirements: Device Initialization
     * The driver MUST start the device initialization by reading and checking
     * values from MagicValue and Version.
     * If both values are valid, it MUST read DeviceID and if its value is zero
     * (0x0) MUST abort initialization and MUST NOT access any other register.
     */
    if (virtio_mmio_magic_value_rd(&mmio_dev->regs) != virtio_mmio_magic_value) {
        VIRTIO_DEBUG_DEV("Virtio Magic Value is invalid\n");
        return VIRTIO_ERR_NOT_VIRTIO_DEVICE;
    }

    err = SYS_ERR_OK;
    switch (virtio_mmio_version_rd(&mmio_dev->regs)) {
        case virtio_mmio_version_legacy:
            VIRTIO_DEBUG_DEV("Handling of legacy devices not supported.\n");
            err = VIRTIO_ERR_VERSION_MISMATCH;
            break;
        case virtio_mmio_version_virtio10:
            err = SYS_ERR_OK;
            break;
        default:
            VIRTIO_DEBUG_DEV("Virtio version is invalid.\n");
            err = VIRTIO_ERR_NOT_VIRTIO_DEVICE;
            break;
    }
    if (err_is_fail(err)) {
        return err;
    }

    virtio_mmio_deviceid_t devid = virtio_mmio_deviceid_rd(&mmio_dev->regs);
    if (devid == 0) {
        VIRTIO_DEBUG_DEV("Virtio device ID invalid.\n");
        return VIRTIO_ERR_NOT_VIRTIO_DEVICE;
    }

    mmio_dev->dev.devid = devid;
    mmio_dev->dev.backend = VIRTIO_DEVICE_BACKEND_MMIO;
    mmio_dev->dev.f = &virtio_mmio_fn;
    mmio_dev->dev.type = virtio_mmio_deviceid_id_rdf(&mmio_dev->regs);

    return SYS_ERR_OK;
}
