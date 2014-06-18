/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>

#include <virtio/virtio.h>
#include <virtio/virtqueue.h>
#include <virtio/virtio_ring.h>
#include <virtio/virtio_device.h>

#ifdef __VIRTIO_HOST__
#include <virtio/virtio_host.h>
#endif

#include <dev/virtio/virtio_mmio_dev.h>

#include "device.h"
#include "backends/virtio_mmio.h"
#include "debug.h"

#define REG_WAIT_MAX 0xFFFF
#define REG_WAIT_USE_YIELD 1

#define REGISTER_SEND_READY(_reg,_dev)      \
    do {                                    \
        _reg(_dev->regs, 0x1);              \
    } while(0);

#if REG_WAIT_USE_YIELD
#define REGISTER_WAIT_READY(_reg,_dev)      \
    do {                                    \
        /* uint32_t wait = REG_WAIT_MAX;   */    \
        while (!_reg(_dev->regs)) {     \
            thread_yield();                 \
        }    \
    } while(0);
#else
#define REGISTER_WAIT_READY(_reg,_dev)      \
    do {                                    \
        uint32_t wait = REG_WAIT_MAX;       \
        while (!_reg(_dev->regs) && (--wait))     \
            ;                               \
    } while(0);

#endif
/**
 * \brief queries the current status flags of the VirtIO device
 *
 * \param dev        VirtIO device
 * \param ret_status pointer to memory to store the return value
 *
 * \returns SYS_ERR_OK on success
 */
static errval_t device_get_status(struct virtio_device *dev,
                                  uint32_t *ret_status)
{
    virtio_mmio_status_t status;

    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *) dev;

    status = virtio_mmio_status_rd(&mmio_dev->regs);

    if (ret_status) {
        *ret_status = status;
    }

    VIRTIO_DEBUG_DEV("getting mmio device status: %x\n", status);

    return SYS_ERR_OK;
}

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
    VIRTIO_DEBUG_DEV("setting mmio device status: %u\n", new_status);

    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *) dev;

    virtio_mmio_status_t status = virtio_mmio_status_default;

    switch (new_status) {
        case VIRTIO_DEVICE_STATUS_RESET:
            virtio_mmio_reset_wr(&mmio_dev->regs, virtio_mmio_device_reset);
            return SYS_ERR_OK;
            break;
        case VIRTIO_DEVICE_STATUS_FAILED:
            status = virtio_mmio_status_failed_insert(status, 0x1);
            break;
        case VIRTIO_DEVICE_STATUS_ACKNOWLEDGE:
            status = virtio_mmio_status_rd(&mmio_dev->regs);
            if (status != 0x0) {
                return VIRTIO_ERR_DEVICE_STATUS;
            }
            status = virtio_mmio_status_acknowledge_insert(status, 0x1);
            break;
        case VIRTIO_DEVICE_STATUS_DRIVER:
            status = virtio_mmio_status_rd(&mmio_dev->regs);
            if (!virtio_mmio_status_acknowledge_extract(status)) {
                return VIRTIO_ERR_DEVICE_STATUS;
            }
            status = virtio_mmio_status_driver_insert(status, 0x1);
            break;

        case VIRTIO_DEVICE_STATUS_FEATURES_OK:
            status = virtio_mmio_status_rd(&mmio_dev->regs);
            if (!virtio_mmio_status_driver_extract(status)) {
                return VIRTIO_ERR_DEVICE_STATUS;
            }
            status = virtio_mmio_status_features_ok_insert(status, 0x1);
            break;

        case VIRTIO_DEVICE_STATUS_DRIVER_OK:
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

#ifndef __VIRTIO_HOST__

/**
 * \brief resets the VirtIO deivces
 *
 * \param dev the device to reset
 *
 * \returns SYS_ERR_OK on success
 */
static errval_t device_reset(struct virtio_device *dev)
{
    VIRTIO_DEBUG_DEV("resetting mmio device: %s\n", dev->dev_name);
    /*
     * TODO: is there some clean up needed ?
     */
    return device_set_status(dev, VIRTIO_DEVICE_STATUS_RESET);
}

/**
 *
 */
static errval_t device_get_device_features(struct virtio_device *dev,
                                           uint64_t *ret_features)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *) dev;

    uint8_t selector = 0x0;
    uint64_t features, tmp = 0;

    if (virtio_mmio_dev_features_sel_selector_rdf(&mmio_dev->regs)) {
        features = virtio_mmio_dev_features_features_rdf(&mmio_dev->regs);
        features <<= 32;
    } else {
        features = virtio_mmio_dev_features_features_rdf(&mmio_dev->regs);
        selector = 0x1;
    }

    virtio_mmio_dev_features_sel_selector_wrf(&mmio_dev->regs, selector);

    REGISTER_WAIT_READY(virtio_mmio_dev_features_sel_ready_rdf, &mmio_dev);

    tmp = virtio_mmio_dev_features_features_rdf(&mmio_dev->regs);
    if (selector) {
        features |= (tmp << 32);
    } else {
        features |= tmp;
    }

    virtio_mmio_dev_features_sel_ready_wrf(&mmio_dev->regs, 0);

    return SYS_ERR_OK;
}



static errval_t device_set_driver_features(struct virtio_device *dev,
                                           uint64_t features)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *) dev;

    debug_printf("device_set_driver_features\n");

    uint8_t selector = 0x0;

    uint32_t reg_val;

    if (virtio_mmio_driv_features_sel_selector_rdf(&mmio_dev->regs)) {
        reg_val = (uint32_t) (features >> 32);
    } else {
        reg_val = (uint32_t) (features);
        selector = 0x1;
    }

    virtio_mmio_driv_features_features_wrf(&mmio_dev->regs, reg_val);

    REGISTER_SEND_READY(virtio_mmio_driv_features_sel_ready_wrf, &mmio_dev);
    REGISTER_WAIT_READY(!virtio_mmio_driv_features_sel_ready_rdf, &mmio_dev);

    virtio_mmio_driv_features_sel_wr(&mmio_dev->regs, selector);

    if (selector) {
        reg_val = (uint32_t) (features >> 32);
    } else {
        reg_val = (uint32_t) (features);
    }

    virtio_mmio_driv_features_features_wrf(&mmio_dev->regs, reg_val);

    REGISTER_SEND_READY(virtio_mmio_driv_features_sel_ready_wrf, &mmio_dev);
    REGISTER_WAIT_READY(!virtio_mmio_driv_features_sel_ready_rdf, &mmio_dev);

    // clear the ready bit in the end
    virtio_mmio_driv_features_sel_ready_wrf(&mmio_dev->regs, 0x0);

    return SYS_ERR_OK;
}


static errval_t device_set_virtq(struct virtio_device *dev,
                                 struct virtqueue *vq)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *) dev;

    uint16_t queue_index = virtio_virtqueue_get_queue_index(vq);

    VIRTIO_DEBUG_TL("setting virtqueue [VQ(%u) @ %s]\n", queue_index, dev->dev_name);

    /* we nee to change to the correct queue */
    if (virtio_mmio_queue_sel_selector_rdf(&mmio_dev->regs) != queue_index) {
        virtio_mmio_queue_sel_selector_wrf(&mmio_dev->regs, queue_index);
        REGISTER_WAIT_READY(virtio_mmio_queue_sel_ready_rdf, &mmio_dev);
    }

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

    virtio_mmio_queue_desc_hi_addr_wrf(&mmio_dev->regs, (uint32_t) (paddr >> 32));
    virtio_mmio_queue_desc_lo_addr_wrf(&mmio_dev->regs, (uint32_t) (paddr));

    paddr += size * sizeof(struct vring_desc);

    virtio_mmio_queue_used_hi_addr_wrf(&mmio_dev->regs, (uint32_t) (paddr >> 32));
    virtio_mmio_queue_used_lo_addr_wrf(&mmio_dev->regs, (uint32_t) (paddr));

    paddr += sizeof(uint16_t) * (2 + size + 1);
    paddr = (paddr + align - 1) & ~(align - 1);

    virtio_mmio_queue_avail_hi_addr_wrf(&mmio_dev->regs, (uint32_t) (paddr >> 32));
    virtio_mmio_queue_avail_lo_addr_wrf(&mmio_dev->regs, (uint32_t) (paddr));

    /* signal the host that the queue is ready */
    virtio_mmio_queue_ready_ready_wrf(&mmio_dev->regs, 0x1);
    virtio_mmio_queue_ready_signal_wrf(&mmio_dev->regs, 0x1);

    REGISTER_WAIT_READY(!virtio_mmio_queue_ready_signal_rdf, &mmio_dev);

    return SYS_ERR_OK;
}


static errval_t device_get_queue_num_max(struct virtio_device *dev,
                                         uint16_t queue_index,
                                         uint16_t *num_max)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *) dev;

    virtio_mmio_queue_sel_selector_wrf(&mmio_dev->regs, queue_index);

    /* TODO: wait till data ready */

    REGISTER_WAIT_READY(virtio_mmio_queue_sel_ready_rdf, &mmio_dev);

    uint16_t qmax = (uint16_t) virtio_mmio_queue_max_size_rdf(&mmio_dev->regs);

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

    VIRTIO_DEBUG_TL("setting negotiated features to: 0x%lx\n", driver_features);

    device_set_driver_features(dev, driver_features);

    dev->features = driver_features;

    return SYS_ERR_OK;
}

static errval_t device_notify_virtq(struct virtio_device *dev,
                                    uint16_t vq_id)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *) dev;

    virtio_mmio_queue_notify_index_wrf(&mmio_dev->regs, vq_id);

    return SYS_ERR_OK;
}

#endif

/**
 * \brief reads the device configuration space and copies it into a local buffer
 *
 * \param vdev  virtio device
 * \param buf   pointer to the buffer to store the data
 * \param len   the length of the buffer
 *
 * \returns SYS_ERR_OK on success
 */
static errval_t device_config_read(struct virtio_device *vdev,
                                   void *buf,
                                   size_t len)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *) vdev;

    if (len > (mmio_dev->dev_size + virtio_mmio_config_offset)) {
        return VIRTIO_ERR_SIZE_INVALID;
    }

    uint8_t *config_space = ((uint8_t *) mmio_dev->dev_base)
                    + virtio_mmio_config_offset;

    memcpy(buf, config_space, len);

    return SYS_ERR_OK;
}

/**
 * \brief writes to the configuration space of a device
 *
 * \param vdev  virtio device
 * \param buf   pointer to the buffer with data to update
 * \param len   the length of the buffer
 *
 * \returns SYS_ERR_OK on success
 */
static errval_t device_config_write(struct virtio_device *dev,
                                    void *config,
                                    size_t offset,
                                    size_t length)
{
    struct virtio_device_mmio *mmio_dev = (struct virtio_device_mmio *) dev;

    if ((length + offset) > (mmio_dev->dev_size + virtio_mmio_config_offset)) {
        return VIRTIO_ERR_SIZE_INVALID;
    }

    size_t config_offset = virtio_mmio_config_offset + offset;

    uint8_t *config_space = ((uint8_t *) mmio_dev->dev_base) + config_offset;

    memcpy(config_space, config, length);

    return SYS_ERR_OK;
}


#ifdef __VIRTIO_HOST__

static errval_t virtio_device_mmio_poll_host(struct virtio_device *host);

struct virtio_device_fn virtio_mmio_fn = {
    .set_status = device_set_status,
    .get_status = device_get_status,
    .get_config = device_config_read,
    .set_config = device_config_write,
    .poll = virtio_device_mmio_poll_host
};
#else
struct virtio_device_fn virtio_mmio_fn = {
    .reset = device_reset,
    .set_status = device_set_status,
    .get_status = device_get_status,
    .negotiate_features = device_negotiate_features,
    .set_virtq = device_set_virtq,
    .get_queue_num_max = device_get_queue_num_max,
    .get_config = device_config_read,
    .set_config = device_config_write,
    .notify = device_notify_virtq
};
#endif

/**
 * \brief initializes and allocates a VirtIO device structure for the MMIO backend
 */
errval_t virtio_device_mmio_init(struct virtio_device **dev,
                                 struct virtio_device_setup *info)
{
    struct virtio_device_mmio *mmio_dev;
    errval_t err;
    mmio_dev = calloc(1, sizeof(*mmio_dev));
    if (mmio_dev == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    virtio_mmio_initialize(&mmio_dev->regs,
                           (mackerel_addr_t) (info->backend.args.mmio.dev_base));
    mmio_dev->dev_base = info->backend.args.mmio.dev_base;
    mmio_dev->dev_size = info->backend.args.mmio.dev_size;

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

    if (devid != info->dev_type) {
        VIRTIO_DEBUG_DEV("VirtIO device id not as expected [%x, %x].\n",
                         devid,
                         info->dev_type);
        return VIRTIO_ERR_DEVICE_TYPE;
    }

    mmio_dev->dev.backend = VIRTIO_DEVICE_BACKEND_MMIO;
    mmio_dev->dev.f = &virtio_mmio_fn;
    mmio_dev->dev.dev_type = virtio_mmio_deviceid_id_rdf(&mmio_dev->regs);

    *dev = &mmio_dev->dev;

    return SYS_ERR_OK;
}

#ifdef __VIRTIO_HOST__

static errval_t handle_device_status_change(struct virtio_device_mmio *mmio_host,
                                            uint8_t new_status)
{
    VIRTIO_DEBUG_TL("handle_device_status_change: [0x%x]\n", new_status);
    mmio_host->dev_reg.status = new_status;
    return SYS_ERR_OK;
}

static errval_t handle_dev_feature_sel_change(struct virtio_device_mmio *mmio_host,
                                              uint8_t selector)
{
    VIRTIO_DEBUG_TL("handle_dev_feature_sel_change: [0x%x]\n", selector);
    mmio_host->dev_reg.dev_feature_sel = selector;

    if (selector) {
        virtio_mmio_dev_features_wr(&mmio_host->regs,
                                    (uint32_t) (mmio_host->dev.device_features >> 32));
    } else {
        virtio_mmio_dev_features_wr(&mmio_host->regs,
                                    (uint32_t) mmio_host->dev.device_features);
    }

    virtio_mmio_dev_features_sel_ready_wrf(&mmio_host->regs, 0x1);

    return SYS_ERR_OK;
}

static errval_t handle_driv_feature_change(struct virtio_device_mmio *mmio_host,
                                           uint8_t selector,
                                           uint32_t feature)
{
    VIRTIO_DEBUG_TL("handle_driv_feature_change: [0x%x] [0x%08x]\n",
                    selector,
                    feature);

    mmio_host->dev_reg.driv_feature_sel = selector;
    mmio_host->dev_reg.driv_features[selector] = feature;

    virtio_mmio_driv_features_sel_ready_wrf(&mmio_host->regs, 0x0);

    return SYS_ERR_OK;
}

static errval_t handle_queue_sel_change(struct virtio_device_mmio *mmio_host,
                                        uint16_t selector)
{
    VIRTIO_DEBUG_TL("handle_queue_sel_change: [0x%x]\n", selector);

    mmio_host->dev_reg.queue_sel = selector;

    /*
     * TODO: load the respective queue registers
     */

    virtio_mmio_queue_sel_ready_wrf(&mmio_host->regs, 0x0);

    return SYS_ERR_OK;
}

static errval_t handle_queue_change(struct virtio_device_mmio *mmio_host,
                                    uint16_t selector)
{
    VIRTIO_DEBUG_TL("handle_queue_change: [0x%x]\n", selector);

    virtio_mmio_queue_ready_signal_wrf(&mmio_host->regs, 0x0);

    return SYS_ERR_OK;
}

static errval_t handle_queue_notify(struct virtio_device_mmio *mmio_host,
                                    uint16_t queue)
{
    VIRTIO_DEBUG_TL("handle_queue_notify: [0x%x]\n", queue);
    if (mmio_host->dev.cb_h->notify) {
        return mmio_host->dev.cb_h->notify(&mmio_host->dev, queue);
    }
    return SYS_ERR_OK;
}

static errval_t virtio_device_mmio_poll_host(struct virtio_device *host)
{
    errval_t err = VIRTIO_ERR_DEVICE_IDLE;
    struct virtio_device_mmio *mmio_host = (struct virtio_device_mmio *) host;

    uint32_t reg_value, selector;

    reg_value = virtio_mmio_status_rd(&mmio_host->regs);
    if (mmio_host->dev_reg.status != (uint8_t) reg_value) {
        err = handle_device_status_change(mmio_host, (uint8_t) reg_value);
    }

    selector = virtio_mmio_dev_features_sel_selector_rdf(&mmio_host->regs);
    if (mmio_host->dev_reg.dev_feature_sel != selector) {
        err = handle_dev_feature_sel_change(mmio_host, selector);
    }

    reg_value = virtio_mmio_driv_features_rd(&mmio_host->regs);
    selector = virtio_mmio_driv_features_sel_selector_rdf(&mmio_host->regs);
    if ((selector != mmio_host->dev_reg.driv_feature_sel) || (mmio_host->dev_reg
                    .driv_features[selector]
                                                              != reg_value)) {
        if (virtio_mmio_driv_features_sel_ready_rdf(&mmio_host->regs)) {
            err = handle_driv_feature_change(mmio_host,
                                             (uint8_t) selector,
                                             reg_value);
        }
    } else {
        // we have to ack the guest
        virtio_mmio_driv_features_sel_ready_wrf(&mmio_host->regs, 0x0);
    }

    reg_value = virtio_mmio_driv_features_rd(&mmio_host->regs);
    selector = virtio_mmio_queue_sel_selector_rdf(&mmio_host->regs);
    if (selector != mmio_host->dev_reg.queue_sel) {
        err = handle_queue_sel_change(mmio_host, (uint16_t) selector);
    }

    if (virtio_mmio_queue_ready_signal_rdf(&mmio_host->regs)) {
        err = handle_queue_change(mmio_host, selector);
    }
    reg_value = virtio_mmio_queue_notify_index_rdf(&mmio_host->regs);
    if (virtio_mmio_queue_notify_index_rdf(&mmio_host->regs) != 0xFFFF) {
        err = handle_queue_notify(mmio_host, (uint16_t)reg_value);
        virtio_mmio_queue_notify_index_wrf(&mmio_host->regs, 0xFFFF);
    }



    /* TODO: poll the queues */

    return err;
}

/**
 * \brief initializes a VirtIO device on the host side using the MMIO transpot
 *
 * \param dev   returns a pointer to the newly allocated device structure
 * \param info  initialization parameters
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_device_mmio_init_host(struct virtio_device **host,
                                      struct virtio_device_setup *setup)
{
    struct virtio_device_mmio *mmio_host;
    errval_t err;

    assert(host);

    mmio_host = malloc(sizeof(*mmio_host));
    if (mmio_host == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    mmio_host->dev.vqh = calloc(setup->vq_num, sizeof(void *));
    if (mmio_host->dev.vqh == NULL) {
        free(mmio_host);
        return LIB_ERR_MALLOC_FAIL;
    }

    /*
     * TODO> Check for minimum MMIO devie size
     */

    if (capref_is_null(setup->backend.args.mmio.dev_cap)) {
        VIRTIO_DEBUG_DEV("allocating a new device frame.\n");
        setup->backend.args.mmio.dev_size += VIRTIO_MMIO_DEVICE_SIZE;
        err = frame_alloc(&mmio_host->dev.dev_cap,
                          setup->backend.args.mmio.dev_size,
                          &setup->backend.args.mmio.dev_size);
        if (err_is_fail(err)) {
            free(mmio_host->dev.vqh);
            free(mmio_host);
            return err;
        }
    } else {
        mmio_host->dev.dev_cap = setup->backend.args.mmio.dev_cap;
    }

    struct frame_identity id;
    err = invoke_frame_identify(mmio_host->dev.dev_cap, &id);
    if (err_is_fail(err)) {
        VIRTIO_DEBUG_DEV("ERROR: could not identify the frame.");
        return err;
    }

    assert((1UL<<id.bits) > VIRTIO_MMIO_DEVICE_SIZE);

    VIRTIO_DEBUG_DEV("Using frame [0x%016lx, 0x%lx] as device frame.\n",
                     id.base,
                     (1UL << id.bits));

    if (setup->backend.args.mmio.dev_base == NULL) {
        VIRTIO_DEBUG_DEV("mapping device frame.\n");
        err = vspace_map_one_frame_attr(&setup->backend.args.mmio.dev_base,
                                        setup->backend.args.mmio.dev_size,
                                        mmio_host->dev.dev_cap,
                                        VIRTIO_VREGION_FLAGS_DEVICE,
                                        NULL,
                                        NULL);
        if (err_is_fail(err)) {
            if (capref_is_null(setup->backend.args.mmio.dev_cap)) {
                cap_destroy(mmio_host->dev.dev_cap);
            }
            free(mmio_host->dev.vqh);
            free(mmio_host);
            return err;
        }
    } else {
        assert(setup->backend.args.mmio.dev_size > VIRTIO_MMIO_DEVICE_SIZE);
    }

    mmio_host->dev.device_features = setup->features;

    for (uint32_t i = 0; i < setup->vq_num; ++i) {
        /* todo: initialize virtqueues */
        // mmio_host->dev.vqh[i].ndesc = setup->queue_num_max[i];
    }

    mmio_host->dev_base = setup->backend.args.mmio.dev_base;
    mmio_host->dev_size = setup->backend.args.mmio.dev_size;

    VIRTIO_DEBUG_DEV("initialize mmio registers to [%016lx].\n",
                     (uintptr_t)mmio_host->dev_base);

    virtio_mmio_initialize(&mmio_host->regs,
                           (mackerel_addr_t) (mmio_host->dev_base));

    /* initialize the device with values */
    virtio_mmio_magic_value_wr(&mmio_host->regs, virtio_mmio_magic_value);
    virtio_mmio_deviceid_id_wrf(&mmio_host->regs, setup->dev_type);
    virtio_mmio_version_version_wrf(&mmio_host->regs, virtio_mmio_version_virtio10);

    virtio_mmio_dev_features_sel_wr(&mmio_host->regs, 0x0);
    virtio_mmio_driv_features_sel_wr(&mmio_host->regs, 0x0);
    virtio_mmio_queue_sel_wr(&mmio_host->regs, 0x0);

    virtio_mmio_dev_features_wr(&mmio_host->regs, (uint32_t) setup->features);
    virtio_mmio_queue_max_wr(&mmio_host->regs, setup->vq_setup[0].vring_ndesc);

    virtio_mmio_queue_notify_index_wrf(&mmio_host->regs, 0xFFFF);

    mmio_host->dev_reg.status = 0x0;
    mmio_host->dev_reg.driv_feature_sel = 0x0;
    mmio_host->dev_reg.dev_feature_sel = 0x0;
    mmio_host->dev_reg.queue_sel = 0x0;

    mmio_host->dev.f = &virtio_mmio_fn;

    mmio_host->dev.f->poll = virtio_device_mmio_poll_host;

    *host = &mmio_host->dev;

    return SYS_ERR_OK;

}

#endif

