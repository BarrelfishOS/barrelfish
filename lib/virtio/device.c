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
#include <virtio/virtio_device.h>

#ifndef __VIRTIO_HOST__
#include <virtio/virtio_guest.h>
#endif

#include "device.h"
#include "backends/virtio_mmio.h"
#include "backends/virtio_pci.h"

#include "debug.h"

/**
 * \brief initializes a new VirtIO device based on the values passed with the
 *        device init struct. The device registers have already to be mapped. *
 *
 * \param dev       device structure to initialize
 * \param init      additional information passed for the init process
 * \param dev_regs  memory location of the device registers
 */
errval_t virtio_device_open(struct virtio_device **dev,
                            struct virtio_device_setup *setup)
{
    errval_t err = SYS_ERR_OK;

    VIRTIO_DEBUG_DEV("virtio_device_open: [%s]\n",
                     setup->dev_name);

    switch (setup->backend.type) {
        case VIRTIO_DEVICE_BACKEND_PCI:
            /*
             * TODO: intialize the PCI device backend
             */
            assert(!"NYI: handling of the PCI backend\n");
            break;
        case VIRTIO_DEVICE_BACKEND_MMIO:
            if (setup->backend.args.mmio.dev_base == NULL
                            || setup->backend.args.mmio.dev_size == 0) {
                    return VIRTIO_ERR_DEVICE_REGISTER;
                }
            err = virtio_device_mmio_init(dev, setup);
            break;
        case VIRTIO_DEVICE_BACKEND_IO:
            /*
             * TODO: intialize the IO device backend
             */
            assert(!"NYI: handling of the IO backend\n");
            break;
        default:
            err = VIRTIO_ERR_BACKEND;
            VIRTIO_DEBUG_DEV("ERROR: unsupported backend: %u\n",
                             setup->backend.type);
            break;
    }

    if (err_is_fail(err)) {
        return err;
    }

    struct virtio_device *vdev = *dev;

    assert(vdev);

    vdev->state = VIRTIO_DEVICE_S_INITIALIZING;
    strncpy(vdev->dev_name, setup->dev_name, sizeof(vdev->dev_name));
    vdev->setup_fn = setup->setup_fn;
    vdev->dev_t_st = setup->dev_t_st;
    vdev->dev_cap = setup->dev_cap;

    /* 1. Reset the device. */
    err = virtio_device_reset(vdev);
    if (err_is_fail(err)) {
        goto failed;
    }

    /* 2. Set the ACKNOWLEDGE status bit: the guest OS has notice the device.*/
    err = virtio_device_set_status(vdev, VIRTIO_DEVICE_STATUS_ACKNOWLEDGE);
    if (err_is_fail(err)) {
        goto failed;
    }

    /* 3. Set the DRIVER status bit: the guest OS knows how to drive the device.*/
    err = virtio_device_set_status(vdev, VIRTIO_DEVICE_STATUS_DRIVER);
    if (err_is_fail(err)) {
        goto failed;
    }

    /* 4. Read device feature bits, and write the subset of feature bits understood by the OS and driver to the
     device. During this step the driver MAY read (but MUST NOT write) the device-specific configuration
     fields to check that it can support the device before accepting it.*/
    err = virtio_device_feature_negotiate(vdev, setup->features);
    if (err_is_fail(err)) {
        goto failed;
    }

    /* 5. Set the FEATURES_OK status bit. The driver MUST not accept new feature bits after this step.*/
    err = virtio_device_set_status(vdev, VIRTIO_DEVICE_STATUS_FEATURES_OK);
    if (err_is_fail(err)) {
        goto failed;
    }

    /* 6. Re-read device status to ensure the FEATURES_OK bit is still set: otherwise, the device does not
     support our subset of features and the device is unusable.*/
    uint32_t status = 0;
    err = virtio_device_get_status(vdev, &status);
    assert(err_is_ok(err));

    if (!virtio_mmio_status_features_ok_extract(status)) {
        goto failed;
    }

    /* 7. Perform device-specific setup, including discovery of virtqueues for the device, optional per-bus setup,
     reading and possibly writing the device’s virtio configuration space, and population of virtqueues.*/
    err = virtio_device_specific_setup(vdev, setup->setup_arg);
    if (err_is_fail(err)) {
        goto failed;
    }
    /* 8. Set the DRIVER_OK status bit. At this point the device is “live”. */
    VIRTIO_DEBUG_DEV("Device [%s] is now live.\n", vdev->dev_name);
    err = virtio_device_set_status(vdev, VIRTIO_DEVICE_STATUS_DRIVER_OK);
    assert(err_is_ok(err));

    return SYS_ERR_OK;

    failed:
    VIRTIO_DEBUG_DEV("Device initialization for [%s] failed..\n", vdev->dev_name);
    virtio_device_set_status(vdev, VIRTIO_DEVICE_STATUS_FAILED);

    return err;
}

/**
 * \brief initializes a new VirtIO device based on the values passed with the
 *        device init struct. The supplied cap contains the memory range of the
 *        device registers.
 *
 * \param dev       device structure to initialize
 * \param init      additional information passed for the init process
 * \param dev_cap   capability representing the device registers
 */
errval_t virtio_device_open_with_cap(struct virtio_device **dev,
                                     struct virtio_device_setup *setup,
                                     struct capref dev_cap)
{
    errval_t err;

    assert(!capref_is_null(dev_cap));

    struct frame_identity id;
    err = invoke_frame_identify(dev_cap, &id);
    if (err_is_fail(err)) {
        VIRTIO_DEBUG_DEV("ERROR: could not identify the device frame.\n");
        return err;
    }
    size_t dev_size = (1UL << id.bits);
    void *dev_base;
    err = vspace_map_one_frame_attr(&dev_base, dev_size, dev_cap,
                                    VIRTIO_VREGION_FLAGS_DEVICE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        VIRTIO_DEBUG_DEV("ERROR: mapping the device register frame failed.\n");
        return err;
    }

    switch (setup->backend.type) {
        case VIRTIO_DEVICE_BACKEND_IO:
        case VIRTIO_DEVICE_BACKEND_PCI:
            return LIB_ERR_NOT_IMPLEMENTED;
            break;
        case VIRTIO_DEVICE_BACKEND_MMIO:
            setup->backend.args.mmio.dev_size = dev_size;
            setup->backend.args.mmio.dev_base = dev_base;
            break;
        default:
            break;
    }

    VIRTIO_DEBUG_DEV("mapped device registers: [0x%016lx] -> [0x%016lx]\n",
                     id.base,
                     (uintptr_t )dev_base);

    setup->dev_cap = dev_cap;

    err = virtio_device_open(dev, setup);
    if (err_is_fail(err)) {
        vspace_unmap(dev_base);
    }

    return err;
}

/**
 * \brief checks if the device supports a certain feature
 *
 * \param dev       the device to query for the feature
 * \param feature   the featurebit to check
 *
 * \returns true  if the device supports that feature
 *          false if the device does not support that feature
 */
bool virtio_device_has_feature(struct virtio_device *dev,
                               uint8_t feature)
{
    /*
     * if the device is not configured yet, we don't know the features
     */
    if (dev->dev_status & VIRTIO_DEVICE_STATUS_FEATURES_OK) {
        return false;
    }

    return (dev->features & (1UL << feature)) != 0;
}

errval_t virtio_device_specific_setup(struct virtio_device *dev,
                                      void *arg)
{
    if (dev->setup_fn) {
        return dev->setup_fn(dev, arg);
    }

    return SYS_ERR_OK;
}

/**
 * \brief resets the virtio device
 *
 * \param dev   the device to reset
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_device_reset(struct virtio_device *dev)
{
    if (dev->f->reset) {
        return dev->f->reset(dev);
    }
    return SYS_ERR_OK;
}

/**
 * \brief returns the status of a virtio device
 *
 * \param the device to query for status
 * \param returned status
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_device_get_status(struct virtio_device *dev,
                                  uint32_t *ret_status)
{
    if (dev->f->get_status) {
        return dev->f->get_status(dev, ret_status);
    }
    return VIRTIO_ERR_BACKEND;
}

/**
 * \brief
 *
 * \param
 */
errval_t virtio_device_set_status(struct virtio_device *dev,
                                  uint8_t status)
{
    if (dev->f->set_status) {
        return dev->f->set_status(dev, status);
    }
    return VIRTIO_ERR_BACKEND;
}

/**
 * \brief Returns the pointer to the device specific structure
 *
 * \param vdev to get the device specific pointer
 *
 * \returns device specific struct pointer
 */
void *virtio_device_get_type_state(struct virtio_device *vdev)
{
    return vdev->dev_t_st;
}

/**
 * \brief notifies the host about new descriptors available in the
 *        available ring
 *
 * \param vdev      VirtIO device
 * \param virtq_id  the virtq to signal on
 *
 * \return SYS_ERR_OK on success
 */
errval_t virtio_device_notify_host(struct virtio_device *vdev,
                                   uint16_t virtq_id)
{
    if (vdev->f->notify) {
        vdev->f->notify(vdev, virtq_id);
    }
    return VIRTIO_ERR_BACKEND;
}

/**
 * \brief reads the device configuration space and copies it into a local buffer
 *
 * \param vdev  virtio device
 * \param buf   pointer to the buffer to store the data
 * \param len   the length of the buffer
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_device_config_read(struct virtio_device *vdev,
                                   void *buf,
                                   size_t len)
{
    if (vdev->f->get_config) {
        return vdev->f->get_config(vdev, buf, len);
    }

    return VIRTIO_ERR_BACKEND;
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
errval_t virtio_device_config_write(struct virtio_device *dev,
                                    void *config,
                                    size_t offset,
                                    size_t length)
{
    if (dev->f->set_config) {
        return dev->f->set_config(dev, config, offset, length);
    }

    return VIRTIO_ERR_BACKEND;
}

errval_t virtio_device_set_driver_features(struct virtio_device *dev,
                                           uint64_t features)
{
    assert(!"NYI:");
    return SYS_ERR_OK;
}

errval_t virtio_device_get_device_features(struct virtio_device *dev,
                                           uint64_t *ret_features)
{
    assert(!"NYI:");
    return SYS_ERR_OK;
}

errval_t virtio_device_feature_negotiate(struct virtio_device *dev,
                                         uint64_t driver_features)
{
    if (dev->f->negotiate_features) {
        return dev->f->negotiate_features(dev, driver_features);
    }

    return VIRTIO_ERR_BACKEND;
}


#ifdef __VIRTIO_HOST__
/**
 * \brief returns a pointer to a virtqueue of the device
 *
 * \param vdev   VirtIO device
 * \param vq_idx the queue index of the queue we want
 *
 * \returns pointer to the requested virtqueue
 *          NULL if no such virtqueue exists
 */
struct virtqueue_host *virtio_device_get_host_virtq(struct virtio_device *vdev,
                                                    uint16_t vq_idx)
{
    if (vq_idx < vdev->vq_num) {
        return vdev->vqh[vq_idx];
    }

    return NULL;
}

#else

errval_t virtio_device_set_virtq(struct virtio_device *dev,
                                 struct virtqueue *vq)
{
    if (dev->f->set_virtq) {
        return dev->f->set_virtq(dev, vq);
    }

    return VIRTIO_ERR_BACKEND;
}

/**
 * \brief allocates the virtqueues for this device based on the setup information
 *
 * \param vdev      virtio device to allocate the queues for
 * \param vq_setup  setup information for the virtqueues
 * \param vq_num    number of virtqueues to allocate
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_device_virtqueue_alloc(struct virtio_device *vdev,
                                       struct virtqueue_setup *vq_setup,
                                       uint16_t vq_num)
{
    errval_t err;

    VIRTIO_DEBUG_VQ("Allocating %u virtqueues for [%s].\n", vq_num, vdev->dev_name);

    vdev->vq = calloc(vq_num, sizeof(void *));
    if (vdev->vq == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct virtqueue_setup *setup;
    for (uint16_t i = 0; i < vq_num; ++i) {
        setup = &vq_setup[i];
        setup->queue_id = i;
        setup->device = vdev;
        err = virtio_virtqueue_alloc(setup, &vdev->vq[i]);
        if (err_is_fail(err)) {
            for (uint16_t j = 0; j < i; ++j) {
                virtio_virtqueue_free(vdev->vq[i]);
            }
            free(vdev->vq);
            return err;
        }
    }

    for (uint16_t i = 0; i < vq_num; ++i) {
        setup = &vq_setup[i];
        struct virtqueue *vq = vdev->vq[i];
        if (setup->auto_add) {
            err = virtio_guest_add_virtq(vq);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not add the viring\n");
            }
            err = virtio_device_set_virtq(vdev, vq);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "adding the virtqueue to the deivce\n");
            }
            // TODO propper error handling
        }
    }

    vdev->vq_num = vq_num;

    return SYS_ERR_OK;
}

/**
 * \brief returns a pointer to a virtqueue of the device
 *
 * \param vdev   VirtIO device
 * \param vq_idx the queue index of the queue we want
 *
 * \returns pointer to the requested virtqueue
 *          NULL if no such virtqueue exists
 */
struct virtqueue *virtio_device_get_virtq(struct virtio_device *vdev,
                                          uint16_t vq_idx)
{
    if (vq_idx < vdev->vq_num) {
        return vdev->vq[vq_idx];
    }

    return NULL;
}
#endif
