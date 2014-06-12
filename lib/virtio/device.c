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
#include <virtio/virtio_device.h>

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
                            struct virtio_device_setup *init)
{
    errval_t err = SYS_ERR_OK;

    if (init->dev_reg == NULL || init->dev_reg_size == 0) {
        /*
         * XXX: does this also hold for the PCI
         */
        return VIRTIO_ERR_DEVICE_REGISTER;
    }

    switch (init->backend) {
        case VIRTIO_DEVICE_BACKEND_PCI:
            /*
             * TODO: intialize the PCI device backend
             */
            assert(!"NYI: handling of the PCI backend\n");
            break;
        case VIRTIO_DEVICE_BACKEND_MMIO:
            err = virtio_device_mmio_init(dev, init);
            break;
        case VIRTIO_DEVICE_BACKEND_IO:
            /*
             * TODO: intialize the IO device backend
             */
            assert(!"NYI: handling of the IO backend\n");
            break;
        default:
            err = VIRTIO_ERR_BACKEND;
            break;
    }

    if (err_is_fail(err)) {
        return err;
    }

    struct virtio_device *vdev = *dev;

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
    err = virtio_device_feature_negotiate(vdev);
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
    uint8_t status;
    err = virtio_device_get_status(vdev, &status);
    assert(err_is_ok(err));

    if (!virtio_mmio_status_features_ok_extract(status)) {
        goto failed;
    }

    /* 7. Perform device-specific setup, including discovery of virtqueues for the device, optional per-bus setup,
     reading and possibly writing the device’s virtio configuration space, and population of virtqueues.*/
    err = virtio_device_specific_setup(vdev);
    if (err_is_fail(err)) {
        goto failed;
    }
    /* 8. Set the DRIVER_OK status bit. At this point the device is “live”. */
    err = virtio_device_set_status(vdev, VIRTIO_DEVICE_STATUS_DRIVER_OK);
    assert(err_is_ok(err));

    if (init->device_setup) {
        return init->device_setup(vdev);
    }

    return SYS_ERR_OK;

    failed: virtio_device_set_status(vdev, VIRTIO_DEVICE_STATUS_FAILED);

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
                                     struct virtio_device_setup *init,
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

    init->dev_reg_size = (1UL << id.bits);

    err = vspace_map_one_frame_attr(&init->dev_reg, init->dev_reg_size, dev_cap,
    VIRTIO_VREGION_FLAGS_DEVICE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        VIRTIO_DEBUG_DEV("ERROR: mapping the device register frame failed.\n");
        init->dev_reg = NULL;
        init->dev_reg_size = 0;
        return err;
    }

    VIRTIO_DEBUG_DEV("mapped device registers: [0x%016lx] -> [0x%016lx]\n",
                     id.base,
                     (uintptr_t )init->dev_reg);

    err = virtio_device_init(dev, init);
    if (err_is_fail(err)) {
        vspace_unmap(init->dev_reg);
    }

    return err;
}

