/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_VIRTIO_DEVICE_H
#define VIRTIO_VIRTIO_DEVICE_H


struct virtio_device;

/*
 * 2.1 Device Status Field
 */

/// The device is in the reset state (not discovered by the guest)
#define VIRTIO_DEVICE_STATUS_RESET       0x00

/// Guest OS has found the device and recognized it as a valid virtio device.
#define VIRTIO_DEVICE_STATUS_ACKNOWLEDGE 0x01

/// Guest OS knows how to drive the device i.e. recognized as valid virtio device.
#define VIRTIO_DEVICE_STATUS_DRIVER      0x02

/// Driver is set up and ready to drive the device.
#define VIRTIO_DEVICE_STATUS_DRIVER_OK   0x04

/// Driver has acknowledged all the features it understands
#define VIRTIO_DEVICE_STATUS_FEATURES_OK 0x08

/// Something went wrong in the guest, and it has given up on the device.
#define VIRTIO_DEVICE_STATUS_FAILED      0x80

/*
 * 5.0 Device Types
 * The following device IDs are used to identify different types of virtio
 * devices. Some device IDs are reserved for devices which are not currently
 * defined in this standard.
 */

/// Invalid device identifier
#define VIRTIO_DEVICE_TYPE_INVALID   0x00

/// Device type for network interface cards
#define VIRTIO_DEVICE_TYPE_NET       0x01

/// Device type for block devices
#define VIRTIO_DEVICE_TYPE_BLOCK     0x02

/// Device type for console devices
#define VIRTIO_DEVICE_TYPE_CONSOLE   0x03

/// Device type for entorpy devices
#define VIRTIO_DEVICE_TYPE_ENTORPY   0x04

//#define VIRTIO_DEVICE_TYPE_LEGACY_BALLOON 5

/// Device type for IO memory devices
#define VIRTIO_DEVICE_TYPE_IOMEM     0x06

/// Device type for rpmgs devices
#define VIRTIO_DEVICE_TYPE_RPMSG     0x07

/// Device type for SCSI host devices
#define VIRTIO_DEVICE_TYPE_SCSIHOST  0x08

/// Device type for 9P transport devices
#define VIRTIO_DEVICE_TYPE_9PTRANSP  0x09

/// Device type for MAC 802.11 WLAn devices
#define VIRTIO_DEVICE_TYPE_WLAN      0x0A

/// Device type for RPROC serial devices
#define VIRTIO_DEVICE_TYPE_SERIAL    0x0B

/// Device type for virtio CAIF devices
#define VIRTIO_DEVICE_TYPE_CAIF      0x0C

/// Device type for memory ballooning devices
#define VIRTIO_DEVICE_TYPE_BALLOON   0x0D

/// Device type for GPU devices
#define VIRTIO_DEVICE_TYPE_GPU       0x0E

/// Device type for timer / clock devices
#define VIRTIO_DEVICE_TYPE_TIMER     0x0F

/**
 * specifies the possible virtio backends to be used
 */
enum virtio_device_backend {
    VIRTIO_DEVICE_BACKEND_INVALID,
    VIRTIO_DEVICE_BACKEND_PCI,
    VIRTIO_DEVICE_BACKEND_MMIO,
    VIRTIO_DEVICE_BACKEND_IO,
};

/*
 * 4.1.2 PCI Device Discovery
 *
 * Any PCI device with Vendor ID 0x1AF4, and Device ID 0x1000 through 0x103F
 * inclusive is a virtio device. The Subsystem Device ID indicates which virtio
 * device is supported by the device.
 */

#define VIRTIO_PCI_VENDOR_ID 0x1AF4
#define VIRTIO_PCI_DEVICE_ID 0x1000
#define VIRTIO_PCI_DEVICE_ID2 0x103F

#define VIRTIO_DEVICE_NAME_MAX 32

/**
 * contains necessary values for the device initialization process
 */
struct virtio_device_setup
{
    uint8_t type;                           ///< expected type of the device
    char name[VIRTIO_DEVICE_NAME_MAX];
    enum virtio_device_backend  backend;    ///< which backend to use
    void *dev_reg;
    size_t dev_reg_size;
    errval_t (*feature_negotiate)(struct virtio_device *dev);
    errval_t (*device_setup)(struct virtio_device *dev);
};



/**
 * \brief initializes a new VirtIO device based on the values passed with the
 *        device init struct. The device registers have already to be mapped. *
 *
 * \param dev       device structure to initialize
 * \param init      additional information passed for the init process
 * \param dev_regs  memory location of the device registers
 */
errval_t virtio_device_open(struct virtio_device **dev,
                            struct virtio_device_setup *init);

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
                                     struct capref dev_cap);

/**
 * \brief   closes a virtio device.
 *
 * \param dev the device to be closed
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_device_close(struct virtio_device *dev);

/**
 * \brief resets the virtio device
 *
 * \param dev   the device to reset
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_device_reset(struct virtio_device *dev);

/**
 * \brief returns the status of a virtio device
 *
 * \param the device to query for status
 * \param returned status
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_device_get_status(struct virtio_device *dev,
                                  uint8_t *ret_status);

/**
 * \brief
 *
 * \param
 */
errval_t virtio_device_set_status(struct virtio_device *dev,
                                  uint8_t status);


errval_t virtio_device_feature_negotiate(struct virtio_device *dev);

errval_t virtio_device_specific_setup(struct virtio_device *dev);

bool     virtio_device_has_feature(struct virtio_device *dev,
                                   uint8_t feature);


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
                                  size_t len);
/**
 * \brief writes to the configuration space of a device
 *
 * \param vdev  virtio device
 * \param buf   pointer to the buffer with data to update
 * \param len   the length of the buffer
 *
 * \returns SYS_ERR_OK on success
 *
 * xxx: this may be done at a certain offset/value combination
 */
errval_t virtio_device_config_write(struct virtio_device *dev,
                                    void *config,
                                    size_t length);

#endif // VIRTIO_VIRTIO_DEVICE_H
