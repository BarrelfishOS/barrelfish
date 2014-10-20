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
enum virtio_backend {
    VIRTIO_DEVICE_BACKEND_INVALID,
    VIRTIO_DEVICE_BACKEND_PCI,
    VIRTIO_DEVICE_BACKEND_MMIO,
    VIRTIO_DEVICE_BACKEND_IO,
};

/**
 * driver specific device status
 */
enum virtio_state {
    VIRTIO_DEVICE_S_INVALID,
    VIRTIO_DEVICE_S_INITIALIZING,
    VIRTIO_DEVICE_S_READY,
    VIRTIO_DEVICE_S_TERMINATING,
    VIRTIO_DEVICE_S_ERROR,
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

#define VIRTIO_DEVICE_HC_IFACE_MAX 32


/*
 * Function pointers type definitions
 */

/// interrupt handler when the device configuration space changes
typedef void (*config_intr_handler_t)(struct virtio_device *dev);

/// device specific initialization function
typedef errval_t (*virtio_device_setup_t)(struct virtio_device *dev,
                                         void *state);

struct virtio_backend_arg {
    enum virtio_backend  type;
    union {
        struct {
            struct capref dev_cap;      ///<
            void   *dev_base;
            size_t  dev_size;
        } mmio;
        struct {
            struct capref dev_cap;
            /* TODO: fill in needed fieds */
        } pci;
        struct {
            struct capref dev_cap;
            /* TODO: fill in needed fieds */
        } io;
    } args;
};


/**
 * contains necessary values for the device initialization process
 */
struct virtio_device_setup
{
    uint8_t dev_type;           ///< expected VirtIO type of the device
    char    dev_name[VIRTIO_DEVICE_NAME_MAX];
    struct capref dev_cap;
    void   *dev_t_st;           ///< pointer to device type specific state
    uint64_t features;          ///< VirtIO feature bits supported

    struct virtio_backend_arg  backend;    ///< arguments for the backend

    virtio_device_setup_t setup_fn;
    void                 *setup_arg;

    config_intr_handler_t config_intr_fn;

    uint16_t vq_num;
    struct virtqueue_setup *vq_setup;
#ifdef __VIRTIO_HOST__
    enum virtio_host       hc_type;
    char                  *hc_iface;
    struct virtio_host_cb *hc_cb;
    lpaddr_t               hc_offset;
#endif
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
                                  uint32_t *ret_status);

/**
 * \brief sets the status bit of the device
 *
 * \param dev    the VirtIO device
 * \param status the status bit to set
 *
 * \return SYS_ERR_OK on success
 */
errval_t virtio_device_set_status(struct virtio_device *dev,
                                  uint8_t status);

/**
 * \brief tells the device which features the driver understands
 *
 * \param dev       the VirtIO device
 * \param features  bitmap of understood features
 *
 * \return SYS_ERR_OK on success
 */
errval_t virtio_device_set_driver_features(struct virtio_device *dev,
                                           uint64_t features);

/**
 * \brief queries the device which features it understands
 *
 * \param dev       the VirtIO device
 * \param features  bitmap of understood features by the device
 *
 * \return SYS_ERR_OK on success
 */
errval_t virtio_device_get_device_features(struct virtio_device *dev,
                                           uint64_t *ret_features);

/**
 * \brief wrapper for calling the device specific initialization function
 *
 * \param dev   the VirtIO device
 * \param arg   argument pointer to the initialization function
 *
 * \returns SYS_ERR_OK on success
 *          VIRTIO_ERR_* on failure
 */
errval_t virtio_device_specific_setup(struct virtio_device *dev,
                                      void *arg);

/**
 * \brief checks if a certain feature is negotiated and understood by both
 *        device and driver.
 *
 * \param dev       the VirtIO device
 * \param feature   feature bit to check
 *
 * \returns true  - if feature bit was set
 *          false - if the featurebit was not set
 */
bool virtio_device_has_feature(struct virtio_device *dev,
                               uint8_t feature);

/**
 * \brief negotiates the supported features based on the offered device features
 *        and the supplied driver features
 *
 * \param dev            the VirtIO device
 * \param driver_featurs bitmask of understood features by the device
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_device_feature_negotiate(struct virtio_device *dev,
                                         uint64_t driver_features);

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
 */
errval_t virtio_device_config_write(struct virtio_device *dev,
                                    void *config,
                                    size_t offset,
                                    size_t length);

/**
 * \brief Returns the pointer to the device specific structure
 *
 * \param vdev to get the device specific pointer
 *
 * \returns device specific struct pointer
 */
void *virtio_device_get_type_state(struct virtio_device *vdev);

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
                                       uint16_t vq_num);

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
                                   uint16_t virtq_id);

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
                                                    uint16_t vq_idx);
#else
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
                                          uint16_t vq_idx);


/**
 * \brief exposes the virtqueue to the device such that it can be used
 *
 * \param dev the VirtIO device
 * \param vq  the virtqueue to be added to the device
 *
 * \return SYS_ERR_OK on success
 */
errval_t virtio_device_set_virtq(struct virtio_device *dev,
                                 struct virtqueue *vq);
#endif
/**
 * \brief sets the interrupt handler for the configuration space interrupts
 *
 * \param dev the VirtIO device
 * \param fn  handler function
 *
 * \return SYS_ERR_OK on success
 */
errval_t virtio_device_set_config_intr_handler(struct virtio_device *dev,
                                               config_intr_handler_t fn);

#endif // VIRTIO_VIRTIO_DEVICE_H
