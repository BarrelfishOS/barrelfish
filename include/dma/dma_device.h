/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_DEVICE_H
#define LIB_DMA_DEVICE_H

struct dma_device;
struct dma_channel;

/**
 * Device State Enumeration
 */
typedef enum dma_dev_st {
    DMA_DEV_ST_INVALID,        ///< this device structure is invalid
    DMA_DEV_ST_UNINITIALIZED,  ///< device has not been initialized yet
    DMA_DEV_ST_CHAN_ENUM,      ///< device is doing channel enumeration
    DMA_DEV_ST_RUNNING,        ///< device is operational
    DMA_DEV_ST_SUSPENDED,      ///< device is suspended state
    DMA_DEV_ST_ERR             ///< an error occurred
} dma_dev_st_t;


/*
 * ----------------------------------------------------------------------------
 * device initialization / termination
 * ----------------------------------------------------------------------------
 */

/**
 * \brief terminates the device operation and frees up the allocated resources
 *
 * \param dev IOAT DMA device to shutdown
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_device_shutdown(struct dma_device *dev);

/**
 * \brief requests access to a IOAT DMA device from the IOAT device manager
 *        and initializes the device.
 *
 * \param dev  returns a pointer to the device structure
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_device_acquire(struct dma_device **dev);

/**
 * \brief terminates the device operation and frees up the allocated resources
 *        and releases the device and returns it to the IOAT device manager.
 *
 * \param dev IOAT DMA device to be released
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_device_release(struct dma_device *dev);

/**
 * \brief polls the channels of the IOAT DMA device
 *
 * \param dev   IOAT DMA device
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_DEVICE_IDLE if there is nothing completed on the channels
 *          errval on error
 */
errval_t dma_device_poll_channels(struct dma_device *dev);

/*
 * ----------------------------------------------------------------------------
 * Getters / Setters
 * ----------------------------------------------------------------------------
 */

/**
 * \brief gets the ID of the DMA device
 *
 * \param dev   DMA device
 *
 * \returns DMA device ID
 */
dma_dev_id_t dma_device_get_id(struct dma_device *dev);

/**
 * \brief gets the DMA device type of the DMA device
 *
 * \param dev   DMA device
 *
 * \returns DMA device type
 */
dma_dev_id_t dma_device_get_type(struct dma_device *dev);

/**
 * \brief gets the state of the DMA device
 *
 * \param dev   DMA device
 *
 * \returns DMA device state
 */
dma_dev_st_t dma_device_get_state(struct dma_device *dev);


/**
 * \brief gets the virtual address of the mapped MMIO register space
 *
 * \param dev   DMA device
 *
 * \returns MMIO register vbase
 */
void *dma_device_get_mmio_vbase(struct dma_device *dev);


/**
 * \brief gets the number of channels this device has
 *
 * \param dev   DMA device
 *
 * \returns DMA channel count
 */
uint8_t dma_device_get_channel_count(struct dma_device *dev);

/**
 * \brief obtains the channel associated with the given index
 *
 * \param dev   DMA device
 *
 * \returns DMA channel if index exists
 *          NULL if index exceeds channel count
 */
struct dma_channel *dma_device_get_channe_by_idx(struct dma_device *dev,
                                                 uint8_t idx);

/**
 * \brief gets a DMA channel from this device in a round robin fashion
 *
 * \param dev   DMA device
 *
 * \returns DMA channel
 */
struct dma_channel *dma_device_get_channel(struct dma_device *dev);


/*
 * ----------------------------------------------------------------------------
 * Interrupt management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief enables the interrupts for the device
 *
 * \param dev     IOAT DMA device
 * \param type    interrupt type
 * \param handler interrupt handler function
 * \param arg     argument supplied to the handler function
 */
errval_t dma_device_intr_enable(struct dma_device *dev,
                                dma_irq_t type,
                                dma_irq_fn_t handler,
                                void *arg);

/**
 * \brief disables the interrupts for the device
 *
 * \param dev   IOAT DMA device
 */
void dma_device_intr_disable(struct dma_device *dev);

/**
 * \brief sets the interrupt delay for the device
 *
 * \param dev   IOAT DMA device
 * \param usec  interrupt delay in microseconds
 */
void dma_device_set_intr_delay(struct dma_device *dev,
                               uint16_t usec);


#endif  /* LIB_DMA_CHANNEL_H */
