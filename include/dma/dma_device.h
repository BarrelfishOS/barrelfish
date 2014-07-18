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

/// IOAT DMA channel id
typedef uint8_t dma_dev_id_t;

/// representation of a PCI address (with more bits)
struct pci_addr {
    uint32_t bus;
    uint32_t device;
    uint32_t function;
};

typedef enum dma_dev_type {

} dma_dev_type_t;

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


/**
 * Enumeration of possible interrupt types supported by the hardware
 */
typedef enum dma_irq {
    DMA_IRQ_DISABLED,          ///< interrupts are disabled
    DMA_IRQ_MSIX,              ///< use MSI-X interrupts
    DMA_IRQ_MSI,               ///< use MSI interrupts
    DMA_IRQ_INTX,              ///< use normal INTx interrupts
} dma_irq_t;

/// interrupt handler function
typedef void (*dma_irq_fn_t)(struct dma_channel *dev, void *);

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


/*
 * ----------------------------------------------------------------------------
 * Interrupt management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief enables the interrupts for the device
 *
 * \param dev   IOAT DMA device
 * \param type  interrupt type
 * \param fn    interrupt handler function
 * \param arg   argument supplied to the handler function
 */
errval_t dma_device_intr_enable(struct dma_device *dev,
                                     dma_irq_t type,
                                     dma_irq_fn_t fn,
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

/*
 * ----------------------------------------------------------------------------
 * Device Operation Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief gets the device state from the IOAT DMA device
 *
 * \param dev   IOAT DMA device
 *
 * \returns device state enumeration
 */
dma_dev_st_t dma_device_get_state(struct dma_device *dev);


/**
 * \brief returns the channel count of this device
 *
 * \param dev   IOAT DMA device
 *
 * \returns number of channels this device has
 */
uint8_t dma_device_get_channel_count(struct dma_device *dev);

/**
 * \brief returns the device ID from the IOAT device
 *
 * \param dev   IOAT DMA device
 *
 * \returns IOAT DMA device ID
 */
dma_dev_id_t dma_device_get_id(struct dma_device *dev);

/**
 * \brief returns the channel belonging with the given ID
 *
 * \param dev   IOAT DMA device
 * \param id    channel id
 *
 * return IOAT DMA channel handle
 *        NULL if no such channel exist
 */
struct dma_channel *dma_device_get_channel(struct dma_device *dev,
                                           uint16_t id);

/**
 * \brief returns a channel from the device based on a round robin fashion
 *
 * \param dev   IOAT DMA device
 *
 * return IOAT DMA channel handle
 */
struct dma_channel *dma_device_get_next_channel(struct dma_device *dev);



#endif  /* LIB_DMA_CHANNEL_H */
