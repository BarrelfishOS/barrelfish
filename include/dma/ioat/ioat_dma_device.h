/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_IOAT_DMA_DEVICE_H
#define LIB_IOAT_DMA_DEVICE_H

/// forward declaration of the device
struct ioat_dma_device;
struct ioat_dma_channel;



/*
 * ----------------------------------------------------------------------------
 * device initialization / termination
 * ----------------------------------------------------------------------------
 */

/**
 * \brief initializes a IOAT DMA device with the giving capability
 *
 * \param mmio capability representing the device's MMIO registers
 * \param dev  returns a pointer to the device structure
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_dma_device_init(struct capref mmio,
                              struct ioat_dma_device **dev);

/**
 * \brief terminates the device operation and frees up the allocated resources
 *
 * \param dev IOAT DMA device to shutdown
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_dma_device_shutdown(struct ioat_dma_device *dev);

/**
 * \brief requests access to a IOAT DMA device from the IOAT device manager
 *        and initializes the device.
 *
 * \param dev  returns a pointer to the device structure
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_dma_device_acquire(struct ioat_dma_device **dev);

/**
 * \brief terminates the device operation and frees up the allocated resources
 *        and releases the device and returns it to the IOAT device manager.
 *
 * \param dev IOAT DMA device to be released
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_dma_device_release(struct ioat_dma_device *dev);


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
errval_t ioat_dma_device_intr_enable(struct ioat_dma_device *dev,
                                     dma_irq_t type,
                                     dma_irq_fn_t fn,
                                     void *arg);

/**
 * \brief disables the interrupts for the device
 *
 * \param dev   IOAT DMA device
 */
void ioat_dma_device_intr_disable(struct ioat_dma_device *dev);

/**
 * \brief sets the interrupt delay for the device
 *
 * \param dev   IOAT DMA device
 * \param usec  interrupt delay in microseconds
 */
void ioat_dma_device_set_intr_delay(struct ioat_dma_device *dev,
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
dma_dev_st_t ioat_dma_device_get_state(struct ioat_dma_device *dev);


/**
 * \brief returns the channel count of this device
 *
 * \param dev   IOAT DMA device
 *
 * \returns number of channels this device has
 */
uint8_t ioat_dma_device_get_channel_count(struct ioat_dma_device *dev);

/**
 * \brief returns the device ID from the IOAT device
 *
 * \param dev   IOAT DMA device
 *
 * \returns IOAT DMA device ID
 */
dma_dev_id_t ioat_dma_device_get_id(struct ioat_dma_device *dev);

/**
 * \brief returns the channel belonging with the given ID
 *
 * \param dev   IOAT DMA device
 * \param id    channel id
 *
 * return IOAT DMA channel handle
 *        NULL if no such channel exist
 */
struct ioat_dma_channel *ioat_dma_device_get_channel(struct ioat_dma_device *dev,
                                                     uint16_t id);

/**
 * \brief returns a channel from the device based on a round robin fashion
 *
 * \param dev   IOAT DMA device
 *
 * return IOAT DMA channel handle
 */
struct ioat_dma_channel *ioat_dma_device_get_next_channel(struct ioat_dma_device *dev);


/**
 * \brief polls the channels of the IOAT DMA device
 *
 * \param dev   IOAT DMA device
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_DEVICE_IDLE if there is nothing completed on the channels
 *          errval on error
 */
errval_t ioat_dma_device_poll_channels(struct ioat_dma_device *dev);



#endif  /* LIB_IOAT_DMA_DEVICE_H */
