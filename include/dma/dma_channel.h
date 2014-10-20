/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_CHANNEL_H
#define LIB_DMA_CHANNEL_H

struct dma_channel;

/// DMA channel state
typedef enum dma_chan_st
{
    DMA_CHAN_ST_INVALID,
    DMA_CHAN_ST_RESETTING,
    DMA_CHAN_ST_UNINITIALEZED,
    DMA_CHAN_ST_PREPARED,
    DMA_CHAN_ST_RUNNING,
    DMA_CHAN_ST_ERROR,
    DMA_CHAN_ST_SUSPENDED
} dma_chan_st_t;

/**
 * \brief polls the DMA channel for completed events
 *
 * \param chan  DMA Channel
 *
 * \returns SYS_ERR_OK if there was something processed
 *          DMA_ERR_CHAN_IDLE if there was no request on the channel
 *          DMA_ERR_REQUEST_UNFINISHED if the request has not been completed yet
 *
 */
errval_t dma_channel_poll(struct dma_channel *chan);

/*
 * ----------------------------------------------------------------------------
 * Getter / Setter Methods
 * ----------------------------------------------------------------------------
 */

/**
 * \brief gets the ID of the channel
 *
 * \param DMA channel
 *
 * \returns DMA channel ID
 */
dma_chan_id_t dma_channel_get_id(struct dma_channel *chan);

/**
 * \brief gets the state of the channel
 *
 * \param DMA channel
 *
 * \returns DMA channel state
 */
dma_chan_st_t dma_channel_get_state(struct dma_channel *chan);

 /**
  * \brief gets the DMA device this channel belongs to
  *
  * \param DMA channel
  *
  * \returns DMA device
  */
struct dma_device *dma_channel_get_device(struct dma_channel *chan);

/**
 * \brief gets the number of unfinished requests on this channel
 *
 * \param DMA channel
 *
 * \returns request count
 */
uint32_t dma_channel_get_request_count(struct dma_channel *chan);

/**
 * \brief gets the address where the MMIO registers are mapped
 *
 * \param DMA channel
 *
 * \returns MMIO register vbase
 */
lvaddr_t dma_channel_get_mmio_vbase(struct dma_channel *chan);

/**
 * \brief gets the maximum transfer size of the channel
 *
 * \param DMA channel
 *
 * \returns maximum transfer size in bytes
 */
uint32_t dma_channel_get_max_xfer_size(struct dma_channel *chan);


/*
 * ----------------------------------------------------------------------------
 * Interrupt management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief enables the interrupts for the DMA channel
 *
 * \param dev     IOAT DMA channel
 * \param type    interrupt type
 * \param handler interrupt handler function
 * \param arg     argument supplied to the handler function
 */
errval_t dma_channel_intr_enable(struct dma_channel *chan,
                                dma_irq_t type,
                                dma_irq_fn_t handler,
                                void *arg);

/**
 * \brief disables the interrupts for the DMA channel
 *
 * \param chan  IOAT DMA channel
 */
void dma_channel_intr_disable(struct dma_channel *chan);

/**
 * \brief sets the interrupt delay for the DMA channel
 *
 * \param chan  IOAT DMA channel
 * \param usec  interrupt delay in microseconds
 */
void dma_channel_set_intr_delay(struct dma_channel *chan,
                                uint16_t usec);

/*
 * ----------------------------------------------------------------------------
 * Helper Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief generates the DMA channel ID from the device and the channel index
 *
 * \param dev   device ID
 * \param idx   channel index
 *
 * \returns DMA channel ID
 */
static inline dma_chan_id_t dma_channel_id_build(dma_dev_id_t dev,
                                                 uint8_t idx)
{
    return ((((uint16_t) dev) << 8) | idx);
}

/**
 * \brief extracts the DMA device ID from the channel id
 *
 * \param id    DMA channel ID
 *
 * \returns DMA device ID
 */
static inline dma_dev_id_t dma_channel_id_get_device_id(dma_chan_id_t id)
{
    return (dma_dev_id_t) (0xFF & (id >> 8));
}

#endif  /* LIB_DMA_CHANNEL_H */
