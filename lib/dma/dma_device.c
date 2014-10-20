/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <dma_internal.h>
#include <dma_device_internal.h>

/*
 * ===========================================================================
 * Public Interface
 * ===========================================================================
 */

/**
 * \brief polls the channels of the IOAT DMA device
 *
 * \param dev   IOAT DMA device
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_DEVICE_IDLE if there is nothing completed on the channels
 *          errval on error
 */
errval_t dma_device_poll_channels(struct dma_device *dev)
{
    if (dev->f.poll) {
        return dev->f.poll(dev);
    }

    return DMA_ERR_DEVICE_UNSUPPORTED;
}

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
inline dma_dev_id_t dma_device_get_id(struct dma_device *dev)
{
    return dev->id;
}

/**
 * \brief gets the DMA device type of the DMA device
 *
 * \param dev   DMA device
 *
 * \returns DMA device type
 */
inline dma_dev_id_t dma_device_get_type(struct dma_device *dev)
{
    return dev->type;
}

/**
 * \brief gets the state of the DMA device
 *
 * \param dev   DMA device
 *
 * \returns DMA device state
 */
inline dma_dev_st_t dma_device_get_state(struct dma_device *dev)
{
    return dev->state;
}


/**
 * \brief gets the virtual address of the mapped MMIO register space
 *
 * \param dev   DMA device
 *
 * \returns MMIO register vbase
 */
inline void *dma_device_get_mmio_vbase(struct dma_device *dev)
{
    return (void *)dev->mmio.vaddr;
}


/**
 * \brief gets the number of channels this device has
 *
 * \param dev   DMA device
 *
 * \returns DMA channel count
 */
inline uint8_t dma_device_get_channel_count(struct dma_device *dev)
{
    return dev->channels.count;
}


/**
 * \brief obtains the channel associated with the given index
 *
 * \param dev   DMA device
 *
 * \returns DMA channel if index exists
 *          NULL if index exceeds channel count
 */

struct dma_channel *dma_device_get_channe_by_idx(struct dma_device *dev,
                                                 uint8_t idx)
{
    if (idx < dev->channels.count) {
        return dev->channels.c[idx];
    }
    return NULL;
}

/**
 * \brief gets a DMA channel from this device in a round robin fashion
 *
 * \param dev   DMA device
 *
 * \returns DMA channel
 */
inline struct dma_channel *dma_device_get_channel(struct dma_device *dev)
{
    if (dev->channels.next >= dev->channels.count) {
        dev->channels.next = 0;
    }
    return dev->channels.c[dev->channels.next++];
}

/*
 * ----------------------------------------------------------------------------
 * Interrupt Management
 * ----------------------------------------------------------------------------
 */

