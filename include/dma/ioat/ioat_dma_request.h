/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_IOAT_DMA_REQUEST_H
#define LIB_IOAT_DMA_REQUEST_H

struct ioat_dma_channel;
struct ioat_dma_request;



/*
 * ----------------------------------------------------------------------------
 * Request Execution
 * ----------------------------------------------------------------------------
 */

/**
 * \brief issues a memcpy request to the given channel
 *
 * \param chan  IOAT DMA channel
 * \param setup request setup information
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_request_memcpy_chan(struct ioat_dma_channel *chan,
                                      struct dma_req_setup *setup);

/**
 * \brief issues a memcpy request to a channel of the given device
 *
 * \param dev   IOAT DMA device
 * \param setup request setup information
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
static inline errval_t ioat_dma_request_memcpy(struct ioat_dma_device *dev,
                                               struct dma_req_setup *setup)
{
    struct ioat_dma_channel *chan = ioat_dma_device_get_next_channel(dev);
    return ioat_dma_request_memcpy_chan(chan, setup);
}

/**
 * \brief issues a NOP / NULL descriptor request on the given channel
 *
 * \param chan  IOAT DMA channel
 * \param setup request setup information
 */
void ioat_dma_request_nop_chan(struct ioat_dma_channel *chan);

/**
 * \brief issues a NOP / NULL descriptor request on the given device
 *
 * \param dev   IOAT DMA device
 * \param setup request setup information
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
static inline void ioat_dma_request_nop(struct ioat_dma_device *dev)
{
    struct ioat_dma_channel *chan = ioat_dma_device_get_next_channel(dev);
    ioat_dma_request_nop_chan(chan);
}

/*
 * ----------------------------------------------------------------------------
 * Getters / Setters
 * ----------------------------------------------------------------------------
 */



#endif  /* LIB_IOAT_DMA_DEVICE_H */
