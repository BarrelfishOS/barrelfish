/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_IOAT_DMA_CHANNEL_H
#define LIB_IOAT_DMA_CHANNEL_H

struct ioat_dma_channel;
struct ioat_dma_request;





/**
 * \brief Resets a IOAT DMA channel
 *
 * \param chan  IOAT DMA channel to be reset
 *
 * \returns SYS_ERR_OK on success
 *          IOAT_ERR_CHAN_RESET on reset timeout
 */
errval_t ioat_dma_channel_reset(struct ioat_dma_channel *chan);


/*
 * ----------------------------------------------------------------------------
 * Getter / Setter Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the IOAT DMA channel ID
 *
 * \param chan  IOAT DMA channel
 *
 * \returns IOAT DMA channel ID of the supplied channel
 */
ioat_dma_chan_id_t ioat_dma_channel_get_id(struct ioat_dma_channel *chan);

/**
 * \brief returns the associated IOAT DMA descriptor ring of a channel
 *
 * \param chan  IOAT DMA channel
 *
 * \returns IOAT DMA descriptor ring handle
 */
struct ioat_dma_ring *ioat_dma_channel_get_ring(struct ioat_dma_channel *chan);


/**
 * \brief returns the maximum number of bytes per DMA descritpor
 *
 * \param chan IOAT DMA channel
 *
 * \returns maximum number of bytes
 */
uint32_t ioat_dma_channel_get_max_xfer_size(struct ioat_dma_channel *chan);

/*
 * ----------------------------------------------------------------------------
 * Request Management
 * ----------------------------------------------------------------------------
 */

/**
 *
 */
void ioat_dma_channel_request_enqueue(struct ioat_dma_channel *chan,
                                      struct ioat_dma_request *req);


#endif  /* LIB_IOAT_DMA_DEVICE_H */
