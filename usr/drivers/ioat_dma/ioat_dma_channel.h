/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DMA_CHANNEL_H
#define IOAT_DMA_CHANNEL_H


struct ioat_dma_channel;
struct ioat_dma_request;

typedef uint16_t ioat_dma_chan_id_t;

/**
 * \brief initializes a new DMA channel and allocates the channel resources
 *
 * \param chan  where to store the channel pointer
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_channel_init(struct ioat_dma_device *dev);

errval_t ioat_dma_channel_reset(struct ioat_dma_channel *chan);

void ioat_dma_chan_free(struct ioat_dma_channel *chan);

errval_t ioat_dma_channel_irq_setup_msix(struct ioat_dma_channel *chan);



/*
 * ---
 */

/**
 * \brief returns a channel to be used form the give device
 *
 * \param dev IOAT DMA device
 *
 * \returns IOAT DMA Channel
 */
struct ioat_dma_channel *ioat_dma_channel_get(struct ioat_dma_device *dev);

ioat_dma_chan_id_t ioat_dma_channel_get_id(struct ioat_dma_channel *chan);

struct ioat_dma_desc_alloc *ioat_dma_channel_get_desc_alloc(struct ioat_dma_channel * chan);

errval_t ioat_dma_channel_poll(struct ioat_dma_channel *chan);

errval_t ioat_dma_channel_submit(struct ioat_dma_channel *chan,
                                 struct ioat_dma_request *req);

#endif /* IOAT_DMA_CHANNEL_H */
