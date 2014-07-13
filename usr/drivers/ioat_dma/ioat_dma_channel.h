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


struct ioat_dma_channel
{

};

/**
 * \brief initializes a new DMA channel and allocates the channel resources
 *
 * \param chan  where to store the channel pointer
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_chan_init(struct ioat_dma_chan **chan);

void ioat_dma_chan_free(struct ioat_dma_chan *chan);


#endif /* IOAT_DMA_CHANNEL_H */
