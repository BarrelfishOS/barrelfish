/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_CLIENT_CHANNEL_H
#define LIB_DMA_CLIENT_CHANNEL_H



/**
 * \brief pointer type conversion
 */
static inline struct dma_client_channel *dma_channel_to_client(struct dma_channel *chan)
{
    return (struct dma_client_channel *)chan;
}

#endif  /* LIB_DMA_CLIENT_CHANNEL_H */
