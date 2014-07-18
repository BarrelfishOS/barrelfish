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

/// DMA channel id
typedef uint16_t dma_chan_id_t;

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
 * \brief generates the DMA channel ID from the device and the channel index
 *
 * \param dev   device ID
 * \param idx   channel index
 *
 * \returns DMA channel ID
 */
static inline ioat_dma_chan_id_t dma_channel_build_id(ioat_dma_devid_t dev,
                                                      uint8_t idx)
{
    return ((((uint16_t) dev) << 8) | idx);
}

#endif  /* LIB_DMA_CHANNEL_H */
