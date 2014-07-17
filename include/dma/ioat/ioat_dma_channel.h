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

typedef uint16_t ioat_dma_chan_id_t;

enum ioat_dma_chan_st
{
    IOAT_DMA_CHAN_ST_INVALID,
    IOAT_DMA_CHAN_ST_RESETTING,
    IOAT_DMA_CHAN_ST_UNINITIALEZED,
    IOAT_DMA_CHAN_ST_PREPARED,
    IOAT_DMA_CHAN_ST_RUNNING,
    IOAT_DMA_CHAN_ST_ERROR,
    IOAT_DMA_CHAN_ST_SUSPENDED,
    IOAT_DMA_CHAN_ST_INIT_FAIL,
};

static inline ioat_dma_chan_id_t ioat_dma_channel_build_id(ioat_dma_devid_t dev,
                                                           uint8_t id)
{
    return ((((uint16_t) dev) << 8) | id);
}

#endif  /* LIB_IOAT_DMA_DEVICE_H */
