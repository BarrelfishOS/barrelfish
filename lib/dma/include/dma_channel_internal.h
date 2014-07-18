/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DMA_CHANNEL_INTERNAL_H
#define DMA_CHANNEL_INTERNAL_H

#include <dma/dma_channel.h>

/**
 * Represents the generic part of a DMA channel
 */
struct dma_channel
{
    dma_chan_id_t id;               ///< unique DMA channel id
    dma_chan_st_t state;            ///< channel state
    struct dma_device *device;      ///< DMA device this channel belongs to
    uint32_t max_xfer_size;         ///< maximum number of bytes per transfer

    struct dma_mem mmio;

    struct {
        dma_irq_t type;
        dma_irq_fn_t fn;
        void *arg;
    } irq;                          ///< Device level interrupt

    struct {
        uint32_t count;             ///< number of requests in the list
        struct dma_request *head;   ///< start of the request list
        struct dma_request *tail;   ///< end of the request list
    } req_list;                     ///< list of submitted requests
};



#endif /* DMA_CHANNEL_INTERNAL_H */
