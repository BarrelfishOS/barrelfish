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

#define IOAT_DMA_CHANNEL_COMPL_SIZE 4096
#define IOAT_DMA_CHANNEL_COMPL_FLAGS VREGION_FLAGS_READ_WRITE

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


/**
 * \brief returns a channel to be used form the give device based on the channel
 *        index
 *
 * \param dev IOAT DMA device
 * \param idx channel index
 *
 * \returns IOAT DMA Channel
 *          NULL if the index exceeds the number of channels
 */
struct ioat_dma_channel *ioat_dma_channel_get_by_idx(struct ioat_dma_device *dev,
                                                     uint8_t idx);

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

errval_t ioat_dma_channel_start(struct ioat_dma_channel *chan);

errval_t ioat_dma_channel_restart(struct ioat_dma_channel *chan);

uint16_t ioat_dma_channel_submit_pending(struct ioat_dma_channel *chan);

bool ioat_dma_channel_is_active(struct ioat_dma_channel *chan);

bool ioat_dma_channel_is_idle(struct ioat_dma_channel *chan);

bool ioat_dma_channel_is_halted(struct ioat_dma_channel *chan);

bool ioat_dma_channel_is_suspended(struct ioat_dma_channel *chan);

struct ioat_dma_ring *ioat_dma_channel_get_ring(struct ioat_dma_channel *chan);

uint32_t ioat_dma_channel_get_max_xfer_size(struct ioat_dma_channel *chan);


void ioat_dma_channel_enq_request(struct ioat_dma_channel *chan,
                                  struct ioat_dma_request *req);


#endif /* IOAT_DMA_CHANNEL_H */
