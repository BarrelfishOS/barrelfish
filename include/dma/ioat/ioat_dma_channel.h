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

/**
 * \brief Resets a IOAT DMA channel
 *
 * \param chan  IOAT DMA channel to be reset
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_CHAN_RESET on reset timeout
 */
errval_t ioat_dma_channel_reset(struct ioat_dma_channel *chan);

/**
 * \brief Starts a channel and sets it into the running state
 *
 * \param chan IOAT DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_channel_start(struct ioat_dma_channel *chan);

/**
 * \brief Restarts a channel and sets it into the running state
 *        this is to be used upon error condition
 *
 * \param chan IOAT DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_channel_restart(struct ioat_dma_channel *chan);


/**
 * \brief Submits the pending descriptors to the hardware queue
 *
 * \param chan  IOAT DMA channel
 *
 * \returns number of submitted descriptors
 */
uint16_t ioat_dma_channel_submit_pending(struct ioat_dma_channel *chan);

/**
 * \brief polls the IOAT DMA channel for completed events
 *
 * \param chan  IOAT DMA channel
 *
 * \returns SYS_ERR_OK if there was something processed
 *
 */
errval_t ioat_dma_channel_poll(struct ioat_dma_channel *chan);

/*
 * ----------------------------------------------------------------------------
 * Channel Status
 * ----------------------------------------------------------------------------
 */

bool ioat_dma_channel_is_active(struct ioat_dma_channel *chan);

/**
 * \brief reads the CHANSTS register and and checks if the channel is idle
 *
 * \param chan IOAT DMA channel
 *
 * \returns true if channel is idle
 *          false if not
 */
bool ioat_dma_channel_is_idle(struct ioat_dma_channel *chan);

/**
 * \brief reads the CHANSTS register and and checks if the channel is halted
 *        i.e. if there was an error condition
 *
 * \param chan IOAT DMA channel
 *
 * \returns true if channel is halted (there was an error)
 *          false if not
 */
bool ioat_dma_channel_is_halted(struct ioat_dma_channel *chan);

/**
 * \brief reads the CHANSTS register and and checks if the channel is suspended
 *
 * \param chan IOAT DMA channel
 *
 * \returns true if channel is suspended
 *          false if not
 */
bool ioat_dma_channel_is_suspended(struct ioat_dma_channel *chan);



/**
 * \brief returns the associated IOAT DMA descriptor ring of a channel
 *
 * \param chan  IOAT DMA channel
 *
 * \returns IOAT DMA descriptor ring handle
 */
struct ioat_dma_ring *ioat_dma_channel_get_ring(struct ioat_dma_channel *chan);


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
