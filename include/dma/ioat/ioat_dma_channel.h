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

#include <dev/ioat_dma_chan_dev.h>


/**
 * \brief pointer type conversion
 */
static inline struct ioat_dma_channel *dma_channel_to_ioat(struct dma_channel *chan)
{
    return (struct ioat_dma_channel *)chan;
}

/*
 * ----------------------------------------------------------------------------
 * Channel State Management
 * ----------------------------------------------------------------------------
 */

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
 * \brief restarts a IOAT DMA channel this updates the chain address register
 *        and the DMA count register.
 *
 * \param chan  IOAT DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t ioat_dma_channel_restart(struct ioat_dma_channel *chan);

/**
 * \brief starts a IOAT DMA channel. This sets the chain address to the first
 *        entry of the ring and the DMA count to zero.
 *
 * \param chan  IOAT DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t ioat_dma_channel_start(struct ioat_dma_channel *chan);

/**
 * \brief stopps the processing of the descriptors.
 *
 * \param chan  IOAT DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t ioat_dma_channel_stop(struct ioat_dma_channel *chan);

/**
 * \brief Puts the IOAT DMA channel into the suspended state
 *
 * \param chan  IOAT DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t ioat_dma_channel_suspend(struct ioat_dma_channel *chan);


/**
 * \brief Submits the pending descriptors to the hardware queue
 *
 * \param chan  IOAT DMA channel
 *
 * \returns number of submitted descriptors
 */
uint16_t ioat_dma_channel_issue_pending(struct ioat_dma_channel *chan);

/**
 * \brief polls the IOAT DMA channel for completed events
 *
 * \param chan  IOAT DMA channel
 *
 * \returns SYS_ERR_OK if there was something processed
 *
 */
errval_t ioat_dma_channel_poll(struct dma_channel *chan);


/*
 * ----------------------------------------------------------------------------
 * Getter / Setter Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the associated IOAT DMA descriptor ring of a channel
 *
 * \param chan  IOAT DMA channel
 *
 * \returns IOAT DMA descriptor ring handle
 */
struct dma_ring *ioat_dma_channel_get_ring(struct ioat_dma_channel *chan);

/**
 * \brief updates the channel status flag by reading the CHANSTS register
 *
 * \param chan IOAT DMA channel
 */
uint64_t ioat_dma_channel_get_status(struct ioat_dma_channel *chan);

/*
 * ----------------------------------------------------------------------------
 * Channel Status
 * ----------------------------------------------------------------------------
 */

/**
 * \brief reads the CHANSTS register and and checks if the channel is active
 *
 * \param chan IOAT DMA channel
 *
 * \returns true if channel is active
 *          false if not
 */
static inline bool ioat_dma_channel_is_active(uint64_t status)
{
    uint32_t tr_st = ioat_dma_chan_sts_lo_dma_trans_state_extract(status);
    return tr_st == ioat_dma_chan_trans_state_active;
}

/**
 * \brief reads the CHANSTS register and and checks if the channel is idle
 *
 * \param chan IOAT DMA channel
 *
 * \returns true if channel is idle
 *          false if not
 */
static inline bool ioat_dma_channel_is_idle(uint64_t status)
{
    uint32_t tr_st = ioat_dma_chan_sts_lo_dma_trans_state_extract(status);
    return tr_st == ioat_dma_chan_trans_state_idle;
}

/**
 * \brief reads the CHANSTS register and and checks if the channel is halted
 *        i.e. if there was an error condition
 *
 * \param chan IOAT DMA channel
 *
 * \returns true if channel is halted (there was an error)
 *          false if not
 */
static inline bool ioat_dma_channel_is_halted(uint64_t status)
{
    uint32_t tr_st = ioat_dma_chan_sts_lo_dma_trans_state_extract(status);
    return tr_st == ioat_dma_chan_trans_state_halt;
}

/**
 * \brief reads the CHANSTS register and and checks if the channel is suspended
 *
 * \param chan IOAT DMA channel
 *
 * \returns true if channel is suspended
 *          false if not
 */
static inline bool ioat_dma_channel_is_suspended(uint64_t status)
{
    uint32_t tr_st = ioat_dma_chan_sts_lo_dma_trans_state_extract(status);
    return tr_st == ioat_dma_chan_trans_state_susp;
}

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
