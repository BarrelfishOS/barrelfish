/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_XEON_PHI_DMA_CHANNEL_H
#define LIB_XEON_PHI_DMA_CHANNEL_H

#include <dma/dma_channel.h>


#define XEON_PHI_DMA_CHANNEL_ENABLE  1
#define XEON_PHI_DMA_CHANNEL_DISABLE 0

/**
 * \brief pointer type conversion
 */
static inline struct xeon_phi_dma_channel *dma_channel_to_xeon_phi(struct dma_channel *chan)
{
    return (struct xeon_phi_dma_channel *)chan;
}

/*
 * ----------------------------------------------------------------------------
 * Channel State Management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief Resets a Xeon Phi DMA channel
 *
 * \param chan  Xeon Phi DMA channel to be reset
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_CHAN_RESET on reset timeout
 */
errval_t xeon_phi_dma_channel_reset(struct xeon_phi_dma_channel *chan);

/**
 * \brief restarts a Xeon Phi DMA channel this updates the chain address register
 *        and the DMA count register.
 *
 * \param chan  Xeon Phi DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t xeon_phi_dma_channel_restart(struct xeon_phi_dma_channel *chan);

/**
 * \brief starts a Xeon Phi DMA channel. This sets the chain address to the first
 *        entry of the ring and the DMA count to zero.
 *
 * \param chan  Xeon Phi DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t xeon_phi_dma_channel_start(struct xeon_phi_dma_channel *chan);

/**
 * \brief stopps the processing of the descriptors.
 *
 * \param chan  Xeon Phi DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t xeon_phi_dma_channel_stop(struct xeon_phi_dma_channel *chan);

/**
 * \brief Puts the Xeon Phi DMA channel into the suspended state
 *
 * \param chan  Xeon Phi DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t xeon_phi_dma_channel_suspend(struct xeon_phi_dma_channel *chan);

/**
 * \brief Submits the pending descriptors to the hardware queue
 *
 * \param chan  Xeon Phi DMA channel
 *
 * \returns number of submitted descriptors
 */
uint16_t xeon_phi_dma_channel_issue_pending(struct xeon_phi_dma_channel *chan);

/**
 * \brief polls the Xeon Phi DMA channel for completed events
 *
 * \param chan  Xeon Phi DMA channel
 *
 * \returns SYS_ERR_OK if there was something processed
 *
 */
errval_t xeon_phi_dma_channel_poll(struct dma_channel *chan);


/*
 * ----------------------------------------------------------------------------
 * Getter / Setter Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the associated Xeon Phi DMA descriptor ring of a channel
 *
 * \param chan  Xeon Phi DMA channel
 *
 * \returns Xeon Phi DMA descriptor ring handle
 */
struct dma_ring *xeon_phi_dma_channel_get_ring(struct xeon_phi_dma_channel *chan);

/*
 * ----------------------------------------------------------------------------
 * Channel Status
 * ----------------------------------------------------------------------------
 */

/**
 * \brief reads the CHANSTS register and and checks if the channel is active
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns true if channel is active
 *          false if not
 */
static inline bool xeon_phi_dma_channel_is_active(uint64_t status)
{
    assert(!"NYI");
    return 0;
}

/**
 * \brief reads the CHANSTS register and and checks if the channel is idle
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns true if channel is idle
 *          false if not
 */
static inline bool xeon_phi_dma_channel_is_idle(uint64_t status)
{
    assert(!"NYI");
    return 0;
}

/**
 * \brief reads the CHANSTS register and and checks if the channel is halted
 *        i.e. if there was an error condition
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns true if channel is halted (there was an error)
 *          false if not
 */
static inline bool xeon_phi_dma_channel_is_halted(uint64_t status)
{
    assert(!"NYI");
    return 0;
}

/**
 * \brief reads the CHANSTS register and and checks if the channel is suspended
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns true if channel is suspended
 *          false if not
 */
static inline bool xeon_phi_dma_channel_is_suspended(uint64_t status)
{
    assert(!"NYI");
    return 0;
}

/*
 * ----------------------------------------------------------------------------
 * Request Management
 * ----------------------------------------------------------------------------
 */

/**
 *
 */
void xeon_phi_dma_channel_request_enqueue(struct dma_channel *chan,
                                          struct dma_request *req);


#endif  /* LIB_XEON_PHI_DMA_CHANNEL_H */
