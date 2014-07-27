/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_DMA_CHANNEL_INTERNAL_H
#define XEON_PHI_DMA_CHANNEL_INTERNAL_H

#include <dma_channel_internal.h>
#include <dma/xeon_phi/xeon_phi_dma_channel.h>

#define XEON_PHI_DMA_CHANNEL_DSTAT_SIZE 64

#include <dev/xeon_phi/xeon_phi_dma_chan_dev.h>

/**
 * \brief initializes and allocates resources for a new channel DMA channel
 *        belonging to a device
 *
 * \param dev       IOAT DMA device
 * \param id        id of this channel
 * \param max_xfer  maximum size in bytes for a transfer
 * \param ret_chan  returned channel pointer
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_dma_channel_init(struct xeon_phi_dma_device *dev,
                                   uint8_t id,
                                   uint32_t max_xfer,
                                   struct xeon_phi_dma_channel **ret_chan);

/**
 * \brief returns the status writeback address
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns physical address of the dstat WB address
 */
lpaddr_t xeon_phi_dma_channel_get_dstat_wb(struct xeon_phi_dma_channel *chan);

/**
 * \brief enqueues a request onto the IOAT DMA channel and submits it to the
 *        hardware
 *
 * \param chan  IOAT DMA channel
 * \param req   IOAT DMA request to be submitted
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t xeon_phi_dma_channel_submit_request(struct xeon_phi_dma_channel *chan,
                                             struct xeon_phi_dma_request *req);

/**
 * \brief initializes the MSI-X interrupts for the channel
 */
errval_t xeon_phi_dma_channel_irq_setup_msix(struct xeon_phi_dma_channel *chan);

#endif /* XEON_PHI_DMA_CHANNEL_INTERNAL_H */
