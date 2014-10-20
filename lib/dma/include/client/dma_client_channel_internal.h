/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DMA_CHANNEL_INTERNAL_H
#define IOAT_DMA_CHANNEL_INTERNAL_H

#include <dma_channel_internal.h>
#include <dma/client/dma_client_channel.h>

/**
 * \brief initializes and allocates resources for a new channel DMA channel
 *        belonging to a device
 *
 * \param dev           DMA client device
 * \param id            id of this channel
 * \param driver_iref   iref to the driver service
 * \param ret_chan      returned channel pointer
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_channel_init(struct dma_client_device *dev,
                                 uint8_t id,
                                 iref_t driver_iref,
                                 struct dma_client_channel **ret_chan);

/**
 * \brief enqueues a request onto the DMA client channel and sends it to the
 *        DMA driver service
 *
 * \param chan  DMA client channel
 * \param req   DMA client request
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t dma_client_channel_submit_request(struct dma_channel *chan,
                                           struct dma_request *req);


#endif /* IOAT_DMA_CHANNEL_INTERNAL_H */
