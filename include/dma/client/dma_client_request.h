/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_CLIENT_REQUEST_H
#define LIB_DMA_CLIENT_REQUEST_H

#include <dma/dma_request.h>

struct dma_client_device;
struct dma_client_channel;
struct dma_client_request;

/**
 * \brief pointer type conversion
 */
static inline struct dma_client_request *dma_request_to_ioat(struct dma_request *req)
{
    return (struct dma_client_request *)req;
}

/*
 * ----------------------------------------------------------------------------
 * DMA Memory Registration / De-Registration
 * ----------------------------------------------------------------------------
 */

/**
 * \brief registers a memory region with a specified client connection
 *
 * \param chan  DMA client channel
 * \param frame the memory frame to register
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_register_memory_chan(struct dma_channel *chan,
                                         struct capref frame);

/**
 * \brief registers a memory region with a specified client connection
 *
 * \param dev   DMA client device
 * \param frame the memory frame to register
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_register_memory(struct dma_device *dev,
                                    struct capref frame);

/**
 * \brief deregisters a previously registered memory region from the connection
 *
 * \param chan  DMA client channel
 * \param frame the memory frame to deregister
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_deregister_memory_chan(struct dma_channel *chan,
                                           struct capref frame);

/**
 * \brief deregisters a previously registered memory region from the connection
 *
 * \param dev   DMA client device
 * \param frame the memory frame to deregister
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_deregister_memory(struct dma_device *dev,
                                      struct capref frame);

/*
 * ----------------------------------------------------------------------------
 * Request Execution
 * ----------------------------------------------------------------------------
 */

/**
 * \brief issues a memcopy request to the service using the connection
 *
 * \param chan  DMA client channel
 * \param setup DMA request setup information
 * \param id    Returns the assigned DMA request ID
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_request_memcpy_chan(struct dma_channel *chan,
                                        struct dma_req_setup *setup,
                                        dma_req_id_t *id);

/**
 * \brief issues a memcopy request to the service using the connection
 *
 * \param dev   DMA client device
 * \param setup DMA request setup information
 * \param id    Returns the assigned DMA request ID
 *
 * \returns SYS_ERR_OK on success
 */
errval_t dma_client_request_memcpy(struct dma_device *dev,
                                   struct dma_req_setup *setup,
                                   dma_req_id_t *id);


/*
 * ----------------------------------------------------------------------------
 * Getters / Setters
 * ----------------------------------------------------------------------------
 */



#endif  /* LIB_DMA_CLIENT_REQUEST_H */
