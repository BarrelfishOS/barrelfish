/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_IOAT_DMA_REQUEST_H
#define LIB_IOAT_DMA_REQUEST_H

#include <dma/dma_request.h>

struct ioat_dma_device;
struct ioat_dma_channel;
struct ioat_dma_request;

/**
 * \brief pointer type conversion
 */
static inline struct ioat_dma_request *dma_request_to_ioat(struct dma_request *req)
{
    return (struct ioat_dma_request *)req;
}

/*
 * ----------------------------------------------------------------------------
 * Request Execution
 * ----------------------------------------------------------------------------
 */

/**
 * \brief issues a memcpy request to the given channel
 *
 * \param chan  IOAT DMA channel
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_request_memcpy_chan(struct dma_channel *chan,
                                      struct dma_req_setup *setup,
                                      dma_req_id_t *id);

/**
 * \brief issues a memcpy request to a channel of the given device
 *
 * \param dev   IOAT DMA device
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_request_memcpy(struct dma_device *dev,
                                 struct dma_req_setup *setup,
                                 dma_req_id_t *id);

/**
 * \brief issues a memcpy request to the given channel
 *
 * \param chan  IOAT DMA channel
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_request_memset_chan(struct dma_channel *chan,
                                      struct dma_req_setup *setup,
                                      dma_req_id_t *id);
/**
 * \brief issues a memset request to a channel of the given device
 *
 * \param dev   IOAT DMA device
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_request_memset(struct dma_device *dev,
                                 struct dma_req_setup *setup,
                                 dma_req_id_t *id);
/**
 * \brief issues a NOP / NULL descriptor request on the given channel
 *
 * \param chan  IOAT DMA channel
 * \param setup request setup information
 */
void ioat_dma_request_nop_chan(struct ioat_dma_channel *chan);

/**
 * \brief issues a NOP / NULL descriptor request on the given device
 *
 * \param dev   IOAT DMA device
 * \param setup request setup information
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
void ioat_dma_request_nop(struct ioat_dma_device *dev);

/*
 * ----------------------------------------------------------------------------
 * Getters / Setters
 * ----------------------------------------------------------------------------
 */



#endif  /* LIB_IOAT_DMA_DEVICE_H */
