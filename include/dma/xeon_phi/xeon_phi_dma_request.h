/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_XEON_PHI_DMA_REQUEST_H
#define LIB_XEON_PHI_DMA_REQUEST_H

#include <dma/dma_request.h>

/**
 * \brief pointer type conversion
 */
static inline struct xeon_phi_dma_request *dma_request_to_xphi(struct dma_request *req)
{
    return (struct xeon_phi_dma_request *) req;
}

/*
 * ----------------------------------------------------------------------------
 * Request Execution
 * ----------------------------------------------------------------------------
 */

/**
 * \brief issues a memcpy request to the given channel
 *
 * \param chan  Xeon Phi DMA channel
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_dma_request_memcpy_chan(struct dma_channel *chan,
                                          struct dma_req_setup *setup,
                                          dma_req_id_t *id);

/**
 * \brief issues a memcpy request to a channel of the given device
 *
 * \param dev   Xeon Phi DMA device
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_dma_request_memcpy(struct dma_device *dev,
                                     struct dma_req_setup *setup,
                                     dma_req_id_t *id);

/**
 * \brief issues a status request to the given channel
 *
 * \param chan  Xeon Phi DMA channel
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_dma_request_status_chan(struct dma_channel *chan,
                                          struct dma_req_setup *setup,
                                          dma_req_id_t *id);

/**
 * \brief issues a status request to a channel of the given device
 *
 * \param dev   Xeon Phi DMA device
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_request_status(struct dma_device *dev,
                                 struct dma_req_setup *setup,
                                 dma_req_id_t *id);

/**
 * \brief issues a general request to the given channel
 *
 * \param chan  Xeon Phi DMA channel
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_dma_request_general_chan(struct dma_channel *chan,
                                           struct dma_req_setup *setup,
                                           dma_req_id_t *id);

/**
 * \brief issues a general request to a channel of the given device
 *
 * \param dev   Xeon Phi DMA device
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_request_general(struct dma_device *dev,
                                  struct dma_req_setup *setup,
                                  dma_req_id_t *id);

/**
 * \brief issues a keynoncecent request to the given channel
 *
 * \param chan  Xeon Phi DMA channel
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_dma_request_keynoncecent_chan(struct dma_channel *chan,
                                                struct dma_req_setup *setup,
                                                dma_req_id_t *id);

/**
 * \brief issues a keynoncecent request to a channel of the given device
 *
 * \param dev   Xeon Phi DMA device
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_request_keynoncecent(struct dma_device *dev,
                                       struct dma_req_setup *setup,
                                       dma_req_id_t *id);

/**
 * \brief issues a key request to the given channel
 *
 * \param chan  Xeon Phi DMA channel
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_dma_request_keynoncecent(struct dma_channel *chan,
                                           struct dma_req_setup *setup,
                                           dma_req_id_t *id);

/**
 * \brief issues a key request to a channel of the given device
 *
 * \param dev   Xeon Phi DMA device
 * \param setup request setup information
 * \param id    returns the generated request id
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_request_key(struct dma_device *dev,
                              struct dma_req_setup *setup,
                              dma_req_id_t *id);

/**
 * \brief issues a NOP / NULL descriptor request on the given channel
 *
 * \param chan  Xeon Phi DMA channel
 * \param setup request setup information
 */
void xeon_phi_dma_request_nop_chan(struct dma_channel *chan);

/**
 * \brief issues a NOP / NULL descriptor request on the given device
 *
 * \param dev   Xeon Phi DMA device
 * \param setup request setup information
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
void xeon_phi_dma_request_nop(struct dma_device *dev);

/*
 * ----------------------------------------------------------------------------
 * Getters / Setters
 * ----------------------------------------------------------------------------
 */

#endif  /* LIB_XEON_PHI_DMA_REQUEST_H */
