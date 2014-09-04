/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DMA_REQUEST_INTERNAL_H
#define DMA_REQUEST_INTERNAL_H

#include <dma/dma_request.h>

/**
 * generic representation of DMA requests
 */
struct dma_request
{
    dma_req_id_t id;            ///<
    dma_req_st_t state;         ///<
    dma_req_type_t type;        ///<
    errval_t err;

#if DMA_BENCH_ENABLED
    cycles_t timer_start;
#endif

    struct dma_req_setup setup; ///<
    struct dma_request *next;   ///<
    struct dma_request *prev;   ///<
};

dma_req_id_t dma_request_generate_req_id(struct dma_channel *chan);

/**
 * \brief initializes the common part of a DMA request
 *
 * \param req   DMA request to initialize
 * \param chan  Channel this request gets executed on
 * \param type  The type of the DMA request
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t dma_request_common_init(struct dma_request *req,
                                 struct dma_channel *chan,
                                 dma_req_st_t type);

/**
 * \brief handles the processing of completed DMA requests
 *
 * \param req   the DMA request to process
 *
 * \returns SYS_ERR_OK on sucess
 *          errval on failure
 */
errval_t dma_request_process(struct dma_request *req);

/**
 * \brief sets the next DMA request of the given request
 *
 * \param req   DMA request
 * \param next  DMA request
 */
void dma_request_set_next(struct dma_request *req,
                          struct dma_request *next);

#endif /* DMA_REQUEST_INTERNAL_H */
