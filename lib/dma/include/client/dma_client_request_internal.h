/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DMA_CLIENT_REQUEST_INTERNAL_H
#define DMA_CLIENT_REQUEST_INTERNAL_H

#include <dma_request_internal.h>
#include <dma/client/dma_client_request.h>

struct dma_client_request
{
    struct dma_request common;
    struct capref cap;
};

/**
 * \brief frees up a DMA request (called by the rx_done() function)
 *
 * \param req DMA request to be freed
 */
void dma_client_request_free(struct dma_client_request *req);

#endif /* DMA_CLIENT_REQUEST_INTERNAL_H */
