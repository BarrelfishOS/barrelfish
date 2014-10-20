/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_DMA_REQUEST_INTERNAL_H
#define XEON_PHI_DMA_REQUEST_INTERNAL_H

#include <dma_request_internal.h>
#include <dma/xeon_phi/xeon_phi_dma_request.h>


/**
 * \brief handles the processing of completed DMA requests
 *
 * \param req   the DMA request to process
 *
 * \returns SYS_ERR_OK on sucess
 *          errval on failure
 */
errval_t xeon_phi_dma_request_process(struct xeon_phi_dma_request *req);

#endif /* XEON_PHI_DMA_REQUEST_INTERNAL_H */
