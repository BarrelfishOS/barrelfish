/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_DMA_SERVICE_H_
#define XEON_PHI_DMA_SERVICE_H_

#include <dma/dma.h>
#include <dma/dma_device.h>

/**
 * \brief initializes the Xeon Phi DMA devices and the service
 *
 * \param phi   Xeon Phi handle
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xdma_service_init(struct xeon_phi *phi);

/**
 *
 */
static inline errval_t xdma_service_poll(struct xeon_phi *phi)
{
    if (phi->dma) {
        return dma_device_poll_channels(phi->dma);
    }
    return DMA_ERR_DEVICE_IDLE;
}

#endif /* XEON_PHI_DMA_SERVICE_H_ */
