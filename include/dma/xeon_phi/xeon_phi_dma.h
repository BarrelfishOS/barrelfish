/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_XEON_PHI_DMA_H
#define LIB_XEON_PHI_DMA_H

#include <dma/dma.h>

struct xeon_phi_dma_device;
struct xeon_phi_dma_channel;
struct xeon_phi_dma_request;

/// the service name for the exported devices
#define XEON_PHI_DMA_SERVICE_NAME "xeon_phi_dma_svc"

/// size of the Xeon PHI DMA descriptor ring in bits
#define XEON_PHI_DMA_RING_SIZE 8

/**
 * \brief initializes the DMA library
 */
errval_t xeon_phi_dma_init(void);


#endif  /* LIB_IOAT_DMA_H */
