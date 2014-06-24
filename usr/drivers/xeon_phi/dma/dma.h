/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef XEON_PHI_DMA_H
#define XEON_PHI_DMA_H

/// the base name of the exported dma service
#define XEON_PHI_DMA_SERVICE_NAME "xeon_phi_dma_svc"

/// alignment for size and memory addresses
#define XEON_PHI_DMA_ALIGNMENT 64

///
#define XEON_PHI_DMA_ALIGN_SHIFT 6

/// the maximum number of DMA channels on the card
#define XEON_PHI_DMA_CHAN_NUM_MAX 8

/// the number of DMA channels for the host
#define XEON_PHI_DMA_CHAN_NUM_HOST 4

#ifdef __k1om__
#define XEON_PHI_DMA_CHAN_NUM (XEON_PHI_DMA_CHAN_NUM_MAX- XEON_PHI_DMA_CHAN_NUM_HOST)
#define XEON_PHI_DMA_CHAN_OFFSET XEON_PHI_DMA_CHAN_NUM_HOST
#else
#define XEON_PHI_DMA_CHAN_NUM XEON_PHI_DMA_CHAN_NUM_HOST
#define XEON_PHI_DMA_CHAN_OFFSET 0
#endif
#define XEON_PHI_DMA_OWNER_HOST         1
#define XEON_PHI_DMA_OWNER_CARD         2




/// alignment constraint of the descriptor ring (cache line)
#define XEON_PHI_DMA_DESC_RING_ALIGN 64


/**
 * \brief Initializes the DMA structure for the Xeon Phi
 *
 * \param phi the xeon phi DMA structure
 *
 * \return SYS_ERR_OK on success,
 */
errval_t dma_init(struct xeon_phi *phi);

#endif /* XEON_PHI_DMA_H */
