/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DMA_RING_INTERNAL_H
#define IOAT_DMA_RING_INTERNAL_H

#include <dma/ioat/ioat_dma_ring.h>

/// maximum ring size in bits
#define IOAT_DESC_RING_SIZE_MAX 16

/*
 * ----------------------------------------------------------------------------
 * Ring Manipulation
 * ----------------------------------------------------------------------------
 */

/**
 * \brief gets the next descriptor based on the head pointer and increments the
 *        head pointer
 *
 * \param ring the DMA ring
 *
 * \returns pointer to a DMA descriptor
 */
struct ioat_dma_descriptor *ioat_ring_get_next_desc(struct ioat_dma_ring *ring);


/**
 * \brief gets the next descriptor based on the tail pointer and increases the
 *        tail pointer index
 *
 * \param ring the DMA ring
 *
 * \returns pointer to a DMA descriptor
 */
struct ioat_dma_descriptor *ioat_ring_get_tail_desc(struct ioat_dma_ring *ring);

/**
 * \brief submits the pending descriptors to the hardware
 *
 * \param ring DMA ring to submit the pending descriptors
 *
 * \returns the current head of the descriptors
 */
uint16_t ioat_ring_submit_pending(struct ioat_dma_ring *ring);

/**
 * \brief obtains the physical address of the descriptor chain
 *        (pending descriptors)
 *
 * \param ring the DMA ring
 *
 * \returns physical address of the pending descriptor chain
 */
lpaddr_t ioat_ring_get_chain_addr(struct ioat_dma_ring *ring);

#endif /* IOAT_DMA_RING_INTERNAL_H */
