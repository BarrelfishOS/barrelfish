/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DMA_DESC_RING_H
#define IOAT_DMA_DESC_RING_H

//#include <dev/ioat_dma_dev.h>

struct ioat_dma_ring;
struct ioat_dma_descriptor;


#define IOAT_DMA_DESC_MAP_FLAGS VREGION_FLAGS_READ_WRITE

/// the minimum amount of DMA descriptors to allocate in bits
#define IOAT_DMA_DESC_RING_SIZE 8

/// maximum ring size in bits
#define IOAT_DMA_DESC_RING_SIZE_MAX 16

/// the size of the basic descriptor
#define IOAT_DMA_DESC_SIZE 64

/// minimum alignment constraint for the descriptors
#define IOAT_DMA_DESC_ALIGN 64


/*
 * ----------------------------------------------------------------------------
 * Descriptor Ring Allocation / free
 * ----------------------------------------------------------------------------
 */


/**
 * \brief allocates a descriptor ring
 *
 * \param size_bits the size of the ring in bits
 * \param ret_ring  where the ring pointer is returned
 *
 * \returns SYS_ERR_OK on succes
 *          errval on error
 */
errval_t ioat_dma_ring_alloc(uint8_t size_bits,
                             struct ioat_dma_ring **ret_ring,
                             struct ioat_dma_channel *chan);

errval_t ioat_dma_ring_free(struct ioat_dma_ring *ring);

/*
 * ----------------------------------------------------------------------------
 * Ring Status Queries
 * ----------------------------------------------------------------------------
 */


uint16_t ioat_dma_ring_get_size(struct ioat_dma_ring *ring);

uint16_t ioat_dma_ring_get_active(struct ioat_dma_ring *ring);

uint16_t ioat_dma_ring_get_pendig(struct ioat_dma_ring *ring);

static inline uint16_t ioat_dma_ring_get_space(struct ioat_dma_ring *ring)
{
    return ioat_dma_ring_get_size(ring) - ioat_dma_ring_get_active(ring);
}


/*
 * ----------------------------------------------------------------------------
 * Ring Manipulation
 * ----------------------------------------------------------------------------
 */

/**
 * \brief gets the next descriptor based on the head pointer
 *
 * \param ring the DMA ring
 *
 * \returns pointer to a DMA descriptor
 */
struct ioat_dma_descriptor *ioat_dma_ring_get_next_desc(struct ioat_dma_ring *ring);

/**
 * \brief gets the next descriptor based on the index
 *
 * \param ring  the DMA ring
 * \param index the index of the ring
 *
 * \returns pointer to a DMA descriptor
 */
struct ioat_dma_descriptor *ioat_dma_ring_get_desc(struct ioat_dma_ring *ring,
                                                   uint16_t index);

/**
 * \brief submits the pending descriptors to the hardware
 *
 * \param ring DMA ring to submit the pending descriptors
 *
 * \returns the current head of the descriptors
 */
uint16_t ioat_dma_ring_submit_pending(struct ioat_dma_ring *ring);

/**
 * \brief obtains the physical address of the descriptor chain
 *        (pending descriptors)
 *
 * \param ring the DMA ring
 *
 * \returns physical address of the pending descriptor chain
 */
lpaddr_t ioat_dma_ring_get_chain_addr(struct ioat_dma_ring *ring);

#endif /* IOAT_DMA_DESC_RING_H */
