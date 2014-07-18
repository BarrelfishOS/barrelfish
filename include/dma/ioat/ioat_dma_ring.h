/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_IOAT_DMA_RING_H
#define LIB_IOAT_DMA_RING_H

struct ioat_dma_ring;

/// the minimum amount of DMA descriptors to allocate in bits
#define IOAT_DMA_DESC_RING_SIZE 8


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

/**
 * \brief frees a previously allocated descriptor ring
 *
 * \param ring IAT DMA descriptor ring to be freed
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_ring_free(struct ioat_dma_ring *ring);

/*
 * ----------------------------------------------------------------------------
 * Getter Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the size of the ring
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns number of descriptors in the ring
 */
uint16_t ioat_dma_ring_get_size(struct ioat_dma_ring *ring);

/**
 * \brief returns the head pointer index of the ring
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns head element index
 */
uint16_t ioat_dma_ring_get_head(struct ioat_dma_ring *ring);

/**
 * \brief returns the tail pointer index of the ring
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns tail element index
 */
uint16_t ioat_dma_ring_get_tail(struct ioat_dma_ring *ring);

/**
 * \brief returns the issued pointer index of the ring
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns issued element index
 */
uint16_t ioat_dma_ring_get_issued(struct ioat_dma_ring *ring);

/**
 * \brief returns the DMA count of the ring for setting the DMA count register
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns dmacount value
 */
uint16_t ioat_dma_ring_get_dmacount(struct ioat_dma_ring *ring);

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

/*
 * ----------------------------------------------------------------------------
 * Ring Status Queries
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the number of active descriptors i.e. those who have not yet
 *        been finished by the DMA hardware. this includes submitted and pending
 *        DMA descriptors
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns number of active descriptors
 */
uint16_t ioat_dma_ring_get_active(struct ioat_dma_ring *ring);


/**
 * \brief returns the number of prepared but not submitted descriptors
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns number of pending descriptors
 */
uint16_t ioat_dma_ring_get_pendig(struct ioat_dma_ring *ring);


/**
 * \brief calculates the free space of the ring
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns number of free descriptors
 */
static inline uint16_t ioat_dma_ring_get_space(struct ioat_dma_ring *ring)
{
    return ioat_dma_ring_get_size(ring) - ioat_dma_ring_get_active(ring);
}



#endif  /* LIB_IOAT_DMA_RING_H */
