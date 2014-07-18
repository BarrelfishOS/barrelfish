/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>

#include <dev/ioat_dma_dev.h>

#include <dma_mem_utils.h>

#include <ioat/ioat_dma_internal.h>
#include <ioat/ioat_dma_descriptors_internal.h>
#include <ioat/ioat_dma_ring_internal.h>

#include <debug.h>

#define ALIGN(val, align) (((val) + (align)-1) & ~((align)-1))

/**
 * represents the IOAT DMA ring with its internal state
 */
struct ioat_dma_ring
{
    uint16_t size;          ///< size of the descriptor ring
    uint16_t head;          ///< allocated index
    uint16_t issued;        ///< hardware notification point
    uint16_t tail;          ///< cleanup index
    uint16_t dmacount;      ///< identical to 'head' except for occasionally resetting to zero
    uint16_t alloc_order;   ///< log2 of the number of allocated descriptors
    uint16_t produce;       ///< number of descriptors to produce at submit time
    struct ioat_dma_descriptor **desc;  ///< descriptor pointer array
    struct ioat_dma_channel *chan;
};

/*
 * ============================================================================
 * Library Internal Interface
 * ============================================================================
 */

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
inline struct ioat_dma_descriptor *ioat_ring_get_next_desc(struct ioat_dma_ring *ring)
{
    struct ioat_dma_descriptor *desc = ioat_dma_ring_get_desc(ring, ring->head++);

    IOATDESC_DEBUG("ring getting next head desc:%p @ [%016lx], new head:%u\n", desc,
                 ioat_desc_get_paddr(desc), ring->head);

    return desc;
}


/**
 * \brief gets the next descriptor based on the tail pointer and increases the
 *        tail pointer index
 *
 * \param ring the DMA ring
 *
 * \returns pointer to a DMA descriptor
 */
inline struct ioat_dma_descriptor *ioat_ring_get_tail_desc(struct ioat_dma_ring *ring)
{
    struct ioat_dma_descriptor *desc = ioat_dma_ring_get_desc(ring, ring->tail++);

    IOATDESC_DEBUG("ring getting tail desc:%p @ [%016lx], new tail: %u\n", desc,
                 ioat_desc_get_paddr(desc), ring->tail);

    return desc;
}

/**
 * \brief submits the pending descriptors to the hardware
 *
 * \param ring DMA ring to submit the pending descriptors
 *
 * \returns the current head of the descriptors
 */
uint16_t ioat_ring_submit_pending(struct ioat_dma_ring *ring)
{
    uint16_t num_pending = ioat_dma_ring_get_pendig(ring);

    if (num_pending != 0) {
        ring->dmacount += num_pending;
        ring->issued = ring->head;
    }

    return ring->dmacount;
}

/**
 * \brief obtains the physical address of the descriptor chain
 *        (pending descriptors)
 *
 * \param ring the DMA ring
 *
 * \returns physical address of the pending descriptor chain
 */
inline lpaddr_t ioat_ring_get_chain_addr(struct ioat_dma_ring *ring)
{
    return ioat_desc_get_paddr(ioat_dma_ring_get_desc(ring, ring->issued));
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

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
                             struct ioat_dma_channel *chan)
{
    errval_t err;

    IOATDESC_DEBUG("allocating descriptor ring of size %u\n", (1 << size_bits));

    struct ioat_dma_ring *ring;

    if (size_bits > IOAT_DESC_RING_SIZE_MAX) {
        size_bits = IOAT_DESC_RING_SIZE_MAX;
    }

    uint16_t ndesc = (1UL << size_bits);

    ring = malloc(sizeof(struct ioat_dma_ring) + ndesc * sizeof(void *));
    if (ring == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    memset(ring, 0, sizeof(struct ioat_dma_ring) + ndesc * sizeof(void *));

    ring->chan = chan;
    ring->size = ndesc;
    ring->desc = (void *) (ring + 1);

    err = ioat_desc_alloc(IOAT_DMA_DESC_SIZE, IOAT_DMA_DESC_ALIGN, ndesc,
                              ring->desc);
    if (err_is_fail(err)) {
        free(ring);
        return err;
    }

    *ret_ring = ring;

    return SYS_ERR_OK;
}

/**
 * \brief frees a previously allocated descriptor ring
 *
 * \param ring IAT DMA descriptor ring to be freed
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_ring_free(struct ioat_dma_ring *ring)
{
    errval_t err;

    IOATDESC_DEBUG("freeing descriptor ring %p\n", ring);

    err = ioat_desc_free(ring->desc[0]);
    if (err_is_fail(err)) {
        return err;
    }

    free(ring);

    return SYS_ERR_OK;
}

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
inline uint16_t ioat_dma_ring_get_active(struct ioat_dma_ring *ring)
{
    return (ring->head - ring->tail) & (ring->size - 1);
}

/**
 * \brief returns the number of prepared but not submitted descriptors
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns number of pending descriptors
 */
inline uint16_t ioat_dma_ring_get_pendig(struct ioat_dma_ring *ring)
{
    return (ring->head - ring->issued) & (ring->size - 1);
}

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
inline uint16_t ioat_dma_ring_get_size(struct ioat_dma_ring *ring)
{
    return ring->size;
}

/**
 * \brief returns the head pointer index of the ring
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns head element index
 */
inline uint16_t ioat_dma_ring_get_head(struct ioat_dma_ring *ring)
{
    return ring->head;
}

/**
 * \brief returns the tail pointer index of the ring
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns tail element index
 */
inline uint16_t ioat_dma_ring_get_tail(struct ioat_dma_ring *ring)
{
    return ring->tail;
}

/**
 * \brief returns the issued pointer index of the ring
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns issued element index
 */
inline uint16_t ioat_dma_ring_get_issued(struct ioat_dma_ring *ring)
{
    return ring->issued;
}

/**
 * \brief gets the next descriptor based on the index
 *
 * \param ring  the DMA ring
 * \param index the index of the ring
 *
 * \returns pointer to a DMA descriptor
 */
inline struct ioat_dma_descriptor *ioat_dma_ring_get_desc(struct ioat_dma_ring *ring,
                                                          uint16_t index)
{
    return ring->desc[index & (ring->size - 1)];
}
