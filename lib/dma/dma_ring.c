/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <dma_internal.h>
#include <dma_mem_utils.h>

#include <dma_internal.h>
#include <dma_descriptor_internal.h>
#include <dma_ring_internal.h>

#include <debug.h>

#define ALIGN(val, align) (((val) + (align)-1) & ~((align)-1))

/**
 * represents the IOAT DMA ring with its internal state
 */
struct dma_ring
{
    uint16_t size;                 ///< size of the descriptor ring
    uint16_t write_next;           ///< next descriptor to be allocated
    uint16_t issued;               ///< hardware notification point
    uint16_t tail;                 ///< cleanup index
    uint16_t head;                 ///< head of the descriptor chain
    uint8_t use_modulo;           ///< return values module ringsize
    struct dma_descriptor **desc;  ///< descriptor pointer array
    struct dma_channel *chan;      ///< channel associated with this ring
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
inline struct dma_descriptor *dma_ring_get_next_desc(struct dma_ring *ring)
{
    struct dma_descriptor *desc = dma_ring_get_desc(ring, ring->write_next++);

    DMADESC_DEBUG("ring getting next head desc:%p @ [%016lx], new head:%u\n", desc,
                  dma_desc_get_paddr(desc), (ring->write_next & (ring->size - 1)));

    return desc;
}

/**
 * \brief gets the next descriptor based on the tail pointer and increases the
 *        tail pointer index
 *
 * \param ring the DMA ring
 *
 * \returns pointer to a DMA descriptor
 *
 */
inline struct dma_descriptor *dma_ring_get_tail_desc(struct dma_ring *ring)
{
    struct dma_descriptor *desc = dma_ring_get_desc(ring, ring->tail++);

    DMADESC_DEBUG("ring getting tail desc:%p @ [%016lx], new tail: %u\n", desc,
                  dma_desc_get_paddr(desc), (ring->tail & (ring->size - 1)));

    return desc;
}

/**
 * \brief submits the pending descriptors and updates the DMA count value
 *
 * \param ring DMA ring to submit the pending descriptors
 *
 * \returns the current head of the descriptors (dmacount)
 */
uint16_t dma_ring_submit_pending(struct dma_ring *ring)
{
    uint16_t num_pending = dma_ring_get_pendig(ring);

    if (num_pending != 0) {
        ring->head += num_pending;
        ring->issued = ring->write_next;

        DMADESC_DEBUG("ring submit pending head: %u, head = %u\n",
                      (ring->head & (ring->size - 1)),
                      (ring->write_next & (ring->size - 1)));
    }
    if (ring->use_modulo) {
        return (ring->head & (ring->size - 1));
    }
    return ring->head;
}

/**
 * \brief obtains the physical address of the descriptor chain
 *        (pending descriptors)
 *
 * \param ring the DMA ring
 *
 * \returns physical address of the pending descriptor chain
 */
inline lpaddr_t dma_ring_get_chain_addr(struct dma_ring *ring)
{
    return dma_desc_get_paddr(dma_ring_get_desc(ring, ring->issued));
}

/**
 * \brief obtains the physical address of the descriptor ring
 *
 * \param ring the DMA ring
 *
 * \returns physical address of the pending descriptor chain
 */
lpaddr_t dma_ring_get_base_addr(struct dma_ring *ring)
{
    return dma_desc_get_paddr(dma_ring_get_desc(ring, 0));
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
 * \brief allocates a descriptor ring of a given element count and descriptor
 *        sizes
 *
 * \param ndesc_bits number of descriptors for this ring in bits
 * \param desc_align alignment constraints of the descriptors
 * \param desc_size  size of the descriptors in bytes
 * \param use_modulo return the head pointer modulo ring size
 * \param chan       DMA channel of this ring
 * \param ret_ring   where the ring pointer is returned
 *
 * \returns SYS_ERR_OK on succes
 *          errval on error
 */
errval_t dma_ring_alloc(uint8_t ndesc_bits,
                        uint32_t desc_align,
                        uint32_t desc_size,
                        uint8_t use_modulo,
                        struct dma_channel *chan,
                        struct dma_ring **ret_ring)
{
    errval_t err;

    DMADESC_DEBUG("allocating descriptor ring of size %u\n", (1 << ndesc_bits));

    struct dma_ring *ring;

    if (ndesc_bits > DMA_RING_SIZE_MAX) {
        ndesc_bits = DMA_RING_SIZE_MAX;
    }

    uint16_t ndesc = (1UL << ndesc_bits);

    ring = calloc(1, sizeof(struct dma_ring) + ndesc * sizeof(void *));
    if (ring == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    ring->chan = chan;
    ring->size = ndesc;
    ring->desc = (void *) (ring + 1);
    ring->use_modulo = use_modulo;

    err = dma_desc_alloc(desc_size, desc_align, ndesc_bits, ring->desc);
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
errval_t dma_ring_free(struct dma_ring *ring)
{
    errval_t err;

    DMADESC_DEBUG("freeing descriptor ring %p\n", ring);

    err = dma_desc_free(ring->desc[0]);
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
inline uint16_t dma_ring_get_active(struct dma_ring *ring)
{
    return (ring->write_next - ring->tail) & (ring->size - 1);
}

/**
 * \brief returns the number of prepared but not submitted descriptors
 *
 * \param ring  IOAT DMA descriptor ring
 *
 * \returns number of pending descriptors
 */
inline uint16_t dma_ring_get_pendig(struct dma_ring *ring)
{
    return (ring->write_next - ring->issued) & (ring->size - 1);
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
inline uint16_t dma_ring_get_size(struct dma_ring *ring)
{
    return ring->size;
}

/**
 * \brief returns the head pointer index of the ring
 *
 * \param ring  DMA descriptor ring
 *
 * \returns head element index
 */
inline uint16_t dma_ring_get_write_next(struct dma_ring *ring)
{
    return (ring->write_next & (ring->size - 1));
}

/**
 * \brief returns the tail pointer index of the ring
 *
 * \param ring  DMA descriptor ring
 *
 * \returns tail element index
 */
inline uint16_t dma_ring_get_tail(struct dma_ring *ring)
{
    return (ring->tail & (ring->size - 1));
}

/**
 * \brief returns the issued pointer index of the ring
 *
 * \param ring  DMA descriptor ring
 *
 * \returns issued element index
 */
inline uint16_t dma_ring_get_issued(struct dma_ring *ring)
{
    return (ring->issued & (ring->size - 1));
}

/**
 * \brief returns the DMA count of the ring for setting the DMA count register
 *
 * \param ring  DMA descriptor ring
 *
 * \returns dmacount value
 */
inline uint16_t dma_ring_get_head(struct dma_ring *ring)
{
    if (ring->use_modulo) {
        return (ring->head & (ring->size - 1));
    }
    return ring->head;
}

/**
 * \brief gets the next descriptor based on the index
 *
 * \param ring  the DMA ring
 * \param index the index of the ring
 *
 * \returns pointer to a DMA descriptor
 */
inline struct dma_descriptor *dma_ring_get_desc(struct dma_ring *ring,
                                                uint16_t idx)
{
    return ring->desc[idx & (ring->size - 1)];
}

