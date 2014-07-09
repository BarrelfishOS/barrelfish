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
#ifndef XEON_PHI_DMA_DESC_RING_H
#define XEON_PHI_DMA_DESC_RING_H

#include <string.h> /* for memset() */
#include <barrelfish/barrelfish.h>

#include <dev/xeon_phi/xeon_phi_dma_dev.h>

#include "xeon_phi_internal.h"
#include "dma.h"

/// the maximum length of the descriptor ring 128k-1 rounded to cache line
//#define XEON_PHI_DMA_DESC_RING_MAX (128*1024 - XEON_PHI_DMA_ALIGNMENT)
#define XEON_PHI_DMA_DESC_RING_MAX ((1<<16) - XEON_PHI_DMA_ALIGNMENT)

/// the size of a descriptor entry
#define XEON_PHI_DMA_DESC_SIZE  16

/// checks if a certain value is aligned to a multiple of cache line
#define XDMA_ASSERT_ALIGNED(x) \
    assert((x) && ((((uintptr_t)x) & (XEON_PHI_DMA_ALIGNMENT - 1)) == 0))

/**
 *
 */
struct xdma_ring
{
    uint16_t size;      ///< the maximum number of elements in the ring
    lpaddr_t pbase;     ///< physical base address of the ring
    void    *vbase;     ///< virtual base address of the ring
    struct capref cap;
};

/**
 * \brief calculates the virtual address of the descriptor at entry
 *
 * \param ring  the dma ring
 * \param entry the entry
 *
 * \returns pointer to the entry
 *          NULL if out or range
 */
static inline void *xdma_desc_get_entry(struct xdma_ring *ring,
                                        uint16_t entry)
{
    if (entry < ring->size) {
        return ((uint8_t*)(ring->vbase))+(entry*XEON_PHI_DMA_DESC_SIZE);
    }
    return NULL;
}

/**
 * \brief clears the DMA descriptor by zeroing it
 *
 * \param desc  pointer to the memory location of th descriptor
 */
static inline void xdma_desc_clear(void *desc)
{
    memset(desc, 0, XEON_PHI_DMA_DESC_SIZE);
}

/**
 * \brief fills in a descriptor of type memcopy
 *
 * \param desc  the descriptor to set
 * \param src   source address of the transfer (PHYS)
 * \param dst   destination address of the transfer (PHYS)
 * \param bytes size of the transfer in bytes
 */
static inline void xdma_desc_set_memcpy(void *desc,
                                        lpaddr_t src,
                                        lpaddr_t dst,
                                        uint32_t bytes,
                                        uint32_t flags)
{
    xdma_desc_clear(desc);

    XDMA_ASSERT_ALIGNED(src);
    XDMA_ASSERT_ALIGNED(dst);
    XDMA_ASSERT_ALIGNED(bytes);

    xeon_phi_dma_desc_memcpy_src_insert(desc, src);
    xeon_phi_dma_desc_memcpy_dst_insert(desc, dst);
    xeon_phi_dma_desc_memcpy_length_insert(desc,
                                           (bytes >> XEON_PHI_DMA_ALIGN_SHIFT));
    xeon_phi_dma_desc_memcpy_dtype_insert(desc, xeon_phi_dma_desc_memcpy);
}

/**
 * \brief fills in a descriptor of type status
 *
 * \param desc  the descriptor to wrige
 * \param dst   where to store the daqta
 * \param data  data
 * \param intr_enable   enable interupts on this one
 */
static inline void xdma_desc_set_status(void *desc,
                                        lpaddr_t dst,
                                        uint64_t data,
                                        uint8_t intr_enable)
{
    xdma_desc_clear(desc);

    xeon_phi_dma_desc_status_data_insert(desc, data);
    xeon_phi_dma_desc_status_dst_insert(desc, dst);
    if (intr_enable) {
        xeon_phi_dma_desc_status_intr_insert(desc, 1);
    }

    xeon_phi_dma_desc_status_dtype_insert(desc, xeon_phi_dma_desc_status);
}

/**
 * \brief fills in a descriptor of type general purpose
 *
 * \param desc  the descriptor to wrige
 * \param dst   where to store the daqta
 * \param data  data
 */
static inline void xdma_desc_set_general(void *desc,
                                        lpaddr_t dst,
                                        uint64_t data)
{
    xdma_desc_clear(desc);

    xeon_phi_dma_desc_general_data_insert(desc, data);
    xeon_phi_dma_desc_general_dst_insert(desc, dst);

    xeon_phi_dma_desc_general_dtype_insert(desc, xeon_phi_dma_desc_general);
}

static inline void xdma_desc_set_keynoncecent(void *desc)
{
    assert(!"NYI: xdma_desc_set_keynoncecent");
}

static inline void xdma_desc_set_key(void *desc)
{
    assert(!"NYI: xdma_desc_set_key");
}

static inline void xdma_desc_set_nop(void *desc) {
    xdma_desc_clear(desc);
}

/**
 * \brief translates the physical address to the format the DMA engine understands
 */
static inline lpaddr_t xdma_desc_ring_host2guest(lpaddr_t host_addr)
{
    assert(host_addr < XEON_PHI_SYSMEM_SIZE);
    return (host_addr | XEON_PHI_SYSMEM_BASE);
}

static inline void xdma_desc_ring_clear(struct xdma_ring *ring)
{
    memset(ring->vbase, 0, XEON_PHI_DMA_DESC_SIZE * ring->size);
}

/**
 * \brief initializes a dma descriptor ring and allocates memory for it
 *
 * \param ring  the ring structure to initialize
 * \param size  number of elements in the ring
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xeon_phi_dma_desc_ring_alloc(struct xdma_ring *ring,
                                      uint16_t size);

/**
 * \brief frees up the resources used by the ring.
 *
 * \param ring the descriptor ring to be freed
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_dma_desc_ring_free(struct xdma_ring *ring);

#endif /* XEON_PHI_DMA_DESC_RING_H */
