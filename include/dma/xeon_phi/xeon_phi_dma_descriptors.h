/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_XEON_PHI_DMA_DESCRIPTORS_H
#define LIB_XEON_PHI_DMA_DESCRIPTORS_H

#include <xeon_phi/xeon_phi.h>

#include <dev/xeon_phi/xeon_phi_dma_dev.h>

struct dma_descriptor;

/// flags how the descriptors are mapped
#define XEON_PHI_DMA_DESC_MAP_FLAGS VREGION_FLAGS_READ_WRITE

/// the size of the Xeon Phi descriptor
#define XEON_PHI_DMA_DESC_SIZE 16

/// the size of the Xeon Phi descriptor
#define XEON_PHI_DMA_DESC_ALIGN 16

/*
 * ----------------------------------------------------------------------------
 * DMA Descriptor Flags
 * ----------------------------------------------------------------------------
 */
#define XEON_PHI_DMA_DESC_FLAG_INTR 0x01
#define XEON_PHI_DMA_DESC_FLAG_TWB  0x02
#define XEON_PHI_DMA_DESC_FLAG_C    0x04
#define XEON_PHI_DMA_DESC_FLAG_CO   0x08
#define XEON_PHI_DMA_DESC_FLAG_ECY  0x10

/*
 * ----------------------------------------------------------------------------
 * DMA Descriptor Allocation / Free
 * ----------------------------------------------------------------------------
 */

/*
 * ----------------------------------------------------------------------------
 * DMA Descriptor Setup Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for memcpy descriptors
 *
 * \param desc  Xeon Phi descriptor
 * \param src   Source address of the transfer
 * \param dst   destination address of the transfer
 * \param size  number of bytes to copy
 * \param flags control flags
 *
 * XXX: this function assumes that the size of the descriptor has already been
 *      checked and must match the maximum transfer size of the channel
 */
void xeon_phi_dma_desc_fill_memcpy(struct dma_descriptor *desc,
                                   lpaddr_t src,
                                   lpaddr_t dst,
                                   uint32_t size,
                                   uint32_t flags);

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for nop descriptors (null descriptors)
 *
 * \param desc  Xeon Phi descriptor
 */
void xeon_phi_dma_desc_fill_nop(struct dma_descriptor *desc);

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for key descriptors
 *
 * \param desc  Xeon Phi descriptor
 */
static inline void xeon_phi_dma_desc_fill_key(struct dma_descriptor *desc)
{
    assert(!"NYI");
}
/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for key noncecent descriptors
 *
 * \param desc  Xeon Phi descriptor
 */
static inline void xeon_phi_dma_desc_fill_keynoncecent(struct dma_descriptor *desc)
{
    assert(!"NYI");
}

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for general descriptors
 *
 * \param desc  Xeon Phi descriptor
 * \param dst   destination address
 * \param data  Data payload for the request (request specific)
 */
void xeon_phi_dma_desc_fill_general(struct dma_descriptor *desc,
                                    lpaddr_t dst,
                                    uint64_t data);

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for status descriptors
 *
 * \param desc  Xeon Phi descriptor
 * \param dst   destination address
 * \param data  Data payload for the request (request specific)
 * \param flags Descriptor flags
 */
void xeon_phi_dma_desc_fill_status(struct dma_descriptor *desc,
                                    lpaddr_t dst,
                                    uint64_t data,
                                    uint32_t flags);

/*
 * ----------------------------------------------------------------------------
 * Helpers
 * ----------------------------------------------------------------------------
 */

/**
 * \brief translates the physical address to the format the DMA engine understands
 */
static inline lpaddr_t xdma_desc_ring_host2guest(lpaddr_t host_addr)
{
    assert(host_addr < XEON_PHI_SYSMEM_SIZE);
    return (host_addr | XEON_PHI_SYSMEM_BASE);
}

#endif  /* LIB_XEON_PHI_DMA_DESCRIPTORS_H */


