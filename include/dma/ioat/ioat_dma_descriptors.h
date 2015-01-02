/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_IOAT_DMA_DESCRIPTORS_H
#define LIB_IOAT_DMA_DESCRIPTORS_H

struct ioat_dma_descriptor;

/// flags how the descriptors are mapped
#define IOAT_DMA_DESC_MAP_FLAGS VREGION_FLAGS_READ_WRITE

/// the size of the basic descriptor XXX: does not hold for super extended desc!
#define IOAT_DMA_DESC_SIZE 64

/// minimum alignment constraint for the descriptors
#define IOAT_DMA_DESC_ALIGN 64


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
 * \param desc  IOAT DMA descriptor
 * \param src   Source address of the transfer
 * \param dst   destination address of the transfer
 * \param size  number of bytes to copy
 * \param ctrl  control flags
 *
 * XXX: this function assumes that the size of the descriptor has already been
 *      checked and must match the maximum transfer size of the channel
 */
void ioat_dma_desc_fill_memcpy(struct dma_descriptor *desc,
                               lpaddr_t src,
                               lpaddr_t dst,
                               uint32_t size,
                               ioat_dma_desc_ctrl_t ctrl);
/**
 * \brief initializes the hardware specific part of the descriptor
 *
 * \param desc  IOAT DMA descriptor
 * \param src   Source address of the transfer
 * \param dst   destination address of the transfer
 * \param size  number of bytes to copy
 * \param ctrl  control flags
 *
 * XXX: this function assumes that the size of the descriptor has already been
 *      checked and must match the maximum transfer size of the channel
 */
void ioat_dma_desc_fill_memset(struct dma_descriptor *desc,
                               uint64_t data,
                               lpaddr_t dst,
                               uint32_t size,
                               ioat_dma_desc_ctrl_t ctrl);
/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for nop descriptors (null descriptors)
 *
 * \param desc  IOAT DMA descriptor
 */
void ioat_dma_desc_fill_nop(struct dma_descriptor *desc);

#endif  /* LIB_IOAT_DMA_DESCRIPTORS_H */
