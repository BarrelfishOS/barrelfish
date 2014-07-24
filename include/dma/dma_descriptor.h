/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_DESCRIPTORS_H
#define LIB_DMA_DESCRIPTORS_H

struct dma_descriptor;

/// flags how the descriptors are mapped
#define DMA_DESC_MAP_FLAGS VREGION_FLAGS_READ_WRITE

/*
 * ----------------------------------------------------------------------------
 * Getters / Setters
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the corresponding IOAT DMA request this descriptor belongs
 *
 * \param desc IOAT DMA descriptor
 *
 * \brief pointer to the request
 *        NULL if there is none
 */
struct dma_request *dma_desc_get_request(struct dma_descriptor *desc);

/**
 * \brief returns a pointer to the next descriptor in the descriptor chain
 *
 * \param desc IOAT DMA descriptor
 *
 * \returns next descriptor
 *          NULL if the end of chain
 */
struct dma_descriptor *dma_desc_get_next(struct dma_descriptor *desc);

#endif  /* LIB_DMA_DESCRIPTORS_H */
