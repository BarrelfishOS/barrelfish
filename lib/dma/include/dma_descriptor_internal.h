/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DMA_DESCRIPTOR_INTERNAL_H
#define DMA_DESCRIPTOR_INTERNAL_H

#include <dma/dma_descriptor.h>

/*
 * ----------------------------------------------------------------------------
 * Allocation / Deallocation
 * ----------------------------------------------------------------------------
 */

/**
 * \brief allocates a number of hardware DMA descriptors and fills them into the
 *        array of descriptor pointers
 *
 * \param size  size of a signle descriptor in bytes
 * \param align alignment constraints of the descriptors
 * \param count number of descriptors to allocate in bits
 * \param desc  pointer to the array of descriptor pointers
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_desc_alloc(uint32_t size,
                        uint16_t align,
                        uint8_t count,
                        struct dma_descriptor **desc);

/**
 * \brief brief frees up the array of previously allocated descriptors
 *        and releases the resources
 *
 * \param desc  the descriptors to be freed
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t dma_desc_free(struct dma_descriptor *desc);

/*
 * ----------------------------------------------------------------------------
 * Descriptor getters / setters
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns a virtual address pointer to the location where the descriptor
 *        is mapped
 *
 * \param desc IOAT DMA descriptor
 */
uint8_t *dma_desc_get_desc_handle(struct dma_descriptor *desc);

/**
 * \brief sets the corresponding request
 *
 * \param desc IOAT DMA descriptor
 */
void dma_desc_set_request(struct dma_descriptor *desc,
                          struct dma_request *req);

/**
 * \brief returns the physical address of the descriptor
 *
 * \param desc IOAT DMA descriptor
 *
 * \returns physical address of the descriptor
 */
lpaddr_t dma_desc_get_paddr(struct dma_descriptor *desc);

#endif /* DMA_DESCRIPTORS_INTERNAL_H */
