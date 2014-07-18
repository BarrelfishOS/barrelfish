/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DMA_DESCRIPTORS_INTERNAL_H
#define IOAT_DMA_DESCRIPTORS_INTERNAL_H

#include <dma/ioat/ioat_dma_descriptors.h>

/*
 * ----------------------------------------------------------------------------
 * Allocation / Deallocation
 * ----------------------------------------------------------------------------
 */

/**
 * \brief allocates a number of hardware DMA descriptors and fills them into the
 *        array of descriptor pointers
 *
 * \param size  descriptor size
 * \param align alignment constraints of the descriptors
 * \param count number of descriptors to allocate
 * \param desc  pointer to the array of descriptor pointers
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_desc_alloc(uint16_t size,
                         uint16_t align,
                         uint16_t count,
                         struct ioat_dma_descriptor **desc);

/**
 * \brief brief frees up the array of previously allocated descriptors
 *        and releases the resources
 *
 * \param desc  the descriptors to be freed
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_desc_free(struct ioat_dma_descriptor *desc);

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
ioat_dma_desc_t ioat_desc_get_desc_handle(struct ioat_dma_descriptor *desc);

/**
 * \brief sets the corresponding request
 *
 * \param desc IOAT DMA descriptor
 */
void ioat_desc_set_request(struct ioat_dma_descriptor *desc,
                               struct ioat_dma_request *req);

/**
 * \brief sets the next pointer of the descriptor and does the corresponding
 *        hardware linkage
 *
 * \param desc descriptor to set the next field
 * \param next following descriptor
 */
void ioat_desc_set_next(struct ioat_dma_descriptor *desc,
                        struct ioat_dma_descriptor *next);

/**
 * \brief returns the physical address of the descriptor
 *
 * \param desc IOAT DMA descriptor
 *
 * \returns physical address of the descriptor
 */
lpaddr_t ioat_desc_get_paddr(struct ioat_dma_descriptor *desc);

#endif /* IOAT_DMA_DESCRIPTORS_INTERNAL_H */
