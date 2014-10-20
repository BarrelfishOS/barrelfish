/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_MEM_UTILS_H
#define LIB_DMA_MEM_UTILS_H

/* forward declaration */
struct dma_mem;

/**
 * \brief allocates and maps a memory region to be used for DMA purposes
 *
 * \param bytes minimum size of the memory region in bytes
 * \param flags VREGION flags how the region gets mapped
 * \param mem   returns the mapping information
 *
  * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_mem_alloc(size_t bytes,
                       vregion_flags_t flags,
                       struct dma_mem *mem);

/**
 * \brief tries to free the allocated memory region
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_mem_free(struct dma_mem *mem);

#endif /* LIB_DMA_MEM_UTILS_H */
