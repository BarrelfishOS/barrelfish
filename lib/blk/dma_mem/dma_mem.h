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

#include <barrelfish/types.h>
#include <barrelfish/capabilities.h>

struct dma_mem
{
    lvaddr_t vaddr;         ///< virtual address of the mapped region
    lpaddr_t paddr;         ///< physical address of the underlying frame
    uint64_t bytes;         ///< size of the region in bytes
    uint64_t requested;     ///< requested size of the region in bytes (<= bytes)
    struct capref frame;    ///< frame capability backing this region
};

/**
 * \brief allocates and maps a memory region to be used for DMA purposes
 *
 * \param bytes minimum size of the memory region in bytes
 * \param mem   returns the mapping information
 *
  * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_mem_alloc(size_t bytes, struct dma_mem *mem);

/**
 * \brief Initializes a dma_mem struct for a given capref
 *
 * \param frame The frame with a reference to memory
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_mem_from_capref(struct capref frame, struct dma_mem *mem);

/**
 * \brief tries to free the allocated memory region
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_mem_free(struct dma_mem *mem);



#endif /* LIB_DMA_MEM_UTILS_H */
