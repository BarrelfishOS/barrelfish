/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DMA_H
#define IOAT_DMA_H


struct ioat_dma_device;
struct ioat_dma_channel;


#define IOAT_DMA_DCA_ENABLE 1

#define IOAT_DMA_IRQ_TYPE IOAT_DMA_IRQ_DISABLED

/**
 * represents a mapped piece of memory
 */
struct ioat_dma_mem
{
    void    *addr;
    lpaddr_t paddr;
    size_t   bytes;
    struct capref frame;
};

struct ioat_dma_ctrl
{
    uint16_t device_num;
    struct ioat_dma_device *devices;
    uint8_t dca_enabled;
    struct ioat_dma_desc_alloc *alloc;
};


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
errval_t ioat_dma_mem_alloc(size_t bytes,
                            vregion_flags_t flags,
                            struct ioat_dma_mem *mem);

/**
 * \brief tries to free the allocated memory region
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_dma_mem_free(struct ioat_dma_mem *mem);


#endif /* IOAT_DMA_H */
