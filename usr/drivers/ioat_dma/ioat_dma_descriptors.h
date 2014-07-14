/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DMA_DESCRIPTORS_H
#define IOAT_DMA_DESCRIPTORS_H

struct ioat_dma_desc_pool;

/// the minimum amount of DMA descriptors to allocate
#define IOAT_DMA_DESC_COUNT

#define IOAT_DMA_DESC_SIZE 0

#define IOAT_DMA_DESC_ALIGN 64

/**
 *
 */
errval_t ioat_dma_desc_alloc_init(size_t element_size,
                                  size_t align);



void *ioat_dma_desc_alloc(void );

errval_t ioat_dma_desc_free(void);

errval_t ioat_dma_desc_chain_alloc(void);

errval_t ioat_dma_desc_chain_free(void);



#endif /* IOAT_DMA_CHANNEL_H */
