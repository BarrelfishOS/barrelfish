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


/// the minimum amount of DMA descriptors to allocate
#define IOAT_DMA_DESC_COUNT


errval_t ioat_dma_desc_alloc();

errval_t ioat_dma_desc_free();

errval_t ioat_dma_desc_chain_alloc();

errval_t ioat_dma_desc_chain_free();



#endif /* IOAT_DMA_CHANNEL_H */
