/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_DMA_MEM_H
#define XEON_PHI_DMA_MEM_H

struct xdma_mem;

enum xdma_mem_type
{
    XDMA_MEM_TYPE_HOST,
    XDMA_MEM_TYPE_CARD
};


/**
 * \brief initializes the memory manager for the range checks
 */
errval_t xdma_mem_init(struct xeon_phi_dma_binding *binding,
                       struct xeon_phi *phi);

/**
 * \brief registers a new block of memory to be used with the DMA engine
 */
errval_t xdma_mem_register(struct xdma_mem *mem,
                           struct capref cap);


/**
 * \brief removes a block of memory from the usable regions
 */
errval_t xdma_mem_deregister(struct xdma_mem *mem,
                             struct capref cap);


/**
 * \brief verifies that a certain memory range is within the previously
 *        registered memory range
 */
errval_t xdma_mem_verify(struct xdma_mem *mem,
                         lpaddr_t addr,
                         size_t   size);


#endif /* XEON_PHI_DMA_DESC_RING_H */
