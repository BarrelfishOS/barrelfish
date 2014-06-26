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

#define XEON_PHI_DMA_MEM_HOST_MAX (512UL * 1024 * 1024 * 1024)

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
errval_t xdma_mem_register(struct xeon_phi_dma_binding *binding,
                           struct capref cap);

/**
 * \brief removes a block of memory from the usable regions
 */
errval_t xdma_mem_deregister(struct xeon_phi_dma_binding *binding,
                             struct capref cap);

/**
 * \brief verifies the memory range and translates into the DMA usable format
 *
 * \param binding the Xeon Phi DMA binding the request originated
 * \param size    size of the requested range
 * \param addr    base address of the range
 *
 * \returns DMA address if the address has previously been registered
 *          0 otherwise
 */
lpaddr_t xdma_mem_verify(struct xeon_phi_dma_binding *binding,
                         lpaddr_t addr,
                         size_t size);

struct xeon_phi *xdma_mem_get_phi(struct xeon_phi_dma_binding *binding);

#endif /* XEON_PHI_DMA_DESC_RING_H */
