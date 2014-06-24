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

#include <barrelfish/barrelfish.h>
#include "xeon_phi.h"

#include <if/xeon_phi_dma_defs.h>

struct xdma_mem
{
    struct xeon_phi *phi;
    struct xeon_phi_dma_binding *b;
    struct xdma_mem_entry *head;
};

struct xdma_mem_entry
{
    struct capref cap;
    lpaddr_t paddr;
    size_t size;
    struct xdma_mem_entry *next;
    struct xdma_mem_entry *prev;
};

/**
 * \brief initializes the memory manager for the range checks
 */
errval_t xdma_mem_init(struct xeon_phi_dma_binding *binding,
                       struct xeon_phi *phi)
{
    struct xdma_mem *xmem = calloc(1, sizeof(struct xdma_mem));
    if (xmem == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    xmem->b = binding;
    xmem->phi = phi;
    binding->st = xmem;

    return SYS_ERR_OK;
}

/**
 * \brief registers a new block of memory to be used with the DMA engine
 */
errval_t xdma_mem_register(struct xdma_mem *mem,
                           struct capref cap)
{

}

/**
 * \brief removes a block of memory from the usable regions
 */
errval_t xdma_mem_deregister(struct xdma_mem *mem,
                             struct capref cap)
{

}

/**
 * \brief verifies that a certain memory range is within the previously
 *        registered memory range
 */
errval_t xdma_mem_verify(struct xdma_mem *mem,
                         lpaddr_t addr,
                         size_t size)
{

}

/**
 * \brief verifies the memory range and translates into the DMA usable format
 */
lpaddr_t xdma_mem_translate(struct xdma_mem *mem,
                            lpaddr_t addr,
                            size_t size)
{

}

