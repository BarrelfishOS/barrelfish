/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

typedef void (*xeon_phi_dma_done_t)(void);


#define XEON_PHI_DMA_ALIGNMENT 64
#define XEON_PHI_DMA_GRANULARITY 64

struct xeon_phi_dma_chan
{

};

/**
 * represents a DMA request which is currently being executed
 */
struct xeon_phi_dma_request
{
    lpaddr_t from;
    lpaddr_t to;
    size_t length;
    void *st;
    xeon_phi_dma_done_t cb;
    struct xeon_phi_dma_request *next;
    struct xeon_phi_dma_request *prev;
};

#define XEON_PHI_DMA_MEM_HOST 1
#define XEON_PHI_DMA_MEM_CARD 2

/**
 * stores the memory information about the registered memory
 */
struct xeon_phi_dma_mem
{
    lpaddr_t base;
    size_t   size;
    struct capref *cap;
    uint8_t type;
    struct xeon_phi_dma_mem *next;
};

static struct xeon_phi_dma_mem *dma_mem;

static bool verify_address(lpaddr_t base, size_t size)
{
    if (dma_mem == NULL) {
        return 0;
    }

    static struct xeon_phi_dma_mem *mem = dma_mem;

    while(mem) {
        if (mem->base > base) {
            return 0;
        }
        if (mem->base <= base
                && (base + size) <= (mem->base +mem->size)) {
            return 1;
        }
        mem = mem->next;
    }

    return 0;
}

/**
 * \brief   initializes the DMA library
 */
errval_t xeon_phi_dma_init(void);

/**
 * \brief   opens a new DMA channel
 *
 * all descriptor rings owned by the host driver must exist
 * in system memory while rings owned by the coprocessor OS must
 * exist in GDDR5 memory.
 */
errval_t xeon_phi_dma_chan_open(struct xeon_phi_dma_chan **chan);


/**
 * \brief   closes an opened DMA channel
 */
errval_t xeon_phi_dma_chan_close(struct xeon_phi_dma_chan *chan);

errval_t xeon_phi_dma_register_memory(struct capref cap);

errval_t xeon_phi_dma_deregister_memory(struct capref cap);

errval_t xeon_phi_dma_exec();

errval_t xeon_phi_dma_stop();
