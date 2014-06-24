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

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <xeon_phi/xeon_phi.h>

#include <if/xeon_phi_dma_defs.h>
#include <dev/xeon_phi/xeon_phi_dma_dev.h>

#include "xeon_phi.h"
#include "dma_channel.h"
#include "dma_descriptor_ring.h"
#include "debug.h"


static inline void xdma_channel_set_headptr(struct xdma_channel *chan,
                                            uint16_t entry)
{
    xeon_phi_dma_dhpr_index_wrf(chan->regs, chan->chanid, entry);
}

static inline uint16_t xdma_channel_get_headptr(struct xdma_channel *chan)
{
    return xeon_phi_dma_dhpr_index_rdf(chan->regs, chan->chanid);
}

static inline void xdma_channel_set_tailptr(struct xdma_channel *chan,
                                            uint16_t entry)
{
    xeon_phi_dma_dtpr_index_wrf(chan->regs, chan->chanid, entry);
}

static inline uint16_t xdma_channel_get_tailptr(struct xdma_channel *chan)
{
    return xeon_phi_dma_dtpr_index_rdf(chan->regs, chan->chanid);
}

static uint16_t xdma_channel_get_desc_avail_count(struct xdma_channel *chan)
{
    assert(!"NYI");
    return 0;
}

/*
 * ----------------------------------------------------------------------------
 * Public Interface
 * ----------------------------------------------------------------------------
 */

/**
 * \brief initializes a DMA channel
 *
 * \param chan   where to initialize the DMA channel
 * \param ndesc  number of descriptors in the ring
 * \param regs   pointer to the Mackerel information structure
 * \param chanid id of the channel
 */
errval_t xdma_channel_init(struct xdma_channel *chan,
                           uint16_t ndesc,
                           xeon_phi_dma_t *regs,
                           uint8_t chanid)
{

}

/**
 * \brief frees up the resources used by the channel
 *
 * \param chan  the DMA channel to be freed
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xdma_channel_free(struct xdma_channel *chan)
{

}

/**
 * \brief
 *
 * \param chan
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_* on failure
 *
 */
errval_t xdma_channel_req_memcpy(struct xdma_channel *chan,
                                 struct xdma_req_setup *setup,
                                 xeon_phi_dma_id_t *id)
{
    assert(setup->type == XDMA_REQ_TYPE_MEMCPY);
    assert(!(setup->info.mem.dst & XEON_PHI_MEM_MASK));
    assert(!(setup->info.mem.src & XEON_PHI_MEM_MASK));

    uint32_t num_desc_needed = (setup->info.mem.bytes
                    + XEON_PHI_DMA_REQ_SIZE_MAX - 1) / XEON_PHI_DMA_REQ_SIZE_MAX;

    if (num_desc_needed > XEON_PHI_DMA_DESC_RING_MAX
                    || num_desc_needed > xdma_channel_get_desc_avail_count(chan)) {
        /* we do not support huge requests at this stage... */
    }



    XDEBUG_DMA("memcpy request: %lu bytes, %lu descriptors\n",
               setup->info.mem.bytes, num_desc_needed);

    for(uint16_t i = 0; i < num_desc_needed; ++i) {

    }



    if (id) {

    }
}
