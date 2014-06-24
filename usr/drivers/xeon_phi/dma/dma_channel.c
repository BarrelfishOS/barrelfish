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
    chan->head = entry;
    xeon_phi_dma_dhpr_index_wrf(chan->regs, chan->chanid, entry - 1);
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

static void xdma_channel_set_owner(struct xdma_channel *chan,
                                   enum xdma_chan_owner owner)
{
    xeon_phi_dma_dcr_t dcr = xeon_phi_dma_dcr_rd(chan->regs);

    uint8_t owner_val;
    if (owner == XDMA_CHAN_CARD_OWNED) {
        owner_val = 0;
    } else {
        owner_val = 1;
    }

    switch (chan->chanid) {
        case 0x0:
            dcr = xeon_phi_dma_dcr_co0_insert(dcr, owner_val);
            break;
        case 0x1:
            dcr = xeon_phi_dma_dcr_co1_insert(dcr, owner_val);
            break;
        case 0x2:
            dcr = xeon_phi_dma_dcr_co2_insert(dcr, owner_val);
            break;
        case 0x3:
            dcr = xeon_phi_dma_dcr_co3_insert(dcr, owner_val);
            break;
        case 0x4:
            dcr = xeon_phi_dma_dcr_co4_insert(dcr, owner_val);
            break;
        case 0x5:
            dcr = xeon_phi_dma_dcr_co5_insert(dcr, owner_val);
            break;
        case 0x6:
            dcr = xeon_phi_dma_dcr_co6_insert(dcr, owner_val);
            break;
        case 0x7:
            dcr = xeon_phi_dma_dcr_co7_insert(dcr, owner_val);
            break;
    }

    xeon_phi_dma_dcr_wr(chan->regs, dcr);

    chan->owner = owner;
}

static void xdma_channel_set_state(struct xdma_channel *chan,
                                   enum xdma_chan_state state)
{
    xeon_phi_dma_dcr_t dcr = xeon_phi_dma_dcr_rd(chan->regs);

    uint8_t enabled_val;
    if (state == XDMA_CHAN_STATE_ENABLED) {
        enabled_val = 0x1;
    } else {
        enabled_val = 0x0;
    }
    switch (chan->chanid) {
        case 0x0:
            dcr = xeon_phi_dma_dcr_ce0_insert(dcr, enabled_val);
            break;
        case 0x1:
            dcr = xeon_phi_dma_dcr_ce1_insert(dcr, enabled_val);
            break;
        case 0x2:
            dcr = xeon_phi_dma_dcr_ce2_insert(dcr, enabled_val);
            break;
        case 0x3:
            dcr = xeon_phi_dma_dcr_ce3_insert(dcr, enabled_val);
            break;
        case 0x4:
            dcr = xeon_phi_dma_dcr_ce4_insert(dcr, enabled_val);
            break;
        case 0x5:
            dcr = xeon_phi_dma_dcr_ce5_insert(dcr, enabled_val);
            break;
        case 0x6:
            dcr = xeon_phi_dma_dcr_ce6_insert(dcr, enabled_val);
            break;
        case 0x7:
            dcr = xeon_phi_dma_dcr_ce7_insert(dcr, enabled_val);
            break;
    }

    chan->state = state;

    xeon_phi_dma_dcr_wr(chan->regs, dcr);
}


static inline void xdma_channel_set_dstat_wb(struct xdma_channel *chan,
                                             lpaddr_t dstat_wb)
{
    xeon_phi_dma_dstatwb_lo_wr(chan->regs, chan->chanid, (uint32_t)dstat_wb);
    xeon_phi_dma_dstatwb_hi_wr(chan->regs, chan->chanid, (uint32_t)(dstat_wb >> 32));
    chan->dstat_wb = dstat_wb;
}


static inline uint8_t xdma_channel_intr_pending(struct xdma_channel *chan)
{
    return xeon_phi_dma_dcar_irq_status_rdf(chan->regs, chan->chanid);
}

/**
 * \brief sets the interrupt status according to the mask flag
 *
 * \param chan the dma channel to change the interrupts
 * \param mask if 0 unmask or enabling the interrupts
 *             if 1 masking or disabling the interrupts
 */
static uint32_t xdma_channel_mask_intr(struct xdma_channel *chan,
                                       uint8_t mask)
{
    uint8_t mask_val = (mask) ? 0x1 : 0x0;

    if (chan->owner == XDMA_CHAN_HOST_OWNED) {
        xeon_phi_dma_dcar_msix_irq_wrf(chan->regs, chan->chanid, mask_val);
    } else {
        xeon_phi_dma_dcar_apic_irq_wrf(chan->regs, chan->chanid, mask_val);
    }

    return xeon_phi_dma_dcar_rd(chan->regs, chan->chanid);
}


static errval_t xdma_channel_set_ring(struct xdma_channel *chan)
{
    assert(!(chan->ring.size & 0x3));
    assert(!(chan->ring.pbase & 0x3F));

    xdma_channel_set_state(chan, XDMA_CHAN_STATE_DISABLED);

    xeon_phi_dma_drar_t drar = 0x0;

    drar = xeon_phi_dma_drar_size_insert(drar, chan->size >> 2);

    if (chan->owner == XDMA_CHAN_HOST_OWNED) {
        drar = xeon_phi_dma_drar_sysbit_insert(drar, 0x1);
        uint32_t sysmem_page = (chan->ring.pbase >> XEON_PHI_SYSMEM_PAGE_BITS);
        drar = xeon_phi_dma_drar_page_insert(drar, sysmem_page);
    }

    drar = xeon_phi_dma_drar_base_insert(drar, chan->ring.pbase >> 6);

    xeon_phi_dma_drar_wr(chan->regs, chan->chanid, drar);

    chan->size = chan->ring.size;

    chan->tail = xdma_channel_get_tailptr(chan);

    xdma_channel_set_state(chan, XDMA_CHAN_STATE_ENABLED);
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
    assert(!"NYI");

    /*
     * initialize the configuration register
     */
#ifdef __k1om__
    xdma_channel_set_owner(chan, XDMA_CHAN_CARD_OWNED);
#else
    xdma_channel_set_owner(chan, XDMA_CHAN_HOST_OWNED);
#endif


    xdma_channel_set_state(chan, XDMA_CHAN_STATE_ENABLED);

    return SYS_ERR_OK;
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
    assert(!"NYI");
    return SYS_ERR_OK;
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
    assert(!(setup->info.mem.bytes & (XEON_PHI_DMA_ALIGNMENT-1)));

    uint32_t num_desc_needed =
                    (setup->info.mem.bytes + XEON_PHI_DMA_REQ_SIZE_MAX - 1) / XEON_PHI_DMA_REQ_SIZE_MAX;

    if (num_desc_needed > XEON_PHI_DMA_DESC_RING_MAX || num_desc_needed
                    > xdma_channel_get_desc_avail_count(chan)) {
        /* we do not support huge requests at this stage... */
    }

    XDMA_DEBUG("memcpy request: %lu bytes, %u descriptors\n",
               setup->info.mem.bytes,
               num_desc_needed);

    lpaddr_t src = setup->info.mem.src;
    lpaddr_t dst = setup->info.mem.dst;
    size_t bytes = setup->info.mem.bytes;
    uint16_t entry = chan->head;
    void *desc = NULL;
    struct xdma_req_info *rinfo = chan->rinfo + entry;

    rinfo->binding = setup->binding;
    while (bytes > XEON_PHI_DMA_REQ_SIZE_MAX) {
        desc = xdma_desc_get_entry(&chan->ring, entry);
        assert(desc);

        xdma_desc_set_memcpy(desc, src, dst, XEON_PHI_DMA_REQ_SIZE_MAX, 0);

        rinfo->head = 0;
        rinfo->done = 0;
        rinfo->last = 0;

        dst += XEON_PHI_DMA_REQ_SIZE_MAX;
        src += XEON_PHI_DMA_REQ_SIZE_MAX;
        bytes -= XEON_PHI_DMA_REQ_SIZE_MAX;
        entry = (entry + 1) % chan->ring.size;
        rinfo = chan->rinfo + entry;
    }

    if (bytes) {
        assert(bytes < XEON_PHI_DMA_REQ_SIZE_MAX);
        xdma_desc_set_memcpy(desc, src, dst, bytes, 0);
        entry = (entry + 1) % chan->ring.size;
        rinfo->head = 0;
        rinfo->done = 0;
        rinfo->last = 1;
    }

    rinfo = chan->rinfo + chan->head;
    rinfo->head = 1;
    rinfo = chan->rinfo + entry - 1;
    rinfo->last = 1;

    xdma_channel_set_headptr(chan, entry);

    if (id) {

    }

    assert(!"NYI");
    return SYS_ERR_OK;
}
