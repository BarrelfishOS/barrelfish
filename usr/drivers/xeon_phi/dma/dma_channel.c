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

#include "xeon_phi_internal.h"
#include "dma_channel.h"
#include "dma_descriptor_ring.h"
#include "debug.h"

#define IS_POW2(num) (((num) != 0) && (((num) & (~(num) + 1)) == (num)))

/**
 * \brief updates the head pointer of the DMA channel according to the value
 *        stored in the channel struct
 *
 * \param chan Xeon Phi DMA channel
 */
static inline void xdma_channel_set_headptr(struct xdma_channel *chan)
{
    assert(chan->head < chan->size);
    XDMAV_DEBUG("chan %u: setting head pointer of to: %u\n",
                chan->chanid, chan->head);

    xeon_phi_dma_dhpr_index_wrf(chan->regs, chan->chanid, chan->head);
}

/**
 * \brief reads the tail pointer of the DMA channel
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns tail pointer of the DMA channel
 */
static inline uint16_t xdma_channel_get_tailptr(struct xdma_channel *chan)
{
    return xeon_phi_dma_dtpr_index_rdf(chan->regs, chan->chanid);
}

/**
 * \brief updates the tail pointer register of the DMA channel
 *
 * \param chan Xeon Phi DMA channel
 */
static inline void xdma_channel_set_tailptr(struct xdma_channel *chan)
{
    return xeon_phi_dma_dtpr_index_wrf(chan->regs, chan->chanid, chan->tail);
}

/**
 * \brief sets the error mask of a Xeon Phi DMA channel
 *
 * \param chan Xeon Phi DMA channel
 * \param mask  bitmask for the errors
 */
static inline void xdma_channel_set_error_mask(struct xdma_channel *chan,
                                               uint32_t mask)
{
    xeon_phi_dma_dcherr_wr(chan->regs, chan->chanid, mask);
}

/**
 * \brief updates the DMA statistics writeback register of the channel
 *
 * \param chan      Xeon Phi DMA channel
 * \param dstat_wb  physical address where the data is written
 */
static inline void xdma_channel_set_dstat_wb(struct xdma_channel *chan,
                                             lpaddr_t dstat_wb)
{
    uint32_t dstat_wb_hi = (uint32_t) (dstat_wb >> 32);
    xeon_phi_dma_dstatwb_lo_wr(chan->regs, chan->chanid, (uint32_t) dstat_wb);
    xeon_phi_dma_dstatwb_hi_wr(chan->regs, chan->chanid, dstat_wb_hi);
    chan->dstat_wb = dstat_wb;
}

/**
 * \brief obtains the address of the DMA stat writeback location
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns physical address of the writeback location
 */
static inline lpaddr_t xdma_channel_get_dstab_wb(struct xdma_channel *chan)
{
    lpaddr_t addr = xeon_phi_dma_dstatwb_hi_rd(chan->regs, chan->chanid);
    addr = addr << 32;
    addr |= xeon_phi_dma_dstatwb_lo_rd(chan->regs, chan->chanid);
    return addr;
}

/**
 * \brief checks if there is an interrupt pending for the channel
 *
 * \param chan Xeon Phi DMA channel
 */
static inline uint8_t xdma_channel_intr_pending(struct xdma_channel *chan)
{
    return xeon_phi_dma_dcar_irq_status_rdf(chan->regs, chan->chanid);
}

/**
 * \brief Channel statistics: number of completed transfers
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns #completed transfers
 */
static inline uint16_t xdma_channel_get_completions(struct xdma_channel *chan)
{
    return xeon_phi_dma_dstat_completions_rdf(chan->regs, chan->chanid);
}

/**
 * \brief calculates the next entry where to write the descriptor
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns index of of the entry in the descriptor table
 */
static inline uint16_t xdma_channel_get_next_entry(struct xdma_channel *chan)
{
    return ((++chan->write_next) & (chan->size - 1));
}

/**
 * \brief increments the head pointer of the channel and updates the register
 *
 * \param chan  Xeon Phi DMA channel
 * \param count the number of entries to advance the head pointer
 *
 */
static inline uint16_t xdma_channel_incr_head(struct xdma_channel *chan,
                                              uint16_t count)
{
    chan->head = (chan->head + count) & (chan->size - 1);
    xdma_channel_set_headptr(chan);
    return chan->head;
}

/**
 * \brief calculates the number of descriptors needed for the transfer
 *
 * \param bytes the size of the transfer in bytes
 *
 * \returns #descriptors needed
 */
static inline uint32_t xdma_channel_num_desc(size_t bytes)
{

    return (bytes + XEON_PHI_DMA_REQ_SIZE_MAX - 1) / XEON_PHI_DMA_REQ_SIZE_MAX;
}

/**
 * \brief sets the owner of this channel
 *
 * \param chan  Xeon Phi DMA channel
 * \param owner the new owner of the channel (card or host)
 */
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

/**
 * \brief updates the channel state
 *
 * \param chan  Xeon Phi DMA channel
 * \param owner the new state of the channel (enabled or disabled)
 */
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

/**
 * \brief acknowledges the interrupts by masking them and renable them
 *
 * \param chan  Xeon Phi DMA channel
 */
static inline void xdma_channel_ack_intr(struct xdma_channel *chan)
{
    xdma_channel_mask_intr(chan, 1);
    xdma_channel_mask_intr(chan, 0);
}

/**
 * \brief sets the previously allocated descriptor ring to be used
 *        with that channel
 *
 * \param chan  Xeon Phi DMA channel
 */
static errval_t xdma_channel_set_ring(struct xdma_channel *chan)
{
    assert(!(chan->ring.size & 0x3));
    assert(!(chan->ring.pbase & 0x3F));

    xdma_channel_set_state(chan, XDMA_CHAN_STATE_DISABLED);

    xeon_phi_dma_drar_hi_t drar_hi = 0x0;
    xeon_phi_dma_drar_lo_t drar_lo = 0x0;

    uint16_t num_desc = chan->size >> xeon_phi_dma_drar_size_shift;

    drar_hi = xeon_phi_dma_drar_hi_size_insert(drar_hi, num_desc);

    if (chan->owner == XDMA_CHAN_HOST_OWNED) {
        /*
         * if the channel is host owned we need to set the system bit
         * and the system memory page accordingly
         */
        drar_hi = xeon_phi_dma_drar_hi_sysbit_insert(drar_hi, 0x1);
        uint32_t sysmem_page = (chan->ring.pbase >> XEON_PHI_SYSMEM_PAGE_BITS);
        drar_hi = xeon_phi_dma_drar_hi_page_insert(drar_hi, sysmem_page);
        drar_hi = xeon_phi_dma_drar_hi_base_insert(drar_hi, chan->ring.pbase >> 32);
    }

    drar_lo = xeon_phi_dma_drar_lo_base_insert(drar_lo, chan->ring.pbase >> 6);

    xeon_phi_dma_drar_lo_wr(chan->regs, chan->chanid, drar_lo);
    xeon_phi_dma_drar_hi_wr(chan->regs, chan->chanid, drar_hi);

    chan->size = chan->ring.size;

    chan->tail = 0;
    chan->last_processed = (uint16_t) -1;
    xdma_channel_set_tailptr(chan);

    xdma_channel_set_state(chan, XDMA_CHAN_STATE_ENABLED);

    return SYS_ERR_OK;
}

/**
 * \brief calculates the number of descriptors we need to process
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns number of descriptors pending to process
 */
static inline uint16_t xmda_channel_process_count(struct xdma_channel *chan)
{
    chan->tail = xdma_channel_get_tailptr(chan);

    uint16_t lp_entry = (chan->last_processed + 1) & (chan->size - 1);

    if (chan->tail > lp_entry) {
        return (chan->tail - lp_entry);
    } else if (lp_entry > chan->tail) {
        return (chan->size - lp_entry + chan->tail);
    } else {
        return 0;
    }
}

/**
 * \brief calculates the number of availabe descriptors in the channel
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns number of free descriptor slots
 *          0 if there are none left
 */
static inline uint16_t xmda_channel_avail_space(struct xdma_channel *chan)
{
    uint16_t head = (chan->head & (chan->size - 1));
    uint16_t lp_entry = (chan->last_processed + 1) & (chan->size - 1);
#if XDEBUG_DMA
    if (chan->tail > lp_entry) {
        XDMA_DEBUG("there are unprocessed descriptors: [%u, %u]\n",
                        chan->last_processed,
                        chan->tail);
    }
#endif
    if (head > lp_entry) {
        return (lp_entry - 0) + (chan->size - head);
    } else if (lp_entry > head) {
        return (lp_entry - head);
    } else {
        return (chan->size - 1);
    }
}

#define XDMA_CHANNE_SPACE_RETRIES 500000

/**
 * \brief checks if there is enough space for the request to be served.
 *        if there is too less space, then it tries to poll the channel
 *        so that some slots are freed
 *
 * \param chan   Xeon Phi DMA channel
 * \param needed the number of descripors needed
 *
 * \returns number of free descriptor slots
 *          0 if there are too less
 */
static uint16_t xdma_channel_has_space(struct xdma_channel *chan,
                                       uint16_t needed)
{
    errval_t err;

    uint16_t count = xmda_channel_avail_space(chan);

    if (needed > chan->size) {
        return 0;
    }

    if (needed < count) {
        return count - 1;
    }

    uint32_t retries = XDMA_CHANNE_SPACE_RETRIES;
    while (count < needed && retries--) {
        thread_yield();

        err = xdma_channel_poll(chan);
        if (err_no(err) == XEON_PHI_ERR_DMA_IDLE) {
            return 0;
        }

        count = xmda_channel_avail_space(chan);
        if (count > needed) {
            return count - 1;
        }
    }
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
                           uint8_t chanid,
                           uint8_t do_poll)
{
    errval_t err;

    XDMA_DEBUG("initializing channel %u with %u desc\n", chanid, ndesc);

    assert(chan->state == XDMA_CHAN_STATE_UNINITIALIZED);

    if (!IS_POW2(ndesc)) {
        return XEON_PHI_ERR_DMA_NUM_DESC;
    }

    memset(chan, 0, sizeof(*chan));

    chan->rinfo = calloc(ndesc, sizeof(struct xdma_req_info));
    if (chan->rinfo == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    chan->chanid = chanid;
    chan->regs = regs;
    chan->size = ndesc;
    chan->do_poll = do_poll;
    chan->write_next = -1;

    /* set the error mask */
    xdma_channel_set_error_mask(chan, 0);
    xdma_channel_ack_intr(chan);

#ifdef __k1om__
    xdma_channel_set_owner(chan, XDMA_CHAN_CARD_OWNED);
#else
    xdma_channel_set_owner(chan, XDMA_CHAN_HOST_OWNED);
#endif

    err = xeon_phi_dma_desc_ring_alloc(&chan->ring, ndesc);
    if (err_is_fail(err)) {
        return err;
    }

    err = xdma_channel_set_ring(chan);
    assert(err_is_ok(err)); /* should not fail */

    if (do_poll) {
        xdma_channel_mask_intr(chan, 0x1);
    } else {
        xdma_channel_mask_intr(chan, 0x0);
    }

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
                                 struct dma_req_setup *setup,
                                 xeon_phi_dma_id_t *id)
{
    assert(setup->type == XDMA_REQ_TYPE_MEMCPY);
    assert(!(setup->info.mem.dst & ~XEON_PHI_MEM_MASK));
    assert(!(setup->info.mem.src & ~XEON_PHI_MEM_MASK));
    assert(!(setup->info.mem.bytes & (XEON_PHI_DMA_ALIGNMENT-1)));

    uint32_t num_desc_needed =  xdma_channel_num_desc(setup->info.mem.bytes);

    XDMAV_DEBUG("memcpy [%016lx]->[%016lx], chan=%u, %lu bytes, %u desc\n",
                    setup->info.mem.src, setup->info.mem.dst, chan->chanid,
                    setup->info.mem.bytes, num_desc_needed);

    if (num_desc_needed > chan->size) {
        /* the request would have more descriptors than we support */
        return XEON_PHI_ERR_DMA_REQ_SIZE;
    }

    if (!xdma_channel_has_space(chan, num_desc_needed)) {
        /* there are too less descriptors available at this moment */
        return XEON_PHI_ERR_DMA_NO_DESC;
    }

    lpaddr_t src = setup->info.mem.src;
    lpaddr_t dst = setup->info.mem.dst;
    size_t bytes = setup->info.mem.bytes;

    void *desc;
    uint16_t entry;
    struct xdma_req_info *rinfo;
    uint16_t first = 0;
    uint16_t last = num_desc_needed;
    xeon_phi_dma_id_t dma_id = xdma_chan_generate_id(chan);

    do {
        assert(last);
        uint32_t length = (bytes > XEON_PHI_DMA_REQ_SIZE_MAX ?
                                       XEON_PHI_DMA_REQ_SIZE_MAX : bytes);

        entry = xdma_channel_get_next_entry(chan);
        desc = xdma_desc_get_entry(&chan->ring, entry);
        rinfo = &chan->rinfo[entry];

        assert(!((rinfo->flags & XDMA_REQ_FLAG_VALID)
                   && !(rinfo->flags & XDMA_REQ_FLAG_DONE)));

        rinfo->flags = XDMA_REQ_FLAG_VALID;

        if (!first) {
            first = 1;
            rinfo->flags |= XDMA_REQ_FLAG_FIRST;

        }

        if (!(--last)) {
            rinfo->flags |= XDMA_REQ_FLAG_LAST;
        }

        rinfo->id = dma_id;
        rinfo->st = setup->st;
        rinfo->cb = setup->cb;

        XDMAV_DEBUG("memcpy setting entry @ [%p]: [%u] {0x08%x}\n",
                        desc, entry, rinfo->flags);

        xdma_desc_set_memcpy(desc, src, dst, length, 0);

        bytes -= length;
        src += length;
        dst += length;
    } while (bytes > 0);

    xdma_channel_incr_head(chan, num_desc_needed);

    XDMAV_DEBUG("memcpy request: id=%016lx, head: [%u]\n",
                    (uint64_t )dma_id, chan->head);

    if (id) {
        *id = dma_id;
    }

    return SYS_ERR_OK;
}

/**
 *
 */
errval_t xdma_channel_intr_handler(struct xdma_channel *chan)
{
    assert(!"NYI");
    return SYS_ERR_OK;
#if 0
    int i = 0;
    int ring_size = chan->intr_ring.ring.size;
    struct dma_completion_cb **temp = chan->intr_ring.comp_cb_array;
    struct dma_completion_cb *cb;
    int new_tail, old_tail;

    if (mic_hw_family(chan->dma_ctx->device_num) == FAMILY_KNC &&
                    mic_hw_stepping(chan->dma_ctx->device_num) >= KNC_B0_STEP) {
        unsigned long error = *((uint32_t*)chan->chan->dstat_wb_loc);
        if (unlikely(test_bit(31, &error)))
        printk(KERN_ERR "DMA h/w error - %s %d, dstatwb=%lx\n",
                        __func__, __LINE__, error);
    }
    new_tail = read_tail(&chan->intr_ring.ring);
    old_tail = chan->intr_ring.old_tail;

    for (; i < ring_size && old_tail != new_tail;
                    old_tail = incr_rb_index(old_tail, ring_size), i++) {
        cb = (struct dma_completion_cb *)xchg(&temp[old_tail], NULL);
        if (cb) {
            cb->dma_completion_func(cb->cb_cookie);
        }
    }
    chan->intr_ring.old_tail = new_tail;
    update_tail(&chan->intr_ring.ring, new_tail);
    wake_up(&chan->intr_wq);
    if (i == ring_size && old_tail != new_tail) {
        printk(KERN_ERR PR_PREFIX "Something went wrong, old tail = %d, new tail = %d\n",
                        old_tail, new_tail);
    }
#endif

}

/**
 * \brief polls the channel for completed DMA transfers
 *
 * \param chan the DMA channel to poll
 *
 * \returns SYS_ERR_OK on success
 *          XEON_PHI_ERR_DMA_IDLE if there were no new completed transfers
 *          XEON_PHI_ERR_DMA_* on failure
 */
errval_t xdma_channel_poll(struct xdma_channel *chan)
{
    errval_t err;

    if (!chan->do_poll) {
        return SYS_ERR_OK;
    }

    uint16_t num_process = xmda_channel_process_count(chan);

    if (num_process == 0) {
        return XEON_PHI_ERR_DMA_IDLE;
    }

    uint16_t entry = ((chan->last_processed + 1) & (chan->size - 1));

    XDMAV_DEBUG("processing %u finished DMA descriptors [%u, %u]\n",
                    num_process, entry, chan->tail);

    assert(num_process < chan->size);

    while (num_process--) {
        struct xdma_req_info *rinfo = &chan->rinfo[entry];

        XDMAV_DEBUG("processing entry: [%u] {0x08%x}\n", entry, rinfo->flags);

        rinfo->flags |= XDMA_REQ_FLAG_DONE;

        chan->last_processed = entry;

        if (rinfo->flags & XDMA_REQ_FLAG_LAST) {
            /* this is the last element of the request */
            XDMAV_DEBUG("Request 0x%016lx done. Last entry was [%u]\n",
                            (uint64_t )first->id, entry);
            if (rinfo->cb) {
                err = rinfo->cb(rinfo->st, SYS_ERR_OK, rinfo->id);
                if (err_is_fail(err)) {
                    return err;
                }
            }
        }
        entry = ((entry + 1) & (chan->size - 1));
    }

    return SYS_ERR_OK;
}

