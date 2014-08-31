/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <xeon_phi/xeon_phi.h>

#include <dev/xeon_phi/xeon_phi_dma_chan_dev.h>

#include <dma_mem_utils.h>

#include <xeon_phi/xeon_phi_dma_internal.h>
#include <xeon_phi/xeon_phi_dma_device_internal.h>
#include <xeon_phi/xeon_phi_dma_channel_internal.h>
#include <dma_ring_internal.h>
#include <xeon_phi/xeon_phi_dma_descriptors_internal.h>
#include <xeon_phi/xeon_phi_dma_request_internal.h>

#include <debug.h>


struct xeon_phi_dma_channel
{
    struct dma_channel common;
    xeon_phi_dma_chan_t channel;         ///< Mackerel address
    struct dma_mem dstat;
    struct dma_ring *ring;              ///< Descriptor ring
    uint16_t last_processed;
    xeon_phi_dma_owner_t owner;
};

static inline uint8_t channel_has_intr_pending(struct xeon_phi_dma_channel *chan)
{
    return xeon_phi_dma_chan_dcar_irq_status_rdf(&chan->channel);
}

/**
 * \brief sets the interrupt status according to the mask flag
 *
 * \param chan the dma channel to change the interrupts
 * \param mask if 0 unmask or enabling the interrupts
 *             if 1 masking or disabling the interrupts
 */
static uint32_t channel_mask_intr(struct xeon_phi_dma_channel *chan,
                                  uint8_t mask)
{
    uint8_t mask_val = (mask) ? 0x1 : 0x0;

    if (chan->owner == XEON_PHI_DMA_OWNER_CARD) {
        xeon_phi_dma_chan_dcar_msix_irq_wrf(&chan->channel, mask_val);
    } else {
        xeon_phi_dma_chan_dcar_apic_irq_wrf(&chan->channel, mask_val);
    }

    return xeon_phi_dma_chan_dcar_rd(&chan->channel);
}

/**
 * \brief acknowledges the interrupts by masking them and renable them
 *
 * \param chan  Xeon Phi DMA channel
 */
static inline void channel_ack_intr(struct xeon_phi_dma_channel *chan)
{
    channel_mask_intr(chan, 1);
    channel_mask_intr(chan, 0);
}

/**
 * \brief sets the error mask of a Xeon Phi DMA channel
 *
 * \param chan Xeon Phi DMA channel
 * \param mask bitmask for the errors
 */
static inline void channel_set_error_mask(struct xeon_phi_dma_channel *chan,
                                          uint32_t mask)
{
    xeon_phi_dma_chan_dcherrmsk_wr(&chan->channel, mask);
}

/**
 * \brief reads the tail pointer register of the Xeon Phi DMA channel
 *
 * \param chan  Xeon Phi DMA channel
 *
 * \returns tail pointer value
 */
static inline uint16_t channel_read_tail(struct xeon_phi_dma_channel *chan)
{
    return xeon_phi_dma_chan_dtpr_index_rdf(&chan->channel);
}

/**
 * \brief updates the tail pointer register of the channel
 *
 * \param chan  Xeon Phi DMA channel
 * \param tail  the new tailpointer index
 */
static inline void channel_write_tail(struct xeon_phi_dma_channel *chan,
                                      uint16_t tail)
{
    XPHICHAN_DEBUG("setting tail pointer to [%u]\n", chan->common.id, tail);
    xeon_phi_dma_chan_dtpr_index_wrf(&chan->channel, tail);
}

/**
 * \brief reads the head pointer register of the Xeon Phi DMA channel
 *
 * \param chan  Xeon Phi DMA channel
 *
 * \returns head pointer value
 */
static inline uint16_t channel_read_head(struct xeon_phi_dma_channel *chan)
{
    return xeon_phi_dma_chan_dhpr_index_rdf(&chan->channel);
}

/**
 * \brief updates the head pointer register of the channel
 *
 * \param chan  Xeon Phi DMA channel
 * \param head  the new headpointer index
 */
static inline void channel_write_head(struct xeon_phi_dma_channel *chan,
                                      uint16_t head)
{
    XPHICHAN_DEBUG("setting head pointer to [%u]\n", chan->common.id, head);
    xeon_phi_dma_chan_dhpr_index_wrf(&chan->channel, head);
}

/**
 * \brief updates the DMA statistics writeback register of the channel
 *
 * \param chan      Xeon Phi DMA channel
 * \param dstat_wb  physical address where the data is written
 */
static inline void channel_set_dstat_wb(struct xeon_phi_dma_channel *chan,
                                        lpaddr_t dstat_wb)
{
    if (chan->owner == XEON_PHI_DMA_OWNER_HOST) {
        dstat_wb |= XEON_PHI_SYSMEM_BASE;
    }

    XPHICHAN_DEBUG("setting channel_set_dstat_wb to 0x%016lx\n", chan->common.id,
                   dstat_wb);
    xeon_phi_dma_chan_dstatwb_lo_wr(&chan->channel, (uint32_t) dstat_wb);
    xeon_phi_dma_chan_dstatwb_hi_wr(&chan->channel, (uint32_t) (dstat_wb >> 32));
}

/**
 * \brief obtains the address of the DMA stat writeback location
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns physical address of the writeback location
 */
static inline lpaddr_t channel_get_dstab_wb(struct xeon_phi_dma_channel *chan)
{
    lpaddr_t addr = xeon_phi_dma_chan_dstatwb_hi_rd(&chan->channel);
    addr = addr << 32;
    addr |= xeon_phi_dma_chan_dstatwb_lo_rd(&chan->channel);
    return addr;
}

/**
 * \brief Channel statistics: number of completed transfers
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns #completed transfers
 */
static inline uint16_t channel_get_completions(struct xeon_phi_dma_channel *chan)
{
    return xeon_phi_dma_chan_dstat_completions_rdf(&chan->channel);
}

/**
 * \brief sets the previously allocated descriptor ring to be used
 *        with that channel
 *
 * \param chan  Xeon Phi DMA channel
 */
static errval_t channel_set_ring(struct xeon_phi_dma_channel *chan,
                                 struct dma_ring *ring)
{
    struct xeon_phi_dma_device *xdev;
    uint16_t size = dma_ring_get_size(ring);
    lpaddr_t ring_base = dma_ring_get_base_addr(ring);
    uint8_t idx = chan->common.id;
    assert(idx < XEON_PHI_DMA_DEVICE_CHAN_TOTAL);

    if ((size & 0x3) || (ring_base & 0x3F)) {
        return DMA_ERR_ALIGNMENT;
    }

    XPHICHAN_DEBUG("setting ring base to [0x%016lx] with size 0x%x\n",
                   chan->common.id, ring_base, size);

    assert(!(size & 0x3));
    assert(!(ring_base & 0x3F));

    xdev = (struct xeon_phi_dma_device *) chan->common.device;
    xeon_phi_dma_device_set_channel_state(xdev, idx,
                                          XEON_PHI_DMA_CHANNEL_DISABLE);

    xeon_phi_dma_chan_drar_hi_t drar_hi = 0x0;
    xeon_phi_dma_chan_drar_lo_t drar_lo = 0x0;

    uint16_t num_desc = size >> xeon_phi_dma_drar_size_shift;

    drar_hi = xeon_phi_dma_chan_drar_hi_size_insert(drar_hi, num_desc);

    if (chan->owner == XEON_PHI_DMA_OWNER_HOST) {
        XPHICHAN_DEBUG("Channel is host owned. setting bit\n", chan->common.id);
        /*
         * if the channel is host owned we need to set the system bit
         * and the system memory page accordingly
         */
        drar_hi = xeon_phi_dma_chan_drar_hi_sysbit_insert(drar_hi, 0x1);
        uint32_t sysmem_page = (ring_base >> XEON_PHI_SYSMEM_PAGE_BITS);
        drar_hi = xeon_phi_dma_chan_drar_hi_page_insert(drar_hi, sysmem_page);
        drar_hi = xeon_phi_dma_chan_drar_hi_base_insert(drar_hi, ring_base >> 32);
    }

    drar_lo = xeon_phi_dma_chan_drar_lo_base_insert(drar_lo, ring_base >> 6);

    XPHICHAN_DEBUG("setting ring base to [%08x][%08x]\n", chan->common.id,
                   drar_hi, drar_lo);

    xeon_phi_dma_chan_drar_lo_wr(&chan->channel, drar_lo);
    xeon_phi_dma_chan_drar_hi_wr(&chan->channel, drar_hi);

    channel_write_tail(chan, 0x0);

    xeon_phi_dma_device_set_channel_state(xdev, idx, XEON_PHI_DMA_CHANNEL_ENABLE);

    return SYS_ERR_OK;
}


static errval_t channel_process_descriptors(struct xeon_phi_dma_channel *chan,
                                            uint16_t tail)
{
    errval_t err = DMA_ERR_REQUEST_UNFINISHED;

    struct dma_ring *ring = chan->ring;
    while(dma_ring_get_tail(ring) != tail) {
        struct dma_descriptor *desc = dma_ring_get_tail_desc(ring);
        struct dma_request *req =  dma_desc_get_request(desc);
        if (req) {
            err = xeon_phi_dma_request_process((struct xeon_phi_dma_request *)req);
        }
    }

    chan->last_processed = tail;

    return err;
}

/*
 * ============================================================================
 * Library Internal Interface
 * ============================================================================
 */

/**
 * \brief initializes and allocates resources for a new channel DMA channel
 *        belonging to a device
 *
 * \param dev       IOAT DMA device
 * \param id        id of this channel
 * \param max_xfer  maximum size in bytes for a transfer
 * \param ret_chan  returned channel pointer
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_dma_channel_init(struct xeon_phi_dma_device *dev,
                                   uint8_t id,
                                   uint32_t max_xfer,
                                   struct xeon_phi_dma_channel **ret_chan)
{

    errval_t err;

    struct dma_device *dma_dev = (struct dma_device *) dev;

    uint8_t idx = id + XEON_PHI_DMA_DEVICE_CHAN_OFFSET;
    assert(idx < XEON_PHI_DMA_DEVICE_CHAN_TOTAL);

    struct xeon_phi_dma_channel *chan = calloc(1, sizeof(*chan));
    if (chan == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    chan->common.device = (struct dma_device *) dev;
    chan->common.id = dma_channel_id_build(dma_device_get_id(dma_dev), idx);
    chan->common.max_xfer_size = max_xfer;
    chan->common.f.memcpy = xeon_phi_dma_request_memcpy_chan;
    chan->common.f.poll = xeon_phi_dma_channel_poll;

    XPHICHAN_DEBUG("initializig channel with max_xfer=%u\n", chan->common.id,
                   max_xfer);

    xeon_phi_dma_chan_initialize(&chan->channel,
                                 xeon_phi_dma_device_get_channel_vbase(dev, idx));

    channel_ack_intr(chan);

#ifdef __k1om__
    chan->owner = XEON_PHI_DMA_OWNER_CARD;
    xeon_phi_dma_device_set_channel_owner(dev, idx, XEON_PHI_DMA_OWNER_CARD);
#else
    chan->owner = XEON_PHI_DMA_OWNER_HOST;
    xeon_phi_dma_device_set_channel_owner(dev, idx, XEON_PHI_DMA_OWNER_HOST);
#endif

    err = dma_ring_alloc(XEON_PHI_DMA_RING_SIZE, XEON_PHI_DMA_DESC_ALIGN,
                         XEON_PHI_DMA_DESC_SIZE, 0x1, &chan->common,
                         &chan->ring);
    if (err_is_fail(err)) {
        free(chan);
        return err;
    }

    xeon_phi_dma_device_set_channel_state(dev, idx,
                                          XEON_PHI_DMA_CHANNEL_DISABLE);

    xeon_phi_dma_device_get_dstat_addr(dev, &chan->dstat);
    channel_set_dstat_wb(chan, chan->dstat.paddr);

    channel_set_error_mask(chan, 0);

    XPHICHAN_DEBUG("setting ring: [%016lx] @ %p\n", chan->common.id,
                   dma_ring_get_base_addr(chan->ring), chan->ring);

    err = channel_set_ring(chan, chan->ring);
    if (err_is_fail(err)) {
        free(chan);
        return err;
    }

    channel_mask_intr(chan, 0x1);

    chan->common.state = DMA_CHAN_ST_PREPARED;

    if (ret_chan) {
        *ret_chan = chan;
    }

    xeon_phi_dma_device_set_channel_state(dev, idx, 0x1);

    return SYS_ERR_OK;
}

/**
 * \brief Submits the pending descriptors to the hardware queue
 *
 * \param chan  Xeon Phi DMA channel
 *
 * \returns number of submitted descriptors
 */
uint16_t xeon_phi_dma_channel_issue_pending(struct xeon_phi_dma_channel *chan)
{
    errval_t err;

    uint16_t pending = dma_ring_get_pendig(chan->ring);

    XPHICHAN_DEBUG("issuing %u pending descriptors to hardware\n",
                   chan->common.id, pending);

    if (chan->common.state != DMA_CHAN_ST_RUNNING) {
        err = xeon_phi_dma_channel_start(chan);
    }
    if (pending > 0) {
        uint16_t head = dma_ring_submit_pending(chan->ring);
        assert(head < dma_ring_get_size(chan->ring));
        channel_write_head(chan, head);
    }

    return pending;
}

/**
 * \brief returns the status writeback address
 *
 * \param chan Xeon Phi DMA channel
 *
 * \returns physical address of the dstat WB address
 */
lpaddr_t xeon_phi_dma_channel_get_dstat_wb(struct xeon_phi_dma_channel *chan)
{
    lpaddr_t dstat_wb = chan->dstat.paddr;
    if (chan->owner == XEON_PHI_DMA_OWNER_HOST) {
        dstat_wb += XEON_PHI_SYSMEM_BASE;
    }
    return dstat_wb;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/*
 * ----------------------------------------------------------------------------
 * Channel State Management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief Resets a IOAT DMA channel
 *
 * \param chan  IOAT DMA channel to be reset
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_CHAN_RESET on reset timeout
 */
errval_t xeon_phi_dma_channel_reset(struct xeon_phi_dma_channel *chan)
{

    return SYS_ERR_OK;
}

/**
 * \brief restarts a IOAT DMA channel this updates the chain address register
 *        and the DMA count register.
 *
 * \param chan  IOAT DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t xeon_phi_dma_channel_restart(struct xeon_phi_dma_channel *chan)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/**
 * \brief starts a IOAT DMA channel. This sets the chain address to the first
 *        entry of the ring and the DMA count to zero.
 *
 * \param chan  IOAT DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t xeon_phi_dma_channel_start(struct xeon_phi_dma_channel *chan)
{
    if (chan->common.state == DMA_CHAN_ST_ERROR) {
        return xeon_phi_dma_channel_restart(chan);
    }

    if (chan->common.state == DMA_CHAN_ST_RUNNING) {
        return SYS_ERR_OK;
    }

    XPHICHAN_DEBUG("starting channel.\n", chan->common.id);

    assert(!"NYI");

    return SYS_ERR_OK;
}

/**
 * \brief stopps the processing of the descriptors.
 *
 * \param chan  IOAT DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t xeon_phi_dma_channel_stop(struct xeon_phi_dma_channel *chan)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/**
 * \brief Puts the IOAT DMA channel into the suspended state
 *
 * \param chan  IOAT DMA channel
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t xeon_phi_dma_channel_suspend(struct xeon_phi_dma_channel *chan)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/**
 * \brief enqueues a request onto the IOAT DMA channel and submits it to the
 *        hardware
 *
 * \param chan  IOAT DMA channel
 * \param req   IOAT DMA request to be submitted
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_* on failure
 */
errval_t xeon_phi_dma_channel_submit_request(struct xeon_phi_dma_channel *chan,
                                             struct xeon_phi_dma_request *req)
{
    XPHICHAN_DEBUG("submit request [%016lx]\n", chan->common.id,
                   dma_request_get_id((struct dma_request * )req));

    dma_channel_enq_request_tail(&chan->common, (struct dma_request *) req);

    xeon_phi_dma_channel_issue_pending(chan);

    return SYS_ERR_OK;
}

/**
 * \brief polls the IOAT DMA channel for completed events
 *
 * \param chan  IOAT DMA channel
 *
 * \returns SYS_ERR_OK if there was something processed
 *          DMA_ERR_CHAN_IDLE if there was no request on the channel
 *          DMA_ERR_REQUEST_UNFINISHED if the request has not been completed yet
 *
 */
errval_t xeon_phi_dma_channel_poll(struct dma_channel *chan)
{
    errval_t err;

    struct xeon_phi_dma_channel *xchan = (struct xeon_phi_dma_channel *) chan;

    uint16_t tail = channel_read_tail(xchan);

    if (xeon_phi_dma_chan_dcherr_rd(&xchan->channel)) {
        USER_PANIC("Handling of error state not implemented!\n");
    }

    if (chan->req_list.head == NULL) {
        return DMA_ERR_CHAN_IDLE;
    }

    if (tail == xchan->last_processed) {
        return DMA_ERR_CHAN_IDLE;
    }

    err =  channel_process_descriptors(xchan, tail);
    switch(err_no(err)) {
        case SYS_ERR_OK:
        /* this means we processed a descriptor request */
        return SYS_ERR_OK;
        case DMA_ERR_REQUEST_UNFINISHED:
        return DMA_ERR_CHAN_IDLE;
        default:
        return err;
    }
}

/*
 * ----------------------------------------------------------------------------
 * Getter / Setter Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the associated IOAT DMA descriptor ring of a channel
 *
 * \param chan  IOAT DMA channel
 *
 * \returns IOAT DMA descriptor ring handle
 */
inline struct dma_ring *xeon_phi_dma_channel_get_ring(struct xeon_phi_dma_channel *chan)
{
    return chan->ring;
}

