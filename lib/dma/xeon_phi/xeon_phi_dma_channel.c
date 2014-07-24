/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

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
    lpaddr_t last_processed;            ///<
    struct dma_ring *ring;      ///< Descriptor ring
    xeon_phi_dma_owner_t owner;
};

/**
 * \brief sets the interrupt status according to the mask flag
 *
 * \param chan the dma channel to change the interrupts
 * \param mask if 0 unmask or enabling the interrupts
 *             if 1 masking or disabling the interrupts
 */
static uint32_t xdma_channel_mask_intr(struct xeon_phi_dma_channel *chan,
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
static inline void xdma_channel_ack_intr(struct xeon_phi_dma_channel *chan)
{
    xdma_channel_mask_intr(chan, 1);
    xdma_channel_mask_intr(chan, 0);
}

/**
 * \brief sets the error mask of a Xeon Phi DMA channel
 *
 * \param chan Xeon Phi DMA channel
 * \param mask bitmask for the errors
 */
static inline void xdma_channel_set_error_mask(struct xeon_phi_dma_channel *chan,
                                               uint32_t mask)
{
    xeon_phi_dma_chan_dcherr_wr(&chan->channel, mask);
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

    struct xeon_phi_dma_channel *chan = calloc(1, sizeof(*chan));
    if (chan == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

#ifdef __k1om__
    chan->owner = XEON_PHI_DMA_OWNER_CARD;
#else
    chan->owner = XEON_PHI_DMA_OWNER_HOST;
#endif

    xeon_phi_dma_chan_initialize(&chan->channel,
                                 xeon_phi_dma_device_get_channel_vbase(dev, id));

    xdma_channel_set_error_mask(chan, 0);
    xdma_channel_ack_intr(chan);

    err = dma_ring_alloc(XEON_PHI_DMA_RING_SIZE, XEON_PHI_DMA_DESC_ALIGN,
                         XEON_PHI_DMA_DESC_SIZE, &chan->ring, &chan->common);

    xdma_channel_mask_intr(chan, 0x1);

    return SYS_ERR_OK;
}

#if 0

/**
 * \brief Submits the pending descriptors to the hardware queue
 *
 * \param chan  IOAT DMA channel
 *
 * \returns number of submitted descriptors
 */
uint16_t xeon_phi_dma_channel_issue_pending(struct xeon_phi_dma_channel *chan)
{
    errval_t err;

    uint16_t pending = xeon_phi_dma_ring_get_pendig(chan->ring);

    IOATCHAN_DEBUG("issuing %u pending descriptors to hardware\n",
                    chan->common.id, pending);

    if (chan->common.state != DMA_CHAN_ST_RUNNING) {
        err = xeon_phi_dma_channel_start(chan);
    }
    if (pending > 0) {
        uint16_t dmacnt = xeon_phi_dma_ring_submit_pending(chan->ring);
        xeon_phi_dma_chan_dmacount_wr(&chan->channel, dmacnt);

        IOATCHAN_DEBUG(" setting dma_count to [%u]\n", chan->common.id, dmacnt);
    }

    return pending;
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
    struct dma_channel *dma_chan = &chan->common;

    IOATCHAN_DEBUG("reset channel.\n", dma_chan->id);

    if (dma_chan->state == DMA_CHAN_ST_ERROR) {
        xeon_phi_dma_chan_err_t chanerr = xeon_phi_dma_chan_err_rd(&chan->channel);
        xeon_phi_dma_chan_err_wr(&chan->channel, chanerr);
        IOATCHAN_DEBUG("Reseting channel from error state: [%08x]\n",
                        dma_chan->id, chanerr);

        /*
         * errval_t pci_read_conf_header(uint32_t dword, uint32_t *val);

         errval_t pci_write_conf_header(uint32_t dword, uint32_t val);
         * TODO: clear the xeon_phi_dma_pci_chanerr register in PCI config space
         *       (same approach as above)
         *       -> How to access this ?
         */
    }
    dma_chan->state = DMA_CHAN_ST_RESETTING;

    /* perform reset */
    xeon_phi_dma_chan_cmd_reset_wrf(&chan->channel, 0x1);

    uint16_t reset_counter = 0xFFF;
    do {
        if (!xeon_phi_dma_chan_cmd_reset_rdf(&chan->channel)) {
            break;
        }
        thread_yield();
    }while (reset_counter--);

    if (xeon_phi_dma_chan_cmd_reset_rdf(&chan->channel)) {
        /* reset failed */
        return DMA_ERR_RESET_TIMEOUT;
    }

    /* XXX: Intel BD architecture will need some additional work here */

    dma_chan->state = DMA_CHAN_ST_UNINITIALEZED;

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

    IOATCHAN_DEBUG("starting channel.\n", chan->common.id);

    chan->common.state = DMA_CHAN_ST_RUNNING;
    channel_set_chain_addr(chan);

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
    IOATCHAN_DEBUG("submit request [%016lx]\n", chan->common.id,
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

    struct xeon_phi_dma_channel *xeon_phi_chan = (struct xeon_phi_dma_channel *)chan;

    uint64_t status = xeon_phi_dma_channel_get_status(xeon_phi_chan);

    if (xeon_phi_dma_channel_is_halted(status)) {
        IOATCHAN_DEBUG("channel is in error state\n", chan->id);
        assert(!"NYI: error event handling");
    }

    /* check if there can be something to process */
    if (chan->req_list.head == NULL) {
        return DMA_ERR_CHAN_IDLE;
    }

    lpaddr_t compl_addr_phys = channel_has_completed_descr(xeon_phi_chan);
    if (!compl_addr_phys) {
        return DMA_ERR_CHAN_IDLE;
    }

    err = channel_process_descriptors(xeon_phi_chan, compl_addr_phys);
    switch (err_no(err)) {
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
inline struct xeon_phi_dma_ring *xeon_phi_dma_channel_get_ring(struct xeon_phi_dma_channel *chan)
{
    return chan->ring;
}

/**
 * \brief updates the channel status flag by reading the CHANSTS register
 *
 * \param chan IOAT DMA channel
 */
inline uint64_t xeon_phi_dma_channel_get_status(struct xeon_phi_dma_channel *chan)
{
    uint32_t status_lo = xeon_phi_dma_chan_sts_lo_rd(&chan->channel);
    chan->status = xeon_phi_dma_chan_sts_hi_rd(&chan->channel);
    chan->status <<= 32;
    chan->status |= status_lo;

    return chan->status;
}

#endif
