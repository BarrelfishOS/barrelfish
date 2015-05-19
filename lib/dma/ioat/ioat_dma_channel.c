/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <dev/ioat_dma_chan_dev.h>

#include <dma_mem_utils.h>

#include <dma_ring_internal.h>
#include <ioat/ioat_dma_internal.h>
#include <ioat/ioat_dma_device_internal.h>
#include <ioat/ioat_dma_channel_internal.h>
#include <ioat/ioat_dma_descriptors_internal.h>
#include <ioat/ioat_dma_request_internal.h>

#include <debug.h>

struct ioat_dma_channel
{
    struct dma_channel common;

    ioat_dma_chan_t channel;         ///< Mackerel address

    lpaddr_t last_completion;        ///<
    struct dma_mem completion;
    struct dma_ring *ring;          ///< Descriptor ring
    uint64_t status;                 ///< channel status
};

/**
 * \brief sets the star of the descriptor chain address of the DMA channel
 *
 * \param chan  IOAT DMA channel
 */
static inline void channel_set_chain_addr(struct ioat_dma_channel *chan)
{
    lpaddr_t chain_addr = dma_ring_get_chain_addr(chan->ring);

    IOATCHAN_DEBUG("setting chain addr to [%016lx]\n", chan->common.id,
                   chain_addr);

    ioat_dma_chan_chainaddr_lo_wr(&chan->channel, (uint32_t) chain_addr);
    ioat_dma_chan_chainaddr_hi_wr(&chan->channel, chain_addr >> 32);
}

/**
 * \brief reads the channel status and returns the physical address of the last
 *        completed descriptor
 *
 * \param chan IOAT DMA channel
 *
 * \returns physical address of last descriptor
 */
static inline lpaddr_t channel_get_completion_addr(struct ioat_dma_channel *chan)
{
    lpaddr_t compl_addr = *((lpaddr_t*) chan->completion.vaddr);

    return (compl_addr & (~ioat_dma_chan_status_mask));
}

/**
 * \brief checks if the channel has completed descriptors which can be processed
 *        and returns the physical address of the last one.
 *
 * \param chan IOAT DMA channel
 *
 * \returns physical address of last descriptor
 *          0 if there were no new descriptors to process
 */
static inline lpaddr_t channel_has_completed_descr(struct ioat_dma_channel *chan)
{
    lpaddr_t curr_compl = channel_get_completion_addr(chan);
    if (curr_compl != chan->last_completion) {
        return curr_compl;
    } else {
        return 0;
    }
}

/**
 * \brief processes the completed descriptors of a DMA channel and finishes
 *        the requests
 *
 * \param chan             IAOT DMA channel
 * \param compl_addr_phyis physical address of the last completed descriptor
 *
 * \returns SYS_ERR_OK on if the request was processed to completion
 *          DMA_ERR_CHAN_IDLE if there was no descriptor to process
 *          DMA_ERR_REQUEST_UNFINISHED if the request is still not finished
 *          errval on error
 */
static errval_t channel_process_descriptors(struct ioat_dma_channel *chan,
                                            lpaddr_t compl_addr_phys)
{
    errval_t err;

    if (!compl_addr_phys) {
        return DMA_ERR_CHAN_IDLE;
    }

    IOATCHAN_DEBUG("processing [%016lx] wrnxt: %u, tail: %u, issued: %u\n",
                   chan->common.id, compl_addr_phys,
                   dma_ring_get_write_next(chan->ring), dma_ring_get_tail(chan->ring),
                   dma_ring_get_issued(chan->ring));

    uint16_t active_count = dma_ring_get_active(chan->ring);

    struct dma_descriptor *desc;
    struct dma_request *req;
    struct dma_request *req_head;

    uint16_t processed = 0;
    uint8_t request_done = 0;

    for (uint16_t i = 0; i < active_count; i++) {
        desc = dma_ring_get_tail_desc(chan->ring);

        /*
         * check if there is a request associated with the descriptor
         * this indicates the last descriptor of a request
         */
        req = dma_desc_get_request(desc);
        if (req) {
            req_head = dma_channel_deq_request_head(&chan->common);
            assert(req_head == req);
            err = ioat_dma_request_process((struct ioat_dma_request *) req);
            if (err_is_fail(err)) {
                dma_channel_enq_request_head(&chan->common, req_head);
                return err;
            }
            request_done = 1;
        }

        /* this was the last completed descriptor */
        if (dma_desc_get_paddr(desc) == compl_addr_phys) {
            processed = i;
            break;
        }
    }

    chan->last_completion = compl_addr_phys;

    /* do a 5us delay per pending descriptor */
    ioat_dma_device_set_intr_delay((struct ioat_dma_device *) chan->common.device,
                                   (5 * active_count - processed));

    if (request_done) {
        return SYS_ERR_OK;
    }

    return DMA_ERR_REQUEST_UNFINISHED;
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
errval_t ioat_dma_channel_init(struct ioat_dma_device *dev,
                               uint8_t id,
                               uint32_t max_xfer,
                               struct ioat_dma_channel **ret_chan)
{

    errval_t err;

    struct ioat_dma_channel *chan = calloc(1, sizeof(*chan));
    if (chan == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct dma_device *dma_dev = (struct dma_device *) dev;
    struct dma_channel *dma_chan = &chan->common;

    dma_chan->id = dma_channel_id_build(dma_device_get_id(dma_dev), id);
    dma_chan->device = dma_dev;
    dma_chan->max_xfer_size = max_xfer;

    IOATCHAN_DEBUG("initialize channel with  max. xfer size of %u bytes\n",
                   dma_chan->id, max_xfer);

    mackerel_addr_t chan_base = dma_device_get_mmio_vbase(dma_dev);
    ioat_dma_chan_initialize(&chan->channel, chan_base + ((id + 1) * 0x80));

    ioat_dma_chan_dcactrl_target_cpu_wrf(&chan->channel,
                                         ioat_dma_chan_dca_ctr_target_any);

    err = ioat_dma_channel_reset(chan);
    if (err_is_fail(err)) {
        return err;
    }

    ioat_dma_device_get_complsts_addr(dev, &chan->completion);

    /* write the completion address */
    ioat_dma_chan_cmpl_lo_wr(&chan->channel, chan->completion.paddr);
    ioat_dma_chan_cmpl_hi_wr(&chan->channel, chan->completion.paddr >> 32);

    err = dma_ring_alloc(IOAT_DMA_DESC_RING_SIZE, IOAT_DMA_DESC_ALIGN,
                         IOAT_DMA_DESC_SIZE, 0x0, dma_chan, &chan->ring);
    if (err_is_fail(err)) {
        dma_mem_free(&chan->completion);
        return err;
    }

    /* we have to do the hardware linkage */
    struct dma_descriptor *dcurr, *dnext;
    for (uint32_t i = 0; i < (1 << IOAT_DMA_DESC_RING_SIZE); ++i) {
        dcurr = dma_ring_get_desc(chan->ring, i);
        dnext = dma_desc_get_next(dcurr);
        assert(dnext);
        ioat_dma_desc_next_insert(dma_desc_get_desc_handle(dcurr),
                                  dma_desc_get_paddr(dnext));
    }

    ioat_dma_chan_ctrl_t chan_ctrl = 0;
    chan_ctrl = ioat_dma_chan_ctrl_err_abort_insert(chan_ctrl, 0x1);
    chan_ctrl = ioat_dma_chan_ctrl_err_cmp_en_insert(chan_ctrl, 0x1);
    chan_ctrl = ioat_dma_chan_ctrl_err_int_en_insert(chan_ctrl, 0x1);
    chan_ctrl = ioat_dma_chan_ctrl_intp_dis_insert(chan_ctrl, 0x1);
    ioat_dma_chan_ctrl_wr(&chan->channel, chan_ctrl);

    dma_chan->state = DMA_CHAN_ST_PREPARED;
    dma_chan->f.memcpy = ioat_dma_request_memcpy_chan;
    dma_chan->f.memset = ioat_dma_request_memset_chan;
    dma_chan->f.poll = ioat_dma_channel_poll;

    *ret_chan = chan;

    /*
     * do a check if the channel operates correctly by issuing a NOP
     */
    IOATCHAN_DEBUG("performing selftest on channel with NOP\n", dma_chan->id);

    ioat_dma_request_nop_chan(chan);
    err = ioat_dma_channel_issue_pending(chan);
    if (err_is_fail(err)) {
        dma_mem_free(&chan->completion);
        return err;
    }

    uint32_t j = 0xFFFF;
    uint64_t status;
    do {
        status = ioat_dma_channel_get_status(chan);
        thread_yield();
    } while (j-- && !ioat_dma_channel_is_active(status)
             && !ioat_dma_channel_is_idle(status));

    if (ioat_dma_channel_is_active(status) || ioat_dma_channel_is_idle(status)) {
        IOATCHAN_DEBUG("channel worked properly: %016lx\n", dma_chan->id,
                       *(uint64_t* ) chan->completion.vaddr);
        return SYS_ERR_OK;
    } else {
        IOATCHAN_DEBUG(" channel error ERROR: %08x\n", dma_chan->id,
                       ioat_dma_chan_err_rd(&chan->channel));
        dma_mem_free(&chan->completion);
        free(chan);
        *ret_chan = NULL;
        return DMA_ERR_CHAN_ERROR;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Submits the pending descriptors to the hardware queue
 *
 * \param chan  IOAT DMA channel
 *
 * \returns number of submitted descriptors
 */
uint16_t ioat_dma_channel_issue_pending(struct ioat_dma_channel *chan)
{
    errval_t err;

    uint16_t pending = dma_ring_get_pendig(chan->ring);

    IOATCHAN_DEBUG("issuing %u pending descriptors to hardware\n",
                   chan->common.id, pending);

    if (chan->common.state != DMA_CHAN_ST_RUNNING) {
        err = ioat_dma_channel_start(chan);
    }
    if (pending > 0) {
        uint16_t dmacnt = dma_ring_submit_pending(chan->ring);
        ioat_dma_chan_dmacount_wr(&chan->channel, dmacnt);

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
errval_t ioat_dma_channel_reset(struct ioat_dma_channel *chan)
{
    struct dma_channel *dma_chan = &chan->common;

    IOATCHAN_DEBUG("reset channel.\n", dma_chan->id);

    if (dma_chan->state == DMA_CHAN_ST_ERROR) {
        ioat_dma_chan_err_t chanerr = ioat_dma_chan_err_rd(&chan->channel);
        ioat_dma_chan_err_wr(&chan->channel, chanerr);
        IOATCHAN_DEBUG("Reseting channel from error state: [%08x]\n",
                       dma_chan->id, chanerr);

        /*
         * errval_t pci_read_conf_header(uint32_t dword, uint32_t *val);

         errval_t pci_write_conf_header(uint32_t dword, uint32_t val);
         * TODO: clear the ioat_dma_pci_chanerr register in PCI config space
         *       (same approach as above)
         *       -> How to access this ?
         */
    }
    dma_chan->state = DMA_CHAN_ST_RESETTING;

    /* perform reset */
    ioat_dma_chan_cmd_reset_wrf(&chan->channel, 0x1);

    uint16_t reset_counter = 0xFFF;
    do {
        if (!ioat_dma_chan_cmd_reset_rdf(&chan->channel)) {
            break;
        }
        thread_yield();
    } while (reset_counter--);

    if (ioat_dma_chan_cmd_reset_rdf(&chan->channel)) {
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
errval_t ioat_dma_channel_restart(struct ioat_dma_channel *chan)
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
errval_t ioat_dma_channel_start(struct ioat_dma_channel *chan)
{
    if (chan->common.state == DMA_CHAN_ST_ERROR) {
        return ioat_dma_channel_restart(chan);
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
errval_t ioat_dma_channel_stop(struct ioat_dma_channel *chan)
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
errval_t ioat_dma_channel_suspend(struct ioat_dma_channel *chan)
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
errval_t ioat_dma_channel_submit_request(struct ioat_dma_channel *chan,
                                         struct ioat_dma_request *req)
{
    IOATCHAN_DEBUG("submit request [%016lx]\n", chan->common.id,
                   dma_request_get_id((struct dma_request * )req));

    dma_channel_enq_request_tail(&chan->common, (struct dma_request *) req);

    ioat_dma_channel_issue_pending(chan);

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
errval_t ioat_dma_channel_poll(struct dma_channel *chan)
{
    errval_t err;

    struct ioat_dma_channel *ioat_chan = (struct ioat_dma_channel *) chan;

    uint64_t status = ioat_dma_channel_get_status(ioat_chan);

    if (ioat_dma_channel_is_halted(status)) {
        IOATCHAN_DEBUG("channel is in error state\n", chan->id);
        char buf[512];
        ioat_dma_chan_err_pr(buf, 512, &ioat_chan->channel);
        printf("channel error: %s\n", buf);
        assert(!"NYI: error event handling");
    }

    /* check if there can be something to process */
    if (chan->req_list.head == NULL) {
        return DMA_ERR_CHAN_IDLE;
    }

    lpaddr_t compl_addr_phys = channel_has_completed_descr(ioat_chan);
    if (!compl_addr_phys) {
        return DMA_ERR_CHAN_IDLE;
    }

    err = channel_process_descriptors(ioat_chan, compl_addr_phys);
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
inline struct dma_ring *ioat_dma_channel_get_ring(struct ioat_dma_channel *chan)
{
    return chan->ring;
}

/**
 * \brief updates the channel status flag by reading the CHANSTS register
 *
 * \param chan IOAT DMA channel
 */
inline uint64_t ioat_dma_channel_get_status(struct ioat_dma_channel *chan)
{
    uint32_t status_lo = ioat_dma_chan_sts_lo_rd(&chan->channel);
    chan->status = ioat_dma_chan_sts_hi_rd(&chan->channel);
    chan->status <<= 32;
    chan->status |= status_lo;

    return chan->status;
}

