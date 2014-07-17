/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 *
 *
 */
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <pci/pci.h>

#include <dev/ioat_dma_chan_dev.h>

#include "ioat_dma.h"
#include "ioat_dma_device.h"
#include "ioat_dma_channel.h"
#include "ioat_dma_ring.h"
#include "ioat_dma_request.h"
#include "ioat_dma_descriptors.h"

#include "debug.h"

struct ioat_dma_channel
{
    ioat_dma_chan_id_t id;           ///< unique channel id
    ioat_dma_chan_t channel;         ///< Mackerel address
    struct ioat_dma_device *dev;     ///< the DMA device this channel belongs to
    size_t max_xfer_size;            ///< maximum transfer size of this channel
    enum ioat_dma_chan_st state;     ///< channel state
    struct ioat_dma_mem completion;  ///< pointer to the completion area mem region
    lpaddr_t last_completion;        ///<
    struct ioat_dma_ring *ring;      ///< Descriptor ring
    uint64_t status;                 ///< channel status
    uint8_t irq_vector;
    size_t irq_msix;
    struct ioat_dma_request *req_head;
    struct ioat_dma_request *req_tail;
};

static void ioat_dma_chan_do_interrupt_msix(void *arg)
{

}

errval_t ioat_dma_channel_irq_setup_msix(struct ioat_dma_channel *chan)
{
    errval_t err;

    err = pci_setup_inthandler(ioat_dma_chan_do_interrupt_msix, chan,
                               &chan->irq_vector);

    err = pci_msix_vector_init(chan->irq_msix, 0, chan->irq_vector);

    return err;
}

errval_t ioat_dma_channel_reset(struct ioat_dma_channel *chan)
{
    chan->state = IOAT_DMA_CHAN_ST_RESETTING;

    /* mask possible errors */
    ioat_dma_chan_err_t chanerr = ioat_dma_chan_err_rd(&chan->channel);
    ioat_dma_chan_err_wr(&chan->channel, chanerr);

    IOCHAN_DEBUG("Reseting channel. Chanerrors=[%08x]\n", chan->id, chanerr);

    /* TODO: Clear any pending errors in pci config space */
#if 0

    /* clear any pending errors */
    err = pci_read_config_dword(pdev,
                    IOAT_PCI_CHANERR_INT_OFFSET, &chanerr);
    if (err) {
        dev_err(&pdev->dev,
                        "channel error register unreachable\n");
        return err;
    }
    pci_write_config_dword(pdev,
                    IOAT_PCI_CHANERR_INT_OFFSET, chanerr);

#endif

    ioat_dma_chan_cmd_reset_wrf(&chan->channel, 0x1);

    while (ioat_dma_chan_cmd_reset_rdf(&chan->channel)) {
        thread_yield();
    }

    chan->state = IOAT_DMA_CHAN_ST_UNINITIALEZED;

    /* broadwell may need some additional work here */
    return SYS_ERR_OK;
}

/**
 * \brief initializes a new DMA channel and allocates the channel resources
 *
 * \param chan  where to store the channel pointer
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_channel_init(struct ioat_dma_device *dev)
{
    errval_t err;

    IOCHAN_DEBUG("Initializing %u channels max. xfer size is %u bytes\n",
                 0, dev->chan_num, dev->xfer_size_max);

    dev->channels = calloc(dev->chan_num, sizeof(struct ioat_dma_channel));
    if (dev->channels == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    /*
     * XXX: actually there is just one channel per device...
     */

    for (uint8_t i = 0; i < dev->chan_num; ++i) {
        struct ioat_dma_channel *chan = dev->channels + i;
        chan->id = (((uint16_t) dev->devid + 1) << 8) | i;
        chan->dev = dev;
        chan->max_xfer_size = dev->xfer_size_max;

        mackerel_addr_t chan_base = (mackerel_addr_t) dev->mmio.vbase;
        ioat_dma_chan_initialize(&chan->channel, chan_base + ((i + 1) * 0x80));

        ioat_dma_chan_dcactrl_target_cpu_wrf(&chan->channel,
        ioat_dma_chan_dca_ctr_target_any);

        err = ioat_dma_channel_reset(chan);
        if (err_is_fail(err)) {
            return err;
        }

        err = ioat_dma_mem_alloc(IOAT_DMA_CHANNEL_COMPL_SIZE,
                                 IOAT_DMA_CHANNEL_COMPL_FLAGS,
                                 &chan->completion);
        if (err_is_fail(err)) {
            return err;
        }

        memset(chan->completion.addr, 0, IOAT_DMA_CHANNEL_COMPL_SIZE);

        /* write the completion address */
        ioat_dma_chan_cmpl_lo_wr(&chan->channel, chan->completion.paddr);
        ioat_dma_chan_cmpl_hi_wr(&chan->channel, chan->completion.paddr >> 32);

        err = ioat_dma_ring_alloc(IOAT_DMA_DESC_RING_SIZE, &chan->ring, chan);
        if (err_is_fail(err)) {
            ioat_dma_mem_free(&chan->completion);
            return err;
        }

        ioat_dma_chan_ctrl_t chan_ctrl = 0;
        chan_ctrl = ioat_dma_chan_ctrl_err_abort_insert(chan_ctrl, 0x1);
        chan_ctrl = ioat_dma_chan_ctrl_err_cmp_en_insert(chan_ctrl, 0x1);
        chan_ctrl = ioat_dma_chan_ctrl_err_int_en_insert(chan_ctrl, 0x1);
        chan_ctrl = ioat_dma_chan_ctrl_intp_dis_insert(chan_ctrl, 0x1);
        ioat_dma_chan_ctrl_wr(&chan->channel, chan_ctrl);

        chan->state = IOAT_DMA_CHAN_ST_PREPARED;

        /*
         * do a check if the channel operates correctly by issuing a NOP
         */

        IOCHAN_DEBUG("performing selftest on channel with NOP\n", chan->id);

        ioat_dma_request_nop(chan);
        err = ioat_dma_channel_submit_pending(chan);
        if (err_is_fail(err)) {
            ioat_dma_mem_free(&chan->completion);
            return err;
        }

        uint32_t j = 0xFFFF;
        do {
            thread_yield();
        } while (j-- && !ioat_dma_channel_is_active(chan)
                 && !ioat_dma_channel_is_idle(chan));

        if (ioat_dma_channel_is_active(chan) || ioat_dma_channel_is_idle(chan)) {
            IOCHAN_DEBUG("channel worked properly: %016lx\n", chan->id,
                         *(uint64_t* )chan->completion.addr);
            return SYS_ERR_OK;
        } else {
            uint32_t error = ioat_dma_chan_err_rd(&chan->channel);
            IOCHAN_DEBUG(" channel error ERROR: %08x\n", chan->id, error);
            ioat_dma_mem_free(&chan->completion);
            return IOAT_ERR_CHAN_ERROR;
        }
    }

    return SYS_ERR_OK;
}

ioat_dma_chan_id_t ioat_dma_channel_get_id(struct ioat_dma_channel *chan)
{
    return chan->id;
}

/**
 * \brief returns a channel to be used form the give device
 *
 * \param dev IOAT DMA device
 *
 * \returns IOAT DMA Channel
 */
struct ioat_dma_channel *ioat_dma_channel_get(struct ioat_dma_device *dev)
{
    if (dev->chan_next == dev->chan_num) {
        dev->chan_next = 0;
    }
    return &dev->channels[dev->chan_next++];
}

/**
 * \brief returns a channel to be used form the give device based on the channel
 *        index
 *
 * \param dev IOAT DMA device
 * \param idx channel index
 *
 * \returns IOAT DMA Channel
 *          NULL if the index exceeds the number of channels
 */
struct ioat_dma_channel *ioat_dma_channel_get_by_idx(struct ioat_dma_device *dev,
                                                     uint8_t idx)
{
    if (idx < dev->chan_num) {
        return &dev->channels[idx];
    }
    return NULL;
}

struct ioat_dma_desc_alloc *ioat_dma_channel_get_desc_alloc(struct ioat_dma_channel *chan)
{
    return chan->dev->dma_ctrl->alloc;
}

static inline void channel_set_chain_addr(struct ioat_dma_channel *chan)
{
    lpaddr_t chain_addr = ioat_dma_ring_get_chain_addr(chan->ring);

    IOCHAN_DEBUG(" setting chain addr to [%016lx]\n", chan->id, chain_addr);

    ioat_dma_chan_chainaddr_lo_wr(&chan->channel, (uint32_t) chain_addr);
    ioat_dma_chan_chainaddr_hi_wr(&chan->channel, chain_addr >> 32);
}

static inline void channel_update_status(struct ioat_dma_channel *chan)
{
    /*
     * We need to read the low address first as this causes the
     * chipset to latch the upper bits for the subsequent read
     */
    uint32_t status_lo = ioat_dma_chan_sts_lo_rd(&chan->channel);
    chan->status = ioat_dma_chan_sts_hi_rd(&chan->channel);
    chan->status <<= 32;
    chan->status |= status_lo;
}

inline bool ioat_dma_channel_is_active(struct ioat_dma_channel *chan)
{
    channel_update_status(chan);
    uint32_t tr_st = ioat_dma_chan_sts_lo_dma_trans_state_extract(chan->status);
    return tr_st == ioat_dma_chan_trans_state_active;
}

inline bool ioat_dma_channel_is_idle(struct ioat_dma_channel *chan)
{
    channel_update_status(chan);
    uint32_t tr_st = ioat_dma_chan_sts_lo_dma_trans_state_extract(chan->status);
    return tr_st == ioat_dma_chan_trans_state_idle;
}

inline bool ioat_dma_channel_is_halted(struct ioat_dma_channel *chan)
{
    channel_update_status(chan);
    uint32_t tr_st = ioat_dma_chan_sts_lo_dma_trans_state_extract(chan->status);
    return tr_st == ioat_dma_chan_trans_state_halt;
}

inline bool ioat_dma_channel_is_suspended(struct ioat_dma_channel *chan)
{
    channel_update_status(chan);
    uint32_t tr_st = ioat_dma_chan_sts_lo_dma_trans_state_extract(chan->status);
    return tr_st == ioat_dma_chan_trans_state_susp;
}

static inline lpaddr_t channel_get_completion_addr(struct ioat_dma_channel *chan)
{
    lpaddr_t compl_addr = ioat_dma_chan_sts_hi_rd(&chan->channel);
    compl_addr <<= 32;
    compl_addr |= (ioat_dma_chan_sts_lo_rd(&chan->channel));

    return (compl_addr & (~ioat_dma_chan_status_mask));
}

static inline lpaddr_t channel_has_completed_descr(struct ioat_dma_channel *chan)
{
    lpaddr_t curr_compl = channel_get_completion_addr(chan);
    if (curr_compl != chan->last_completion) {
        return curr_compl;
    } else {
        return 0;
    }

}

errval_t ioat_dma_channel_start(struct ioat_dma_channel *chan)
{
    if (chan->state == IOAT_DMA_CHAN_ST_ERROR) {
        return ioat_dma_channel_restart(chan);
    }
    IOCHAN_DEBUG(" starting channel.\n", chan->id);
    chan->state = IOAT_DMA_CHAN_ST_RUNNING;
    channel_set_chain_addr(chan);

    return SYS_ERR_OK;
}

errval_t ioat_dma_channel_restart(struct ioat_dma_channel *chan)
{
    return SYS_ERR_OK;
}

uint16_t ioat_dma_channel_submit_pending(struct ioat_dma_channel *chan)
{
    errval_t err;

    uint16_t pending = ioat_dma_ring_get_pendig(chan->ring);

    IOCHAN_DEBUG(" submitting %u pending desc\n", chan->id, pending);

    if (chan->state != IOAT_DMA_CHAN_ST_RUNNING) {
        err = ioat_dma_channel_start(chan);
    }
    if (pending > 0) {
        uint16_t dmacnt = ioat_dma_ring_submit_pending(chan->ring);
        ioat_dma_chan_dmacount_wr(&chan->channel, dmacnt);

        IOCHAN_DEBUG(" setting dma_count to [%u]\n", chan->id, dmacnt);
    }

    return pending;
}

inline struct ioat_dma_ring *ioat_dma_channel_get_ring(struct ioat_dma_channel *chan)
{
    return chan->ring;
}

inline uint32_t ioat_dma_channel_get_max_xfer_size(struct ioat_dma_channel *chan)
{
    return chan->max_xfer_size;
}

void ioat_dma_channel_enq_request(struct ioat_dma_channel *chan,
                                  struct ioat_dma_request *req)
{
    IOCHAN_DEBUG(" request : enq tail [%016lx]\n",chan->id,  req->id);

    req->next = NULL;
    if (chan->req_head == NULL) {
        chan->req_head = req;
        chan->req_tail = req;
    } else {
        chan->req_tail->next = req;
        chan->req_tail = req;
    }

    ioat_dma_channel_submit_pending(chan);
}

static inline struct ioat_dma_request *channel_deq_request_head(struct ioat_dma_channel *chan)
{
    struct ioat_dma_request *req = chan->req_head;
    if (req == NULL) {
        return NULL;
    }
    chan->req_head = req->next;
    if (chan->req_head == NULL) {
        chan->req_tail = NULL;
    }

    IOCHAN_DEBUG(" request : deq head [%016lx]\n",chan->id,  req->id);

    return req;
}

static inline void channel_enq_request_head(struct ioat_dma_channel *chan,
                                            struct ioat_dma_request *req)
{
    req->next = chan->req_head;
    chan->req_head = req;
    if (chan->req_tail == NULL) {
        chan->req_tail = req;
    }

    IOCHAN_DEBUG(" request : enq head [%016lx]\n",chan->id,  req->id);
}

static errval_t channel_process_descriptors(struct ioat_dma_channel *chan,
                                            lpaddr_t compl_addr_phys)
{
    errval_t err;

    if (!compl_addr_phys) {
        return IOAT_ERR_CHAN_IDLE;
    }

    IOCHAN_DEBUG(" processing [%016lx] head: %u, tail: %u, issued: %u\n",
                 chan->id, compl_addr_phys, ioat_dma_ring_get_head(chan->ring),
                 ioat_dma_ring_get_tail(chan->ring),
                 ioat_dma_ring_get_issued(chan->ring));

    uint16_t active_count = ioat_dma_ring_get_active(chan->ring);

    struct ioat_dma_descriptor *desc;
    struct ioat_dma_request *req, *req_head;

    uint16_t processed = 0;
    uint8_t request_done = 0;

    for (uint16_t i = 0; i < active_count; i++) {
        desc = ioat_dma_ring_get_tail_desc(chan->ring);

        /*
         * check if there is a request associated with the descriptor
         * this indicates the last descriptor of a request
         */
        req = ioat_dma_desc_get_request(desc);
        if (req) {
            req_head = channel_deq_request_head(chan);
            assert(req_head == req);
            err = ioat_dma_request_process(req);
            if (err_is_fail(err)) {
                channel_enq_request_head(chan, req_head);
                return err;
            }
            request_done = 1;
        }

        /* this was the last completed descriptor */
        if (ioat_dma_desc_get_paddr(desc) == compl_addr_phys) {
            processed = i;
            break;
        }
    }

    chan->last_completion = compl_addr_phys;

    /* do a 5us delay per pending descriptor */
    ioat_dma_device_set_intr_delay(chan->dev, (5 * active_count - processed));

    if (request_done) {
        return SYS_ERR_OK;
    }

    return IOAT_ERR_REQUEST_UNFINISHED;
}

errval_t ioat_dma_channel_poll(struct ioat_dma_channel *chan)
{
    errval_t err;

    if (ioat_dma_channel_is_halted(chan)) {
        IOCHAN_DEBUG("channel is in error state\n", chan->id);
    }

    /* process finished descriptors */
    if (chan->req_head == NULL) {
        return IOAT_ERR_CHAN_IDLE;
    }

    lpaddr_t compl_addr_phys = channel_has_completed_descr(chan);
    if (!compl_addr_phys) {
        return IOAT_ERR_CHAN_IDLE;
    }

    err = channel_process_descriptors(chan, compl_addr_phys);
    switch (err_no(err)) {
        case SYS_ERR_OK:
            /* this means we processed a descriptor request */
            return SYS_ERR_OK;
        case IOAT_ERR_REQUEST_UNFINISHED:
            return IOAT_ERR_CHAN_IDLE;
        default:
            return err;
    }
}

