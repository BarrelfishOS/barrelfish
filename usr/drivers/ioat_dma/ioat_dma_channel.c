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

#include "debug.h"

enum ioat_dma_chan_st
{
    IOAT_DMA_CHAN_ST_INVALID,
    IOAT_DMA_CHAN_ST_COMPL_PENDING,
    IOAT_DMA_CHAN_ST_COMPL_ACK,
    IOAT_DMA_CHAN_ST_RESETTING,
    IOAT_DMA_CHAN_ST_INIT_FAIL,
    IOAT_DMA_CHAN_ST_RESHAPING,
    IOAT_DMA_CHAN_ST_RUNNING,
    IOAT_DMA_CHAN_ST_ACTIVE
};

struct ioat_dma_channel
{
    ioat_dma_chan_id_t chan_id;      ///< unique channel id
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
    /* mask possible errors */
    ioat_dma_chan_err_t chanerr = ioat_dma_chan_err_rd(&chan->channel);
    ioat_dma_chan_err_wr(&chan->channel, chanerr);

    IOCHAN_DEBUG("Reseting channel %x. Chanerrors=[%08x]\n", chan->chan_id, chanerr);

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
                 dev->chan_num, dev->xfer_size_max);

    dev->channels = calloc(dev->chan_num, sizeof(struct ioat_dma_channel));
    if (dev->channels == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    /*
     * XXX: actually there is just one channel per device...
     */

    for (uint8_t i = 0; i < dev->chan_num; ++i) {
        struct ioat_dma_channel *chan = dev->channels + i;
        chan->chan_id = (((uint16_t) dev->devid + 1) << 8) | i;
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

        /*
         * do a check if the channel operates correctly by issuing a NOP
         */

        IOCHAN_DEBUG("Performing selftest on channel with NOP\n");

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
            IOCHAN_DEBUG("Channel worked properly: %016lx\n",
                         *(uint64_t* )chan->completion.addr);
            return SYS_ERR_OK;
        } else {
            uint32_t error = ioat_dma_chan_err_rd(&chan->channel);
            IOCHAN_DEBUG("Channel error ERROR: %08x\n", error);
            ioat_dma_mem_free(&chan->completion);
            return IOAT_ERR_CHAN_ERROR;
        }
    }

    return SYS_ERR_OK;
}

ioat_dma_chan_id_t ioat_dma_channel_get_id(struct ioat_dma_channel *chan)
{
    return chan->chan_id;
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

struct ioat_dma_desc_alloc *ioat_dma_channel_get_desc_alloc(struct ioat_dma_channel *chan)
{
    return chan->dev->dma_ctrl->alloc;
}

static inline void channel_set_chain_addr(struct ioat_dma_channel *chan,
                                          lpaddr_t chain_addr)
{
    IOCHAN_DEBUG("  setting chain addr to [%016lx]\n", chain_addr);
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

uint16_t ioat_dma_channel_submit_pending(struct ioat_dma_channel *chan)
{
    uint16_t pending = ioat_dma_ring_get_pendig(chan->ring);

    IOCHAN_DEBUG("Submitting %u pending descriptors to channel 0x%04x\n", pending,
                 chan->chan_id);
    if (pending > 0) {
        lpaddr_t chain_addr = ioat_dma_ring_get_chain_addr(chan->ring);
        channel_set_chain_addr(chan, chain_addr);

        uint16_t head = ioat_dma_ring_submit_pending(chan->ring);

        IOCHAN_DEBUG("  setting dma_count to [%u]\n", head);
        ioat_dma_chan_dmacount_wr(&chan->channel, head);
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
    IOCHAN_DEBUG("Enqueuing request [%016lx] on channel [%04x]\n", req->id,
                 chan->chan_id);
    req->next = NULL;
    if (chan->req_head == NULL) {
        chan->req_head = req;
        chan->req_tail = req;
    } else {
        chan->req_tail->next = req;
        chan->req_tail = req;
    }
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
}

errval_t ioat_dma_channel_poll(struct ioat_dma_channel *chan)
{
    errval_t err;

    /* process finished descriptors */
    if (chan->req_head == NULL) {
        return IOAT_ERR_CHAN_IDLE;
    }

    uint32_t compl_count = 0;

    do {
        struct ioat_dma_request *req = channel_deq_request_head(chan);
        err = ioat_dma_request_process(req);
        if (err_is_fail(err)) {
            channel_enq_request_head(chan, req);
            if (err_no(err) != IOAT_ERR_REQUEST_UNFINISHED) {
                return err;
            }
            break;
        }
        compl_count++;
    } while(chan->req_head == NULL);

    if (compl_count) {
        return SYS_ERR_OK;
    } else {
        return IOAT_ERR_CHAN_IDLE;
    }


}

