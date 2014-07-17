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

#include <ioat/ioat_dma_internal.h>
#include <ioat/ioat_dma_device_internal.h>
#include <ioat/ioat_dma_channel_internal.h>
#include <ioat/ioat_dma_ring_internal.h>
#include <ioat/ioat_dma_descriptors_internal.h>

#include <debug.h>

struct ioat_dma_channel
{
    ioat_dma_chan_id_t id;           ///< unique channel id
    ioat_dma_chan_t channel;         ///< Mackerel address
    struct ioat_dma_device *dev;     ///< the DMA device this channel belongs to
    size_t max_xfer_size;            ///< maximum transfer size of this channel
    enum ioat_dma_chan_st state;     ///< channel state
    lpaddr_t last_completion;        ///<
    struct dma_mem completion;
    struct ioat_dma_ring *ring;      ///< Descriptor ring
    uint64_t status;                 ///< channel status
    uint8_t irq_vector;
    size_t irq_msix;
    struct ioat_dma_request *req_head;
    struct ioat_dma_request *req_tail;
};

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
errval_t ioat_channel_init(struct ioat_dma_device *dev,
                           uint8_t id,
                           uint32_t max_xfer,
                           struct ioat_dma_channel **ret_chan)
{
#if 0
    errval_t err;

    IOATCHAN_DEBUG("initialize channel with  max. xfer size of %u bytes\n", id,
                   max_xfer);

    struct ioat_dma_channel *chan = malloc(sizeof(*chan));
    if (chan == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }


    chan->id = ioat_dma_channel_build_id(ioat_dma_device_get_id(dev), id);
    chan->dev = dev;
    chan->max_xfer_size = max_xfer;

    mackerel_addr_t chan_base = ioat_device_get_mmio_base(dev);
    ioat_dma_chan_initialize(&chan->channel, chan_base + ((id + 1) * 0x80));

    ioat_dma_chan_dcactrl_target_cpu_wrf(&chan->channel,
                                         ioat_dma_chan_dca_ctr_target_any);

    err = ioat_dma_channel_reset(chan);
    if (err_is_fail(err)) {
        return err;
    }

    ioat_device_get_complsts_addr(dev, &chan->completion);


    /* write the completion address */
    ioat_dma_chan_cmpl_lo_wr(&chan->channel, chan->completion.paddr);
    ioat_dma_chan_cmpl_hi_wr(&chan->channel, chan->completion.paddr >> 32);

    err = ioat_dma_ring_alloc(IOAT_DMA_DESC_RING_SIZE, &chan->ring, chan);
    if (err_is_fail(err)) {
        dma_mem_free(&chan->completion);
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

    IOATCHAN_DEBUG("performing selftest on channel with NOP\n", chan->id);

    ioat_dma_request_nop(chan);
    err = ioat_dma_channel_submit_pending(chan);
    if (err_is_fail(err)) {
        dma_mem_free(&chan->completion);
        return err;
    }


    uint32_t j = 0xFFFF;
    do {
        thread_yield();
    } while (j-- && !ioat_dma_channel_is_active(chan)
             && !ioat_dma_channel_is_idle(chan));

    if (ioat_dma_channel_is_active(chan) || ioat_dma_channel_is_idle(chan)) {
        IOATCHAN_DEBUG("channel worked properly: %016lx\n", chan->id,
                     *(uint64_t*) chan->completion.addr);
        return SYS_ERR_OK;
    } else {
        uint32_t error = ioat_dma_chan_err_rd(&chan->channel);
        IOATCHAN_DEBUG(" channel error ERROR: %08x\n", chan->id, error);
        ioat_dma_mem_free(&chan->completion);
        return IOAT_ERR_CHAN_ERROR;
    }
#endif
    return SYS_ERR_OK;
}
