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
    ioat_dma_chan_st_t state;        ///< channel state
    lpaddr_t last_completion;        ///<
    struct dma_mem completion;
    struct ioat_dma_ring *ring;      ///< Descriptor ring
    uint64_t status;                 ///< channel status
    uint8_t irq_vector;
    size_t irq_msix;
    struct ioat_dma_request *req_head;
    struct ioat_dma_request *req_tail;
};

/*
 * ============================================================================
 * Public Internal Interface
 * ============================================================================
 */

/**
 * \brief Resets a IOAT DMA channel
 *
 * \param chan  IOAT DMA channel to be reset
 *
 * \returns SYS_ERR_OK on success
 *          IOAT_ERR_CHAN_RESET on reset timeout
 */
errval_t ioat_dma_channel_reset(struct ioat_dma_channel *chan)
{
    IOATCHAN_DEBUG("reset channel.\n", chan->id);

    if (chan->state == IOAT_DMA_CHAN_ST_ERROR) {
        ioat_dma_chan_err_t chanerr = ioat_dma_chan_err_rd(&chan->channel);
        ioat_dma_chan_err_wr(&chan->channel, chanerr);
        IOATCHAN_DEBUG("Reseting channel from error state: [%08x]\n", chan->id,
                       chanerr);

        /*
         * TODO: clear the ioat_dma_pci_chanerr register in PCI config space
         *       (same approach as above)
         *       -> How to access this ?
         */
    }
    chan->state = IOAT_DMA_CHAN_ST_RESETTING;

    /* perform reset */
    ioat_dma_chan_cmd_reset_wrf(&chan->channel, 0x1);

    uint16_t reset_counter = 0xFFF;
    do {
        if (!ioat_dma_chan_cmd_reset_rdf(&chan->channel)) {
            break;
        }
        thread_yield();
    } while(reset_counter--);

    if (ioat_dma_chan_cmd_reset_rdf(&chan->channel)) {
        /* reset failed */
        return IOAT_ERR_RESET_TIMEOUT;
    }

    /* XXX: Intel BD architecture will need some additional work here */

    chan->state = IOAT_DMA_CHAN_ST_UNINITIALEZED;

    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Getter / Setter Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the IOAT DMA channel ID
 *
 * \param chan  IOAT DMA channel
 *
 * \returns IOAT DMA channel ID of the supplied channel
 */
inline ioat_dma_chan_id_t ioat_dma_channel_get_id(struct ioat_dma_channel *chan)
{
    return chan->id;
}

/**
 * \brief returns the associated IOAT DMA descriptor ring of a channel
 *
 * \param chan  IOAT DMA channel
 *
 * \returns IOAT DMA descriptor ring handle
 */
inline struct ioat_dma_ring *ioat_dma_channel_get_ring(struct ioat_dma_channel *chan)
{
    return chan->ring;
}

/**
 * \brief returns the maximum number of bytes per DMA descritpor
 *
 * \param chan IOAT DMA channel
 *
 * \returns maximum number of bytes
 */
inline uint32_t ioat_dma_channel_get_max_xfer_size(struct ioat_dma_channel *chan)
{
    return chan->max_xfer_size;
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
errval_t ioat_channel_init(struct ioat_dma_device *dev,
                           uint8_t id,
                           uint32_t max_xfer,
                           struct ioat_dma_channel **ret_chan)
{

    errval_t err;

    IOATCHAN_DEBUG("initialize channel with  max. xfer size of %u bytes\n", id,
                   max_xfer);

    struct ioat_dma_channel *chan = calloc(1, sizeof(*chan));
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
#if 0
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
    }while (j-- && !ioat_dma_channel_is_active(chan)
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
