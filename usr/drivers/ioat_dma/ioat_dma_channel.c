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

#include <barrelfish/barrelfish.h>
#include <pci/pci.h>

#include <dev/ioat_dma_chan_dev.h>

#include "ioat_dma.h"
#include "ioat_dma_device.h"
#include "ioat_dma_channel.h"

#include "debug.h"



struct ioat_dma_channel
{
    struct ioat_dma_device *dev;
    ioat_dma_chan_id_t chan_id;
    ioat_dma_chan_t channel;
    uint8_t irq_vector;
    size_t irq_msix;
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
        mackerel_addr_t chan_base = (mackerel_addr_t) dev->mmio.vbase;
        ioat_dma_chan_initialize(&chan->channel, chan_base + ((i + 1) * 0x80));

        ioat_dma_chan_dcactrl_target_cpu_wrf(&chan->channel,
        ioat_dma_chan_dca_ctr_target_any);

        err = ioat_dma_channel_reset(chan);
        if (err_is_fail(err)) {
            return err;
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

errval_t ioat_dma_channel_submit(struct ioat_dma_channel *chan,
                                 struct ioat_dma_request *req)
{
    IOCHAN_DEBUG("Submitting request [0x%016lx] to channel 0x%04x\n",
                 (lvaddr_t)req, chan->chan_id);


    return SYS_ERR_OK;
}

