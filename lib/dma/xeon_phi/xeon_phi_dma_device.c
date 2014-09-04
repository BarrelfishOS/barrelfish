/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>

#include <dev/xeon_phi/xeon_phi_dma_dev.h>

#include <dma_mem_utils.h>

#include <xeon_phi/xeon_phi_dma_internal.h>
#include <xeon_phi/xeon_phi_dma_device_internal.h>
#include <xeon_phi/xeon_phi_dma_channel_internal.h>

#include <debug.h>

/**
 * Xeon Phi DMA device representation
 */
struct xeon_phi_dma_device
{
    struct dma_device common;

    xeon_phi_dma_t device;          ///< mackerel device base
    struct dma_mem dstat;     ///< memory region for channels dstat_wb
    uint32_t flags;
};

/// counter for device ID enumeration
static dma_dev_id_t device_id = 1;

/*
 * ===========================================================================
 * Library Internal Interface
 * ===========================================================================
 */

/**
 * \brief fills in the memory information structure for the channel's dstat
 *        address
 *
 * \param dev   Xeon Phi DMA device
 * \param mem   Memory structure to fill in
 */
void xeon_phi_dma_device_get_dstat_addr(struct xeon_phi_dma_device *dev,
                                        struct dma_mem *mem)
{
    assert(dev->dstat.vaddr);

    *mem = dev->dstat;
    mem->bytes = XEON_PHI_DMA_CHANNEL_DSTAT_SIZE;
    mem->paddr += (XEON_PHI_DMA_CHANNEL_DSTAT_SIZE * dev->common.channels.next);
    mem->frame = NULL_CAP
    ;
    mem->vaddr += (XEON_PHI_DMA_CHANNEL_DSTAT_SIZE * dev->common.channels.next++);
}

/**
 * \brief globally enables the interrupts for the given device
 *
 * \param dev   Xeon Phi DMA device
 * \param type  the interrupt type to enable
 */
errval_t xeon_phi_dma_device_irq_setup(struct xeon_phi_dma_device *dev,
                                       dma_irq_t type)
{
    assert(!"NYI");

    return SYS_ERR_OK;
}

/**
 * \brief gets the Xeon Phi virtual base address of the DMA channel with
 *        the given id
 *
 * \param dev   Xeon Phi DMA device
 * \param idx   DMA channel index
 *
 * \returns virtual address of MMIO registers for the channel
 */
void *xeon_phi_dma_device_get_channel_vbase(struct xeon_phi_dma_device *dev,
                                            uint8_t idx)
{
    XPHIDEV_DEBUG("getting channel vbase for %u, offset=%x\n", dev->common.id,
                  idx, (idx * 0x40) + XEON_PHI_DMA_OFFSET);
    return (void *) (dev->common.mmio.vaddr + (idx * 0x40) + XEON_PHI_DMA_OFFSET);
}

/**
 * \brief sets the channel owner register of the Xeon Phi DMA device
 *
 * \param dev   Xeon Phi DMA device
 * \param idx   channel index
 * \param owner owner of the channel
 */
void xeon_phi_dma_device_set_channel_owner(struct xeon_phi_dma_device *dev,
                                           uint8_t idx,
                                           xeon_phi_dma_owner_t owner)
{
    uint8_t owner_val;
    if (owner == XEON_PHI_DMA_OWNER_CARD) {
        XPHIDEV_DEBUG("settings owner of channel [%u] to card.\n", dev->common.id, idx);
        owner_val = 0;
    } else {
        XPHIDEV_DEBUG("settings owner of channel [%u] to host.\n", dev->common.id, idx);
        owner_val = 1;
    }

    xeon_phi_dma_dcr_t dcr = xeon_phi_dma_dcr_rd(&dev->device);

    switch (idx) {
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

    xeon_phi_dma_dcr_wr(&dev->device, dcr);
}

/**
 * \brief Enables / Disables the Xeon Phi DMA channel
 *
 * \param chan      Xeon Phi DMA channel
 * \param idx       channel index
 * \param enabled   flag to set the channel enabled
 */
void xeon_phi_dma_device_set_channel_state(struct xeon_phi_dma_device *dev,
                                           uint8_t idx,
                                           uint8_t enabled)
{
    uint8_t id = idx - XEON_PHI_DMA_DEVICE_CHAN_OFFSET;

    xeon_phi_dma_dcr_t dcr = xeon_phi_dma_dcr_rd(&dev->device);

    uint8_t enabled_val;
    if (enabled) {
        XPHIDEV_DEBUG("Enabling channel. [%u]\n", dev->common.id, idx);
        if (dev->common.channels.c[id]) {
            dev->common.channels.c[id]->state = DMA_CHAN_ST_RUNNING;
        }
        enabled_val = 0x1;
    } else {
        XPHIDEV_DEBUG("Disabling channel. [%u]\n", dev->common.id, idx);
        if (dev->common.channels.c[id]) {
            dev->common.channels.c[id]->state = DMA_CHAN_ST_SUSPENDED;
        }
        enabled_val = 0x0;
    }

    switch (idx) {
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

    xeon_phi_dma_dcr_wr(&dev->device, dcr);
}

/*
 * ===========================================================================
 * Public Interface
 * ===========================================================================
 */

/*
 * ----------------------------------------------------------------------------
 * device initialization / termination
 * ----------------------------------------------------------------------------
 */

/**
 * \brief initializes a Xeon Phi DMA device with the giving capability
 *
 * \param mmio capability representing the device's MMIO registers
 * \param dev  returns a pointer to the device structure
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xeon_phi_dma_device_init(void *mmio_base,
                                  struct xeon_phi_dma_device **dev)
{
    errval_t err;

    struct xeon_phi_dma_device *xdev = calloc(1, sizeof(*xdev));
    if (xdev == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

#if DMA_BENCH_ENABLED
     bench_init();
#endif

    struct dma_device *dma_dev = (struct dma_device *) xdev;

    XPHIDEV_DEBUG("initializing Xeon Phi DMA device @ %p\n", device_id,
                  mmio_base);

    err = dma_mem_alloc(XEON_PHI_DMA_DEVICE_DSTAT_SIZE,
                        XEON_PHI_DMA_DEVICE_DSTAT_FLAGS,
                        &xdev->dstat);
    if (err_is_fail(err)) {
        free(xdev);
        return err;
    }

    dma_dev->id = device_id++;
    dma_dev->irq_type = DMA_IRQ_DISABLED;
    dma_dev->type = DMA_DEV_TYPE_XEON_PHI;
    dma_dev->mmio.vaddr = (lvaddr_t) mmio_base;
    dma_dev->f.poll = xeon_phi_dma_device_poll_channels;

    xeon_phi_dma_initialize(&xdev->device, mmio_base);

    XPHIDEV_DEBUG("initializing %u channels\n", device_id,
                  XEON_PHI_DMA_DEVICE_CHANNELS);

    dma_dev->channels.count = XEON_PHI_DMA_DEVICE_CHANNELS;
    dma_dev->channels.c = calloc(XEON_PHI_DMA_DEVICE_CHANNELS,
                                 sizeof(void *));

    if (dma_dev->channels.c == NULL) {
        device_id--;
        free(xdev);
        return LIB_ERR_MALLOC_FAIL;
    }

    for (uint8_t i = 0; i < XEON_PHI_DMA_DEVICE_CHANNELS; ++i) {
        struct dma_channel **chan = &dma_dev->channels.c[i];
        err = xeon_phi_dma_channel_init(xdev, i, XEON_PHI_DMA_DEVICE_MAX_XFER,
                                        (struct xeon_phi_dma_channel **) chan);
        if (err_is_fail(err)) {
            free(dma_dev->channels.c);
            device_id--;
            free(xdev);
            return err;
        }
    }

    *dev = xdev;

    XPHIDEV_DEBUG("Xeon Phi DMA device initialized\n", dma_dev->id);

    return err;
}

/**
 * \brief terminates the device operation and frees up the allocated resources
 *
 * \param dev Xeon Phi DMA device to shutdown
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xeon_phi_dma_device_shutdown(struct xeon_phi_dma_device *dev)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Interrupt management
 * ----------------------------------------------------------------------------
 */

/**
 * \brief enables the interrupts for the device
 *
 * \param dev   Xeon Phi DMA device
 * \param type  interrupt type
 * \param fn    interrupt handler function
 * \param arg   argument supplied to the handler function
 */
errval_t xeon_phi_dma_device_intr_enable(struct xeon_phi_dma_device *dev,
                                         dma_irq_t type,
                                         dma_irq_fn_t fn,
                                         void *arg)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/**
 * \brief disables the interrupts for the device
 *
 * \param dev   Xeon Phi DMA device
 */
void xeon_phi_dma_device_intr_disable(struct xeon_phi_dma_device *dev)
{
    assert(!"NYI");
}

/*
 * ----------------------------------------------------------------------------
 * Device Operation Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief polls the channels of the Xeon Phi DMA device
 *
 * \param dev   Xeon Phi DMA device
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_DEVICE_IDLE if there is nothing completed on the channels
 *          errval on error
 */
errval_t xeon_phi_dma_device_poll_channels(struct dma_device *dev)
{
    errval_t err;

    uint8_t idle = 0x1;

    for (uint8_t i = 0; i < dev->channels.count; ++i) {
        err = xeon_phi_dma_channel_poll(dev->channels.c[i]);
        switch (err_no(err)) {
            case DMA_ERR_CHAN_IDLE:
                /* no op */
                break;
            case SYS_ERR_OK:
                idle = 0;
                break;
            default:
                return err;
        }
    }

    if (idle) {
        return DMA_ERR_DEVICE_IDLE;
    }

    return SYS_ERR_OK;
}

