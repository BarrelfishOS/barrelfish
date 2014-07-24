/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>
#include <barrelfish/barrelfish.h>

#include <dev/ioat_dma_dev.h>

#include <dma_mem_utils.h>

#include <ioat/ioat_dma_internal.h>
#include <ioat/ioat_dma_dca_internal.h>
#include <ioat/ioat_dma_device_internal.h>
#include <ioat/ioat_dma_channel_internal.h>

#include <debug.h>

/**
 * IOAT DMA device representation
 */
struct ioat_dma_device
{
    struct dma_device common;

    ioat_dma_t device;                  ///< mackerel device base
    ioat_dma_cbver_t version;           ///< Crystal Beach version number

    struct dma_mem complstatus;         ///< memory region for channels CHANSTS

    uint32_t flags;
};

/// counter for device ID enumeration
static dma_dev_id_t device_id = 1;

/*
 * ----------------------------------------------------------------------------
 * device initialization functions
 * ----------------------------------------------------------------------------
 */

static errval_t device_init_ioat_v1(struct ioat_dma_device *dev)
{
    IOATDEV_DEBUG("devices of Crystal Beach Version 1.xx are currently not supported.\n",
                  dev->common.id);
    return DMA_ERR_DEVICE_UNSUPPORTED;
}

static errval_t device_init_ioat_v2(struct ioat_dma_device *dev)
{
    IOATDEV_DEBUG("devices of Crystal Beach Version 2.xx are currently not supported.\n",
                  dev->common.id);
    return DMA_ERR_DEVICE_UNSUPPORTED;
}

static errval_t device_init_ioat_v3(struct ioat_dma_device *dev)
{
    errval_t err;

    IOATDEV_DEBUG("initialize Crystal Beach 3 DMA device\n", dev->common.id);

    ioat_dma_dmacapability_t cap = ioat_dma_dmacapability_rd(&dev->device);

    if (ioat_dma_cbver_minor_extract(dev->version) == 2) {
        IOATDEV_DEBUG("disabling XOR and PQ opcodes for Crystal Beach 3.2\n",
                      dev->common.id);
        cap = ioat_dma_dmacapability_xor_insert(cap, 0x0);
        cap = ioat_dma_dmacapability_pq_insert(cap, 0x0);
    } else if (ioat_dma_cbver_minor_extract(dev->version) == 3) {
        IOATDEV_DEBUG("devices of Crystal Beach Version 3.3 are not supported.\n",
                      dev->common.id);
        return DMA_ERR_DEVICE_UNSUPPORTED;
    }

    /* if DCA is enabled, we cannot support the RAID functions */
    if (ioat_dma_dca_is_enabled()) {
        IOATDEV_DEBUG("Disabling XOR and PQ while DCA is enabled\n", dev->common.id);
        cap = ioat_dma_dmacapability_xor_insert(cap, 0x0);
        cap = ioat_dma_dmacapability_pq_insert(cap, 0x0);
    }

    if (ioat_dma_dmacapability_xor_extract(cap)) {
        IOATDEV_DEBUG("device supports XOR RAID.\n", dev->common.id);

        dev->flags |= IOAT_DMA_DEV_F_RAID;

        /*
         * this may need some additional functions to prepare
         * the specific transfers...
         *
         * max_xor = 8;
         * prepare_xor, prepare_xor_val
         */
    }

    if (ioat_dma_dmacapability_pq_extract(cap)) {
        IOATDEV_DEBUG("device supports PQ RAID.\n", dev->common.id);

        dev->flags |= IOAT_DMA_DEV_F_RAID;

        /*
         * this may need some additional functions to prepare the
         * DMA descriptors
         *
         * max_xor = 8;
         * max_pq = 8;
         * prepare_pq, perpare_pq_val
         *
         * also set the prepare_xor pointers...
         *
         */
    }

    /* set the interrupt type to disabled*/
    dev->common.irq_type = DMA_IRQ_DISABLED;
    dev->common.type = DMA_DEV_TYPE_IOAT;

    /* allocate memory for completion status writeback */
    err = dma_mem_alloc(IOAT_DMA_COMPLSTATUS_SIZE, IOAT_DMA_COMPLSTATUS_FLAGS,
                        &dev->complstatus);
    if (err_is_fail(err)) {
        return err;
    }

    dev->common.channels.count = ioat_dma_chancnt_num_rdf(&dev->device);

    dev->common.channels.c = calloc(dev->common.channels.count,
                                    sizeof(*dev->common.channels.c));
    if (dev->common.channels.c == NULL) {
        dma_mem_free(&dev->complstatus);
        return LIB_ERR_MALLOC_FAIL;
    }

    /* channel enumeration */

    IOATDEV_DEBUG("channel enumeration. discovered %u channels\n", dev->common.id,
                  dev->common.channels.count);

    uint32_t max_xfer_size = (1 << ioat_dma_xfercap_max_rdf(&dev->device));

    for (uint8_t i = 0; i < dev->common.channels.count; ++i) {
        struct dma_channel **chan = &dev->common.channels.c[i];
        err = ioat_dma_channel_init(dev, i, max_xfer_size,
                                    (struct ioat_dma_channel **) chan);
    }

    if (dev->flags & IOAT_DMA_DEV_F_DCA) {
        /*TODO: DCA initialization device->dca = ioat3_dca_init(pdev, device->reg_base);*/
    }

    return SYS_ERR_OK;
}

/*
 * ===========================================================================
 * Library Internal Interface
 * ===========================================================================
 */

void ioat_dma_device_get_complsts_addr(struct ioat_dma_device *dev,
                                       struct dma_mem *mem)
{
    if (dev->common.state != DMA_DEV_ST_CHAN_ENUM) {
        memset(mem, 0, sizeof(*mem));
    }

    assert(dev->complstatus.vaddr);

    *mem = dev->complstatus;
    mem->bytes = IOAT_DMA_COMPLSTATUS_SIZE;
    mem->paddr += (IOAT_DMA_COMPLSTATUS_SIZE * dev->common.channels.next);
    mem->frame = NULL_CAP
    ;
    mem->vaddr += (IOAT_DMA_COMPLSTATUS_SIZE * dev->common.channels.next++);

}

/**
 * \brief globally enables the interrupts for the given device
 *
 * \param dev   IOAT DMA device
 * \param type  the interrupt type to enable
 */
errval_t ioat_dma_device_irq_setup(struct ioat_dma_device *dev,
                                   dma_irq_t type)
{
    ioat_dma_intrctrl_t intcrtl = 0;
    intcrtl = ioat_dma_intrctrl_intp_en_insert(intcrtl, 1);

    dev->common.irq_type = type;
    switch (type) {
        case DMA_IRQ_MSIX:
            IOATDEV_DEBUG("Initializing MSI-X interrupts \n", dev->common.id);
            assert(!"NYI");
            break;
        case DMA_IRQ_MSI:
            IOATDEV_DEBUG("Initializing MSI interrupts \n", dev->common.id);
            assert(!"NYI");
            break;
        case DMA_IRQ_INTX:
            IOATDEV_DEBUG("Initializing INTx interrupts \n", dev->common.id);
            assert(!"NYI");
            break;
        default:
            /* disabled */
            intcrtl = 0;
            IOATDEV_DEBUG("Disabling interrupts \n", dev->common.id);
            break;
    }

    ioat_dma_intrctrl_wr(&dev->device, intcrtl);

    return SYS_ERR_OK;
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
 * \brief initializes a IOAT DMA device with the giving capability
 *
 * \param mmio capability representing the device's MMIO registers
 * \param dev  returns a pointer to the device structure
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_dma_device_init(struct capref mmio,
                              struct ioat_dma_device **dev)
{
    errval_t err;

    struct ioat_dma_device *ioat_device = calloc(1, sizeof(*ioat_device));
    if (ioat_device == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct dma_device *dma_dev = &ioat_device->common;

    struct frame_identity mmio_id;
    err = invoke_frame_identify(mmio, &mmio_id);
    if (err_is_fail(err)) {
        free(ioat_device);
        return err;
    }

    dma_dev->id = device_id++;
    dma_dev->mmio.paddr = mmio_id.base;
    dma_dev->mmio.bytes = (1UL << mmio_id.bits);
    dma_dev->mmio.frame = mmio;

    IOATDEV_DEBUG("init device with mmio range: {paddr=0x%016lx, size=%u kB}\n",
                  dma_dev->id, mmio_id.base, 1 << mmio_id.bits);

    err = vspace_map_one_frame_attr((void**) &dma_dev->mmio.vaddr,
                                    dma_dev->mmio.bytes, dma_dev->mmio.frame,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL,
                                    NULL);
    if (err_is_fail(err)) {
        free(ioat_device);
        return err;
    }

    ioat_dma_initialize(&ioat_device->device, NULL, (void *) dma_dev->mmio.vaddr);

    ioat_device->version = ioat_dma_cbver_rd(&ioat_device->device);

    IOATDEV_DEBUG("device registers mapped at 0x%016lx. IOAT version: %u.%u\n",
                  dma_dev->id, dma_dev->mmio.vaddr,
                  ioat_dma_cbver_major_extract(ioat_device->version),
                  ioat_dma_cbver_minor_extract(ioat_device->version));

    switch (ioat_dma_cbver_major_extract(ioat_device->version)) {
        case ioat_dma_cbver_1x:
            err = device_init_ioat_v1(ioat_device);
            break;
        case ioat_dma_cbver_2x:
            err = device_init_ioat_v2(ioat_device);
            break;
        case ioat_dma_cbver_3x:
            err = device_init_ioat_v3(ioat_device);
            break;
        default:
            err = DMA_ERR_DEVICE_UNSUPPORTED;
    }

    if (err_is_fail(err)) {
        vspace_unmap((void*) dma_dev->mmio.vaddr);
        free(ioat_device);
        return err;
    }

    dma_dev->f.deregister_memory = NULL;
    dma_dev->f.register_memory = NULL;
    dma_dev->f.poll = ioat_dma_device_poll_channels;

    *dev = ioat_device;

    return err;
}

/**
 * \brief terminates the device operation and frees up the allocated resources
 *
 * \param dev IOAT DMA device to shutdown
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_dma_device_shutdown(struct ioat_dma_device *dev)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/**
 * \brief requests access to a IOAT DMA device from the IOAT device manager
 *        and initializes the device.
 *
 * \param dev  returns a pointer to the device structure
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_dma_device_acquire(struct ioat_dma_device **dev)
{
    errval_t err;

    struct ioat_dma_device *ioat_device = calloc(1, sizeof(*ioat_device));
    if (ioat_device == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    assert(!"NYI");
    err = SYS_ERR_OK;
    return err;
}

/**
 * \brief terminates the device operation and frees up the allocated resources
 *        and releases the device and returns it to the IOAT device manager.
 *
 * \param dev IOAT DMA device to be released
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_dma_device_release(struct ioat_dma_device *dev)
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
 * \param dev   IOAT DMA device
 * \param type  interrupt type
 * \param fn    interrupt handler function
 * \param arg   argument supplied to the handler function
 */
errval_t ioat_dma_device_intr_enable(struct ioat_dma_device *dev,
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
 * \param dev   IOAT DMA device
 */
void ioat_dma_device_intr_disable(struct ioat_dma_device *dev)
{
    assert(!"NYI");
}

/**
 * \brief sets the interrupt delay for the device
 *
 * \param dev   IOAT DMA device
 * \param usec  interrupt delay in microseconds
 */
void ioat_dma_device_set_intr_delay(struct ioat_dma_device *dev,
                                    uint16_t usec)
{
    ioat_dma_intrdelay_delay_us_wrf(&dev->device, usec);
}

/*
 * ----------------------------------------------------------------------------
 * Device Operation Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief polls the channels of the IOAT DMA device
 *
 * \param dev   IOAT DMA device
 *
 * \returns SYS_ERR_OK on success
 *          DMA_ERR_DEVICE_IDLE if there is nothing completed on the channels
 *          errval on error
 */
errval_t ioat_dma_device_poll_channels(struct dma_device *dev)
{
    errval_t err;

    uint8_t idle = 0x1;

    for (uint8_t i = 0; i < dev->channels.count; ++i) {
        err = ioat_dma_channel_poll(dev->channels.c[i]);
        switch (err_no(err)) {
            case DMA_ERR_CHAN_IDLE:
                idle = idle && 0x1;
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

