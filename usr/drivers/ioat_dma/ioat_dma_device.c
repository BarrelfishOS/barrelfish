/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <pci/pci.h>
#include <pci/devids.h>

#include "ioat_dma.h"
#include "ioat_dma_device.h"
#include "ioat_dma_channel.h"

#include "debug.h"

static struct ioat_dma_device *curr_dev;

static errval_t curr_err;

static void ioat_dma_do_interrupt(void *arg)
{

}

static errval_t device_setup_interrupts(struct ioat_dma_device *dev)
{
    errval_t err;

    dev->irq_type = IOAT_DMA_IRQ_TYPE;
    switch (dev->irq_type) {
        case IOAT_DMA_IRQ_MSIX:
            ;
            uint16_t msi_count;
            err = pci_msix_enable(&msi_count);

            IODEV_DEBUG("Initializing %u MSI-X Vectors\n", msi_count);

            for (uint16_t i = 0; i < msi_count; ++i) {
                if (i == dev->chan_num) {
                    break;
                }
                struct ioat_dma_channel *chan = dev->channels;
                err = ioat_dma_channel_irq_setup_msix(chan);
                if (err_is_fail(err)) {
                    return err;
                }

            }
            break;
        case IOAT_DMA_IRQ_MSI:
            break;
        case IOAT_DMA_IRQ_INTX:
            break;
        default:
            /* disabled */
            break;
    }

    return SYS_ERR_OK;
}

static errval_t device_init_ioat_v1(struct ioat_dma_device *dev)
{
    IODEV_DEBUG("Devices of Crystal Beach Version 1.xx are not supported.\n");
    return IOAT_ERR_DEVICE_UNSUPPORTED;
}

static errval_t device_init_ioat_v2(struct ioat_dma_device *dev)
{
    IODEV_DEBUG("Devices of Crystal Beach Version 2.xx are not supported.\n");
    return IOAT_ERR_DEVICE_UNSUPPORTED;
}

static errval_t device_init_ioat_v3(struct ioat_dma_device *dev)
{
    errval_t err;

    IODEV_DEBUG("Initializing Crystal Beach 3 DMA Device.\n");

    ioat_dma_dmacapability_t cap = ioat_dma_dmacapability_rd(&dev->device);

    debug_printf("cap - %x\n", cap);

    if (ioat_dma_cbver_minor_extract(dev->version) == 2) {
        IODEV_DEBUG("Disabling XOR and PQ.\n");
        cap = ioat_dma_dmacapability_xor_insert(cap, 0x0);
        cap = ioat_dma_dmacapability_pq_insert(cap, 0x0);
    } else if (ioat_dma_cbver_minor_extract(dev->version) == 3) {
        IODEV_DEBUG("Devices of Crystal Beach Version 3.3 are not supported.\n");
        return IOAT_ERR_DEVICE_UNSUPPORTED;
    }

    /* if DCA is enabled, we cannot support the RAID functions */
    if (dev->flags & IOAT_DMA_DEV_F_DCA) {
        IODEV_DEBUG("Disabling XOR and PQ.\n");
        cap = ioat_dma_dmacapability_xor_insert(cap, 0x0);
        cap = ioat_dma_dmacapability_pq_insert(cap, 0x0);
    }

    if (ioat_dma_dmacapability_xor_extract(cap)) {
        IODEV_DEBUG("Device supports XOR RAID.\n");

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
        IODEV_DEBUG("Device supports PQ RAID.\n");

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

    /* set the interrupt type */
    dev->irq_type = IOAT_DMA_IRQ_TYPE;

    /* channel enumeration */
    dev->chan_num = ioat_dma_chancnt_num_rdf(&dev->device);
    dev->xfer_size_max = (1UL << ioat_dma_xfercap_max_rdf(&dev->device));

    err = ioat_dma_channel_init(dev);
    if (err_is_fail(err)) {
        return err;
    }

    err = device_setup_interrupts(dev);
    if (err_is_fail(err)) {
        return err;
    }


#if 0

    err = device->self_test(device);

    ioat_set_tcp_copy_break(262144);

    err = ioat_register(device);
    if (err)
    return err;

    ioat_kobject_add(device, &ioat2_ktype);

    if (dca)
    device->dca = ioat3_dca_init(pdev, device->reg_base);
#endif

    return SYS_ERR_OK;
}

static void device_init(struct device_mem* bar_info,
                        int bar_count)
{
    assert(curr_dev);

    errval_t err;

    if (bar_count != IOAT_DMA_BAR_COUNT) {
        curr_dev->state = IOAT_DMA_DEV_ST_ERR;
        curr_err = IOAT_ERR_DEVICE_UNSUPPORTED;
        return;
    }

    IODEV_DEBUG("IOAT DMA device BAR[0]: {paddr=0x%016lx, size=%u kB}\n",
                bar_info[0].paddr,
                (uint32_t ) (bar_info[0].bytes / 1024));

    err = map_device(bar_info);
    if (err_is_fail(err)) {
        curr_err = err;
        curr_dev->state = IOAT_DMA_DEV_ST_ERR;
    }

    curr_dev->mmio.bits = bar_info->bits;
    curr_dev->mmio.vbase = bar_info->vaddr;
    curr_dev->mmio.pbase = bar_info->paddr;
    curr_dev->mmio.bytes = bar_info->bytes;
    curr_dev->mmio.cap = bar_info->frame_cap[0];

    ioat_dma_initialize(&curr_dev->device, NULL, bar_info->vaddr);

    curr_dev->version = ioat_dma_cbver_rd(&curr_dev->device);

    IODEV_DEBUG("IOAT DMA device version: %u.%u\n",
                ioat_dma_cbver_major_extract(curr_dev->version),
                ioat_dma_cbver_minor_extract(curr_dev->version));

    switch (ioat_dma_cbver_major_extract(curr_dev->version)) {
        case ioat_dma_cbver_1x:
            curr_err = device_init_ioat_v1(curr_dev);
            break;
        case ioat_dma_cbver_2x:
            curr_err = device_init_ioat_v2(curr_dev);
            break;
        case ioat_dma_cbver_3x:
            curr_err = device_init_ioat_v3(curr_dev);
            break;
        default:
            curr_err = IOAT_ERR_DEVICE_UNSUPPORTED;
    }

    if (err_is_fail(err)) {
        curr_dev->state = IOAT_DMA_DEV_ST_ERR;
        return;
    }

}

errval_t ioat_dma_device_init(struct pci_addr addr,
                              uint16_t device_id,
                              struct ioat_dma_device *dev)
{
    errval_t err;

    assert(dev);
    assert(curr_dev == NULL);

    if (dev->state != IOAT_DMA_DEV_ST_UNINITIALIZED) {
        return SYS_ERR_OK;
    }

    curr_dev = dev;
    curr_err = SYS_ERR_OK;

    err = pci_register_driver_irq(device_init,
                                  PCI_DONT_CARE,
                                  PCI_DONT_CARE,
                                  PCI_DONT_CARE,
                                  PCI_VENDOR_INTEL,
                                  device_id,
                                  addr.bus,
                                  addr.device,
                                  addr.function,
                                  ioat_dma_do_interrupt,
                                  dev);
    if (err_is_fail(err)) {
        dev->state = IOAT_DMA_DEV_ST_ERR;
        return err;
    }
    curr_dev = NULL;
    return curr_err;
}

/**
 *
 */
errval_t ioat_dma_device_discovery(struct pci_addr addr,
                                   uint16_t device_id,
                                   struct ioat_dma_ctrl *ctrl)
{
    errval_t err;

    uint16_t *dev_ids = NULL;
    uint16_t dev_cnt = 0;

    err = pci_client_connect();
    if (err_is_fail(err)) {
        return err;
    }

    /* there are several possible devices which implement Intel IOAT DMA */
    switch (device_id & 0xFFF0) {
        case PCI_DEVICE_IOAT_IVB0:
            IODEV_DEBUG("doing device discovery: Ivy Bridge\n");
            /*
             * Ivy Bridge Platform
             */
            if (addr.device != 4) {
                /* the IOAT DMA engine should be on device 4 on ivy bridge */
                /* TODO: error number */
                return IOAT_ERR_PCI_ADDRESS;
            }
            dev_ids = calloc(PCI_DEVICE_IOAT_IVB_CNT, sizeof(uint16_t));
            if (dev_ids == NULL) {
                return LIB_ERR_MALLOC_FAIL;
            }
            dev_ids[0] = PCI_DEVICE_IOAT_IVB0;
            dev_ids[1] = PCI_DEVICE_IOAT_IVB1;
            dev_ids[2] = PCI_DEVICE_IOAT_IVB2;
            dev_ids[3] = PCI_DEVICE_IOAT_IVB3;
            dev_ids[4] = PCI_DEVICE_IOAT_IVB4;
            dev_ids[5] = PCI_DEVICE_IOAT_IVB5;
            dev_ids[6] = PCI_DEVICE_IOAT_IVB6;
            dev_ids[7] = PCI_DEVICE_IOAT_IVB7;
            dev_ids[8] = PCI_DEVICE_IOAT_IVB8;
            dev_ids[9] = PCI_DEVICE_IOAT_IVB9;
            dev_cnt = PCI_DEVICE_IOAT_IVB_CNT;
            addr.function = 0;
            break;

        case PCI_DEVICE_IOAT_HSW0:
            IODEV_DEBUG("doing device discovery: Haswell\n");
            /*
             * Haswell Platform
             */
            if (addr.device != 4) {
                /* the IOAT DMA engine should be on device 4 on ivy bridge */
                return IOAT_ERR_PCI_ADDRESS;
            }
            dev_ids = calloc(PCI_DEVICE_IOAT_HSW_CNT, sizeof(uint16_t));
            if (dev_ids == NULL) {
                return LIB_ERR_MALLOC_FAIL;
            }
            dev_ids[0] = PCI_DEVICE_IOAT_HSW0;
            dev_ids[1] = PCI_DEVICE_IOAT_HSW1;
            dev_ids[2] = PCI_DEVICE_IOAT_HSW2;
            dev_ids[3] = PCI_DEVICE_IOAT_HSW3;
            dev_ids[4] = PCI_DEVICE_IOAT_HSW4;
            dev_ids[5] = PCI_DEVICE_IOAT_HSW5;
            dev_ids[6] = PCI_DEVICE_IOAT_HSW6;
            dev_ids[7] = PCI_DEVICE_IOAT_HSW7;
            dev_ids[8] = PCI_DEVICE_IOAT_HSW8;
            dev_ids[9] = PCI_DEVICE_IOAT_HSW9;
            dev_cnt = PCI_DEVICE_IOAT_HSW_CNT;
            addr.function = 0;
            break;
        default:
            return IOAT_ERR_DEVICE_UNSUPPORTED;
    }

    struct ioat_dma_device *dev = calloc(dev_cnt, sizeof(struct ioat_dma_device));
    if (dev == NULL) {
        free(dev_ids);
        return LIB_ERR_MALLOC_FAIL;
    }

    for (uint16_t i = 0; i < dev_cnt; ++i) {
        dev[i].dma_ctrl = ctrl;
        if (ctrl->dca_enabled) {
            dev[i].flags = IOAT_DMA_DEV_F_DCA;
        }
        err = ioat_dma_device_init(addr, dev_ids[i], dev + i);
        if (err_is_fail(err)) {
            /*
             * there may be less than the maximum possible number of devices
             * present in the system
             */
            if (i == 0) {
                free(dev_ids);
                return err;
            }

            dev_cnt = i + 1;
            break;
        }

        addr.function++;
    }

    IODEV_DEBUG("Device discovery done: got %u devices.\n", dev_cnt);

    ctrl->devices = dev;
    ctrl->device_num = dev_cnt;

    free(dev_ids);

    return SYS_ERR_OK;
}

