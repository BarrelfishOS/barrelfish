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

#include <dma/dma.h>
#include <dma/dma_device.h>
#include <dma/ioat/ioat_dma.h>
#include <dma/ioat/ioat_dma_device.h>

#include "device.h"
#include "ioat_mgr_service.h"
#include "debug.h"

static uint8_t device_count = 0;
struct ioat_dma_device **devices;
static uint8_t device_next = 0;
static struct pci_address pci_addr;

static void handle_device_interrupt(void *arg)
{

//    struct ioat_dma_device *dev = *((struct ioat_dma_device **) arg);
//    struct dma_device *dma_dev = (struct dma_device *) dev;

    INTR_DEBUG("interrupt! device %u", dma_device_get_id(arg));

}

static void pci_dev_init_service(struct device_mem *bar_info,
                                 int nr_mapped_bars)
{
    errval_t err;

    DEV_DEBUG("Initialize device @ [%016lx] with %u bars\n", bar_info->paddr,
              nr_mapped_bars);

    if (nr_mapped_bars != 1) {
        DEV_ERR("number of mapped bars is wrong. Skipping initialization\n");
        return;
    }

    /* initialize the device */
    err = ioat_dma_device_init(*bar_info->frame_cap, &pci_addr,
                               &devices[device_count]);
    if (err_is_fail(err)) {
        DEV_ERR("Could not initialize the device: %s\n", err_getstring(err));
        return;
    }

    device_count++;
}

static void pci_dev_init_manager(struct device_mem *bar_info,
                                 int nr_mapped_bars)
{
    errval_t err;

    DEV_DEBUG("Initialize device @ [%016lx] with %u bars\n", bar_info->paddr,
              nr_mapped_bars);

    if (nr_mapped_bars != 1) {
        DEV_ERR("number of mapped bars is wrong. Skipping initialization\n");
        return;
    }

    err = ioat_mgr_svc_add_device(bar_info->frame_cap);
    if (err_is_fail(err)) {
        DEV_ERR("Device coult not be added to the manager: %s\n",
                err_getstring(err));
    }
}

errval_t ioat_device_discovery(struct pci_addr addr,
                               enum device_type devtype,
                               uint8_t is_dev_mgr)
{

    errval_t err;

    uint16_t *dev_ids = NULL;
    uint16_t dev_cnt = 0;

    err = pci_client_connect();
    if (err_is_fail(err)) {
        return err;
    }

    switch (devtype) {
        case IOAT_DEVICE_IVB:
            DEV_DEBUG("Doing device discovery: Ivy Bridge\n");
            if (addr.device != 4) {
                /* the IOAT DMA engine should be on device 4 on Ivy Bridge */
                return DMA_ERR_PCI_ADDRESS;
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
        case IOAT_DEVICE_HSW:
            DEV_DEBUG("Doing device discovery: Haswell\n");
            if (addr.device != 4) {
                /* the IOAT DMA engine should be on device 4 on Haswell */
                return DMA_ERR_PCI_ADDRESS;
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
            break;
        default:
            return DMA_ERR_DEVICE_UNSUPPORTED;
            break;
    }

    devices = calloc(dev_cnt, sizeof(struct ioat_dma_device *));
    if (devices == NULL) {
        free(dev_ids);
        return LIB_ERR_MALLOC_FAIL;
    }

    if (is_dev_mgr == IOAT_DMA_OPERATION_LIBRARY) {
        err = ioat_mgr_svc_init();
        if (err_is_fail(err)) {
            return err;
        }
    }

    /**
     * enumerating all the devices
     *
     * The devices on Haswell and Ivy Bridge are located on
     * Bus x, Device 4, Function 0..7
     */
    for (uint8_t i = 0; i < dev_cnt; ++i) {
        pci_addr.bus = addr.bus;
        pci_addr.device = addr.device;
        pci_addr.function = i;

        if (is_dev_mgr == IOAT_DMA_OPERATION_LIBRARY) {
            /*
             * discover devices as manager i.e. don't initialize them as they
             * are handed over to the domains upon request
             */
            err = pci_register_driver_noirq(pci_dev_init_manager, PCI_DONT_CARE,
                                            PCI_DONT_CARE, PCI_DONT_CARE,
                                            PCI_VENDOR_INTEL, dev_ids[i], addr.bus,
                                            addr.device, addr.function + i);
        } else {
            /*
             * discover devices as a service i.e. initialize and map devices
             */
            err = pci_register_driver_irq(pci_dev_init_service, PCI_DONT_CARE,
                                          PCI_DONT_CARE, PCI_DONT_CARE,
                                          PCI_VENDOR_INTEL,
                                          dev_ids[i], addr.bus, addr.device,
                                          addr.function + i, handle_device_interrupt,
                                          devices[i]);
        }
        if (err_is_fail(err)) {
            if (i == 0) {
                /* XXX: if a system does not implement all devices listed above,
                 *      the registration will fail which is ok. However if it
                 *      happens on the first device this is an error.
                 */
                return err;
            }
        }
    }

    DEV_DEBUG("Device discovery done: got %u devices.\n", device_count);

    free(dev_ids);

    return SYS_ERR_OK;
}

struct ioat_dma_device *ioat_device_get_next(void)
{
    if (device_next >= device_count) {
        device_next = 0;
    }
    return devices[device_next++];
}

errval_t ioat_device_poll(void)
{
    errval_t err;

    uint8_t idle = 0x1;
    for (uint8_t i = 0; i < device_count; ++i) {
        err = ioat_dma_device_poll_channels((struct dma_device *)devices[i]);
        switch (err_no(err)) {
            case SYS_ERR_OK:
                idle = 0;
                break;
            case DMA_ERR_DEVICE_IDLE:
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
