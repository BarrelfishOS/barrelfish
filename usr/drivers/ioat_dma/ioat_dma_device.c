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

#include "ioat_dma_device.h"
#include "ioat_dma_channel.h"

static struct ioat_dma_device *curr_dev;

static errval_t curr_err;

static void ioat_intrupt_handler(void *arg)
{

}

static void device_init(struct device_mem* bar_info,
                        int bar_count)
{
    assert(curr_dev);

    if (curr_dev->state != IOAT_DMA_DEV_ST_UNINITIALIZED) {
        return;
    }

    printf("Got: %i bars\n", bar_count);
    for (int i = 0; i < bar_count; ++i) {
        printf("> Bar[%i]: {type=%i, paddr=0x%lx, size=%u}\n",
               i,
               bar_info[i].type,
               bar_info[i].paddr,
               (uint32_t) (bar_info[i].bytes / 1024));
    }

    if (bar_count != 1) {
        USER_PANIC("There is something wrong. The device should have 1 MBARs.");
    }

}

errval_t ioat_dma_device_init(uint16_t device_id,
                              struct pci_address addr,
                              struct ioat_dma_device *dev)
{
    errval_t err;

    assert(dev);
    assert(curr_dev == NULL);

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
                                  ioat_intrupt_handler,
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
errval_t ioat_dma_device_discovery(struct pci_address addr,
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
    switch (device & 0xFFF0) {
        case PCI_DEVICE_IOAT_IVB0:
            /*
             * Ivy Bridge Platform
             */
            if (addr.device != 4) {
                /* the IOAT DMA engine should be on device 4 on ivy bridge */
                /* TODO: error number */
                return -1;
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
            /*
             * Haswell Platform
             */
            if (addr.device != 4) {
                /* the IOAT DMA engine should be on device 4 on ivy bridge */
                /* TODO: error number */
                return -1;
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
            /* TODO: return unsupported error number */
            return -1;
    }

    struct ioat_dma_device *dev = calloc(dev_cnt, sizeof(struct ioat_dma_device));
    if (dev == NULL) {
        free(dev_ids);
        return LIB_ERR_MALLOC_FAIL;
    }

    for (uint16_t i = 0; i < dev_cnt; ++i) {
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
        dev[i].dma_ctrl = ctrl;
        addr.function++;
    }

    ctrl->devices = dev;
    ctrl->device_num = dev_cnt;

    free(dev_ids);

    return SYS_ERR_OK;
}
