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
static struct pci_addr pci_addr;


#if 0
static void pci_dev_init_service(void *arg, struct device_mem *bar_info,
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
    err = ioat_dma_device_init(bar_info->frame_cap, &pci_addr,
                               &devices[device_count]);
    if (err_is_fail(err)) {
        DEV_ERR("Could not initialize the device: %s\n", err_getstring(err));
        return;
    }

    device_count++;
}

static void pci_dev_init_manager(void *arg, struct device_mem *bar_info,
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
#endif


