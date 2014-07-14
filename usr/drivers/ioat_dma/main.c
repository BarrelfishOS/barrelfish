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

#include <pci/devids.h>

#include "ioat_dma.h"
#include "ioat_dma_device.h"
#include "ioat_dma_descriptors.h"

#include "debug.h"

struct ioat_dma_ctrl dma_ctrl;

int main(int argc,
         char *argv[])
{
    errval_t err;

    debug_printf("I/O AT DMA driver started\n");

    memset(&dma_ctrl, 0, sizeof(dma_ctrl));

    /*
     * Parsing of cmdline arguments.
     *
     * When started by Kaluga, the last element of the cmdline will contain
     * the basic PCI information of the device.
     * VENDORID:DEVICEID:BUS:DEV:FUN
     */
    uint32_t vendor_id, device_id;
    struct pci_addr addr = {
        .bus = PCI_ADDR_DONT_CARE,
        .device = PCI_ADDR_DONT_CARE,
        .device = PCI_ADDR_DONT_CARE
    };

    if (argc > 1) {
        uint32_t parsed = sscanf(argv[argc - 1],
                                 "%x:%x:%x:%x:%x",
                                 &vendor_id,
                                 &device_id,
                                 &addr.bus,
                                 &addr.device,
                                 &addr.function);
        if (parsed != 5) {
            IODEBUG("WARNING: cmdline parsing failed. Using PCI Address [0,0,0]");
        } else {
            if (vendor_id != 0x8086 || (((device_id & 0xFFF0) != PCI_DEVICE_IOAT_IVB0)
                            && ((device_id & 0xFFF0) != PCI_DEVICE_IOAT_HSW0))) {
                USER_PANIC("unexpected vendor / device id: [%x, %x]",
                           vendor_id,
                           device_id);
                return -1;
            }
            IODEBUG("Initializing I/O AT DMA device with PCI address "
                   "[%u,%u,%u]\n",
                   addr.bus, addr.device, addr.function);
        }
    } else {
        IODEBUG("WARNING: Initializing I/O AT DMA device with unknown PCI address "
               "[0,0,0]\n");
    }

    err = ioat_dma_device_discovery(addr, device_id, &dma_ctrl);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "DMA Device discovery failed");
    }

    err = ioat_dma_desc_alloc_init(IOAT_DMA_DESC_SIZE, IOAT_DMA_DESC_ALIGN);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "DMA Descriptor allocator initialization failed.\n");
    }

    return 0;
}

