/*
 * Copyright (c) 2016, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 * e1000e.c
 *
 *  Created on: Oct 10, 2016
 *      Author: lh
 *
 * NOTES:
 *      General:
 *          The driver must be started by Kaluga. It supports
 *          the Intel 82574 GbE Controller Family.
 *
 *      PCI Ids:
 *          Vendor: 0x8086, Device: 0x10d3 
 *
 *
 * Inspired by the Barrelfish e1000 driver.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <octopus/octopus.h>
#include <net_queue_manager/net_queue_manager.h>
#include <if/net_queue_manager_defs.h>
#include <pci/devids.h>
#include <pci/pci.h>

#include "e1000e_debug.h"

#include <dev/e1000e_dev.h>

static void e1000e_hwinit(e1000e_t *dev, struct device_mem *bar_info,
                  int nr_allocated_bars)
{
    errval_t err;
    if (nr_allocated_bars < 1) {
        USER_PANIC("Error: Not enough PCI bars allocated. Can not initialize network device.\n");
    }

    err = map_device(&bar_info[0]);
    if (err_is_fail(err)) {
        USER_PANIC("Error: map_device failed. Can not initialize network device.\n");
    }

    e1000e_initialize(dev, (void *) bar_info[0].vaddr);
}

// Interrupt handler
static void e1000e_interrupt_handler_fn(void *arg)
{
    E1000E_DEBUG("e1000e_interrupt_handler called");
}

// Callback from pci lib
static void e1000e_init_fn(struct device_mem *bar_info, int nr_allocated_bars) {
    E1000E_DEBUG("e1000e_init_fn interrupt handler called");
    e1000e_t dev;
    e1000e_hwinit(&dev, bar_info, nr_allocated_bars);
}

// On core move
static void e1000e_reregister_handler(void *arg) {
    printf("%s:%s:%d:\n", __FILE__, __FUNCTION__, __LINE__);
}

// Local state
static uint32_t class = PCI_CLASS_ETHERNET;
static uint32_t subclass = PCI_DONT_CARE;
static uint32_t bus = PCI_DONT_CARE;
static uint32_t device = PCI_DONT_CARE;
static uint32_t function = PCI_DONT_CARE;
static uint32_t deviceid = PCI_DONT_CARE;
static uint32_t vendor = PCI_VENDOR_INTEL;
static uint32_t program_interface = PCI_DONT_CARE;

int main(int argc, char **argv)
{
    errval_t err;
    // Parse command line arguments.
    E1000E_DEBUG("e1000e standalone driver started.\n");
    E1000E_DEBUG("argc = %d\n", argc);

    // try parse Kaluga information which is located at the last argument
    if (argc > 1) {
        uint32_t parsed = sscanf(argv[argc - 1], "%x:%x:%x:%x:%x", &vendor,
                                 &deviceid, &bus, &device, &function);
        if (parsed != 5) {
            E1000E_DEBUG("Driver seems not to be started by Kaluga.\n");
            vendor = PCI_DONT_CARE;
            deviceid = PCI_DONT_CARE;
            bus = PCI_DONT_CARE;
            device = PCI_DONT_CARE;
            function = PCI_DONT_CARE;
        } else {
            E1000E_DEBUG("PCI Device (%u, %u, %u) Vendor: 0x%04x, Device 0x%04x\n",
                        bus, device, function, vendor, deviceid);
            // remove the last argument
            argc--;
        }
    }

    printf("########### Driver with interrupts ###########\n");
    err = pci_register_driver_movable_irq(e1000e_init_fn, class, subclass, program_interface,
                                          vendor, deviceid, bus, device, function,
                                          e1000e_interrupt_handler_fn, NULL,
                                          e1000e_reregister_handler,
                                          NULL);
    assert(err_is_ok(err));

    E1000E_DEBUG("#### starting polling.\n");
    while (true) {
        event_dispatch(get_default_waitset());
    }
    return 1;
}

