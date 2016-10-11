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

#include "e1000n_debug.h"
#include <pci/devids.h>
/*****************************************************************
 * Local states:
 *****************************************************************/
static uint32_t bus = PCI_DONT_CARE;
static uint32_t device = PCI_DONT_CARE;
static uint32_t function = PCI_DONT_CARE;
static uint32_t deviceid = PCI_DONT_CARE;
static uint32_t vendor = PCI_VENDOR_INTEL;

int main(int argc, char **argv)
{
    /** Parse command line arguments. */
    E1000E_DEBUG("e1000e standalone driver started.\n");

    E1000E_DEBUG("argc = %d\n", argc);

    /* try parse Kaluga information which is located at the last argument */
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

    E1000E_DEBUG("#### starting polling.\n");
    return 1;
}

