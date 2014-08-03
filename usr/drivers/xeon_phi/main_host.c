/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <octopus/octopus.h>
#include <vfs/vfs.h>
#include <pci/pci.h>
#include <pci/devids.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_manager_client.h>

#include "xeon_phi_internal.h"
#include "smpt.h"
#include "dma_service.h"
#include "service.h"
#include "xphi_service.h"
#include "interphi.h"
#include "domain.h"
#include "sysmem_caps.h"

struct xeon_phi xphi;

/**
 * \brief Main function of the Xeon Phi Driver (Host Side)
 */
int main(int argc,
         char *argv[])
{
    errval_t err;

    XDEBUG("Xeon Phi host module started.\n");

    memset(&xphi, 0, sizeof(xphi));

    /*
     * Parsing of cmdline arguments.
     *
     * When started by Kaluga, the last element of the cmdline will contain
     * the basic PCI information of the device.
     * VENDORID:DEVICEID:BUS:DEV:FUN
     */
    uint32_t vendor_id, device_id;
    uint32_t bus = PCI_DONT_CARE, dev = PCI_DONT_CARE, fun = PCI_DONT_CARE;

    if (argc > 1) {
        uint32_t parsed = sscanf(argv[argc - 1], "%x:%x:%x:%x:%x", &vendor_id,
                                 &device_id, &bus, &dev, &fun);
        if (parsed != 5) {
            XDEBUG("WARNING: cmdline parsing failed. Using PCI Address [0,0,0]");
            bus = PCI_DONT_CARE;
            dev = PCI_DONT_CARE;
            fun = PCI_DONT_CARE;
        } else {
            if (vendor_id != 0x8086 || ((device_id & 0xFFF0) != 0x2250)) {
                USER_PANIC("unexpected vendor / device id: [%x, %x]", vendor_id,
                           device_id);
                return -1;
            }
            XDEBUG("Initializing Xeon Phi with PCI address "
                   "[%u,%u,%u]\n",
                   bus, dev, fun);
        }
    } else {
        XDEBUG("WARNING: Initializing Xeon Phi with unknown PCI address "
               "[0,0,0]\n");
    }

    /* set the client flag to false */
    xphi.is_client = XEON_PHI_IS_CLIENT;

    vfs_init();

    err = oct_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "initializing octopus\n");
    }

    err = service_init(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not start the driver service\n");
    }

    uint8_t num;
    iref_t *irefs;
    err = xeon_phi_manager_client_register(xphi.iref, &xphi.id, &num, &irefs);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register with the Xeon Phi manager\n");
    }

    xphi.state = XEON_PHI_STATE_NULL;

    err = xeon_phi_init(&xphi, bus, dev, fun);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not do the card initialization\n");
    }

    err = service_register(&xphi, irefs, num);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register with the other drivers");
    }

    err = xeon_phi_boot(&xphi, XEON_PHI_BOOTLOADER, XEON_PHI_MULTIBOOT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not boot the card\n");
    }

    interphi_wait_for_client(&xphi);

    err = xdma_service_init(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the DMA engine\n");
    }

    err = xeon_phi_service_init(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the messaging service");
    }

    /*
     * in case there are more than one Xeon Phi present in the system, indicated
     * by an id > 0, the driver will register itself with the other Xeon Phi
     * driver instances running in the system and initializes the inter-Phi
     * messaging frame
     */
    if (xphi.id != 0) {
        XDEBUG("Doing Intra Xeon Phi setup with %u other instances\n", xphi.id);
        for (uint32_t i = 0; i < xphi.id; ++i) {
            /* initialize the messaging frame */
            err = interphi_init_xphi(i, &xphi, NULL_CAP, XEON_PHI_IS_CLIENT);
            if (err_is_fail(err)) {
                XDEBUG("Could not initialize messaging\n");
                continue;
            }
        }
    }


    char buf[20];
    snprintf(buf, 20, "xeon_phi.%u.ready", xphi.id);

    XDEBUG("register ready: %s\n", buf);
    err = domain_register(buf, 0xcafebabe);
    assert(err_is_ok(err));

    XDEBUG("initialization done. Going into main message loop\n");

    /* starts the basic handler service. This function should not return */
    service_start(&xphi);

    XDEBUG("Xeon Phi host module terminated.\n");

    return 0;
}
