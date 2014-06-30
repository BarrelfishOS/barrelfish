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

#include <vfs/vfs.h>
#include <pci/pci.h>

#include <xeon_phi/xeon_phi_manager_client.h>
#include <xeon_phi/xeon_phi_messaging.h>

#include "xeon_phi_internal.h"
#include "smpt.h"
#include "service.h"
#include "messaging.h"
#include "sysmem_caps.h"

volatile uint32_t bootstrap_done = 0;

struct xeon_phi xphi;

struct xeon_phi_messaging_cb msg_cb = {
    .open_iface = messaging_send_open,
    .spawn = messaging_send_spawn
};

int main(int argc,
         char *argv[])
{
    errval_t err;
    debug_printf("Xeon Phi host module started.\n");

    uint32_t vendor_id, device_id;
    uint32_t bus = PCI_DONT_CARE, dev = PCI_DONT_CARE, fun = PCI_DONT_CARE;

    if (argc > 1) {
        uint32_t parsed = sscanf(argv[argc - 1],
                                 "%x:%x:%x:%x:%x",
                                 &vendor_id,
                                 &device_id,
                                 &bus,
                                 &dev,
                                 &fun);
        if (parsed != 5) {
            debug_printf("WARNING: parsing cmdline argument failed. >"
                         "Switching back to unknown PCI address [0,0,0]");
            bus = PCI_DONT_CARE;
            dev = PCI_DONT_CARE;
            fun = PCI_DONT_CARE;
        } else {
            if (vendor_id != 0x8086 || ((device_id & 0xFFF0) != 0x2250)) {
                debug_printf("ERROR: Unexpected vendor / device ID"
                             "was: [%x, %x] expected: [%x, %x]",
                             vendor_id, (device_id & 0xFF00), 0x8086, 0x2500);
                return -1;
            }
            debug_printf("Initializing Xeon Phi with PCI address "
                         "[%u,%u,%u]\n",
                         bus, dev, fun);
        }
    } else {
        debug_printf("WARNING: Initializing Xeon Phi with unknown PCI address "
                     "[0,0,0]\n");
    }

    memset(&xphi, 0, sizeof(xphi));

    xphi.is_client = 0x0;

    vfs_init();

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

    err = xeon_phi_messaging_service_init(&msg_cb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the messaging service");
    }

    err = xeon_phi_messaging_service_start_phi(xphi.id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not export the service");
    }

    if (xphi.id != 0) {
        XDEBUG("Doing Intra Xeon Phi setup\n");
        for (uint32_t i = 0; i < xphi.id; ++i) {
            /* initialize the messaging frame */
            err = messaging_init_xphi(i, &xphi, NULL_CAP, 0x0);
            if (err_is_fail(err)) {
                XDEBUG("Could not initialize messaging\n");
                continue;
            }

            err = service_open(&xphi, i);
            if (err_is_fail(err)) {
                XDEBUG("Could not initialize messaging\n");
                continue;
            }
        }
    }

    XDEBUG("initialization done. Going into main message loop\n");

    service_start(&xphi);

    debug_printf("Xeon Phi host module terminated.\n");

    return 0;
}
