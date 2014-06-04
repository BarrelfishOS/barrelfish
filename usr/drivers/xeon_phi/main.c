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
#include <xeon_phi/xeon_phi_manager_client.h>

#include "xeon_phi.h"
#include "smpt.h"
#include "service.h"
#include "messaging.h"

volatile uint32_t bootstrap_done = 0;

struct xeon_phi xphi;

int main(int argc,
         char *argv[])
{
    errval_t err;
    debug_printf("Xeon Phi host module started.\n");

    xphi.is_client = 0x0;

    err = host_bootstrap();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not do the host bootstrap\n");
    }

    while (bootstrap_done == 0) {
        messages_wait_and_handle_next();
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

    err = xeon_phi_init(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not do the card initialization\n");
    }

    err = xeon_phi_boot(&xphi, XEON_PHI_BOOTLOADER, XEON_PHI_MULTIBOOT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not boot the card\n");
    }

    err = service_register(&xphi, irefs, num);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not register with the other drivers");
    }

    // sending a test message
    struct xeon_phi_msg_hdr hdr;
    hdr.size = sizeof(uint64_t);
    uint64_t data = 0x1;
    messaging_send(&xphi, hdr, &data);

    service_start(&xphi);

    debug_printf("Xeon Phi host module terminated.\n");

    return 0;
}
