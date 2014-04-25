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

#include "xeon_phi.h"

volatile uint32_t bootstrap_done = 0;

struct xeon_phi xphi;

int main(int argc,
         char *argv[])
{
    debug_printf("Xeon Phi host module started.\n");

    host_bootstrap();

    while (bootstrap_done == 0) {
        messages_wait_and_handle_next();
    }

    debug_printf("Host bootstrap done\n");

    xeon_phi_boot(&xphi,
                  XEON_PHI_BOOTLOADER,
                  XEON_PHI_MULTIBOOT);



    return 0;
}
