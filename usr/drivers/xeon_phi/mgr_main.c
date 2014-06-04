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

#include <xeon_phi/xeon_phi.h>

#include "xeon_phi.h"
#include "messaging.h"
#include "sysmem_caps.h"

static struct xeon_phi xphi;

static struct capref mmio_cap = {
    .slot = TASKCN_SLOT_IO
};

static struct capref sysmem_cap = {
    .slot = TASKCN_SLOT_SYSMEM
};

static struct capref host_cap;



int main(int argc,
         char *argv[])
{
    debug_printf("Xeon Phi Mgr module started. %i, %s\n", argc, argv[0]);

    errval_t err;
    lpaddr_t host_base;
    genvaddr_t offset;

    mmio_cap.cnode = cnode_task;
    sysmem_cap.cnode = cnode_task;

    assert(!capref_is_null(mmio_cap));
    assert(!capref_is_null(sysmem_cap));

    xphi.is_client = 0x1;

    XDEBUG("Initializing system memory cap manager...\n");
    err = sysmem_cap_manager_init(sysmem_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize the cap manager.\n");
    }

    host_base = strtol(argv[0], NULL, 16);
    offset = host_base;

    XMESSAGING_DEBUG("Getting the host messaging cap...\n");
    err = sysmem_cap_request(host_base, XEON_PHI_MSG_INIT_SIZE, &host_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not obtain the system messsaging cap\n");
    }


    messaging_init(&xphi, host_cap);

    XMESSAGING_DEBUG("Start polling for messages...\n");
    while(1) {
        messaging_poll(&xphi);
    }

    XDEBUG("Messaging loop terminated...\n");
    return 0;
}
