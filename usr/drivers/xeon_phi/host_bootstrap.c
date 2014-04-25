/**
 * \file
 * \brief Boot module for the Xeon Phi
 *
 * Loads the co processor OS onto the card and boots it
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
#include <barrelfish/nameservice_client.h>
#include <spawndomain/spawndomain.h>

#include <if/monitor_defs.h>
#include <if/monitor_blocking_rpcclient_defs.h>

#include "xeon_phi.h"

extern struct bootinfo *bi;
extern volatile uint32_t bootstrap_done;

/**
 * \brief reply handler for the multiboot cap transfers
 *
 *
 */
static void multiboot_cap_reply(struct monitor_binding *st,
                                struct capref cap,
                                errval_t msgerr)
{
    errval_t err;
    static cslot_t multiboot_slots = 0;

    /*
     * this indicates that there are no more multiboot caps available
     * so we got everything
     */
    if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "We have all multiboot modules...");

        struct monitor_blocking_rpc_client *cl = get_monitor_blocking_rpc_client();
        if (cl == NULL) {
            USER_PANIC("Could not get a connection to monitor RPC client\n");
        }

        struct capref bootinfo_frame;
        size_t bootinfo_size;

        msgerr = cl->vtbl.get_bootinfo(cl, &err, &bootinfo_frame, &bootinfo_size);
        if (err_is_fail(msgerr)) {
            USER_PANIC_ERR(err, "Could not request bootinfo from monitor.\n");
        }

        err = vspace_map_one_frame((void**) bi,
                                   bootinfo_size,
                                   bootinfo_frame,
                                   NULL,
                                   NULL);

        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Could not map bootinfo frame in our VSPACE\n");
        }

        bootstrap_done = 1;
    }

    /*
     * we got a new cap, move it to the correct location in our CSPACE
     */
    struct capref dest = {
        .cnode = cnode_module,
        .slot = multiboot_slots++,
    };

    err = cap_copy(dest, cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not copy the cap");
    }

    err = cap_destroy(cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Destroying the received cap failed");
    }

    err = st->tx_vtbl.multiboot_cap_request(st, NOP_CONT, multiboot_slots);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Sending the next multiboot request failed");
    }
}

/**
 * \brief Bootstraps the host driver to get the multiboot images of the
 *        xeon phi loader and the xeon phi multiboot image
 */
errval_t host_bootstrap(void)
{
    errval_t err;

    /* Create the module cnode */
    struct capref modulecn_cap = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_MODULECN
    };

    err = cnode_create_raw(modulecn_cap,
                           NULL,
                           ((cslot_t) 1 << MODULECN_SIZE_BITS),
                           NULL);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cnode_create_raw failed");
        return err;
    }

    struct monitor_binding *st = get_monitor_binding();
    st->rx_vtbl.multiboot_cap_reply = multiboot_cap_reply;

    // Make first multiboot cap request
    err = st->tx_vtbl.multiboot_cap_request(st, NOP_CONT, 0);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}
