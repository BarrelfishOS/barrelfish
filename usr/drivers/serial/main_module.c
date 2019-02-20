/**
 * \file
 * \brief Serial port driver.
 */

/*
 * Copyright (c) 2007, 2008, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include "serial.h"
#include "serial_debug.h"
#include <driverkit/driverkit.h>

static errval_t
init_kernel(struct bfdriver_instance* bfi, uint64_t flags, iref_t *dev)
{
    errval_t err;
    struct serial_main *m = malloc(sizeof(struct serial_main));
    bfi->dstate = m;

    m->irq_src.cnode = bfi->argcn;
    m->irq_src.slot = 0;

    err = init_serial_main(m, bfi->argc, bfi->argv);
    assert(err_is_ok(err));

    // Initialize serial driver
    err = serial_kernel_init(m);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "serial_init");
        return err;
    }

    SERIAL_DEBUG("Kernel Serial driver initialized.\n");

    return SYS_ERR_OK;
}

static errval_t attach(struct bfdriver_instance* bfi) {
    return SYS_ERR_OK;
}

static errval_t detach(struct bfdriver_instance* bfi) {
    return SYS_ERR_OK;
}

static errval_t set_sleep_level(struct bfdriver_instance* bfi, uint32_t level) {
    return SYS_ERR_OK;
}

static errval_t destroy(struct bfdriver_instance* bfi) {
    struct pl390_dist_driver_state* uds = bfi->dstate;
    free(uds);
    bfi->dstate = NULL;
    // XXX: Tear-down the service
    bfi->device = 0x0;
    return SYS_ERR_OK;
}

static errval_t get_ep(struct bfdriver_instance* bfi, bool lmp, struct capref* ret_cap)
{   
    USER_PANIC("NIY \n");
    return SYS_ERR_OK;
}

DEFINE_MODULE(serial_kernel, init_kernel, attach, detach, set_sleep_level, destroy, get_ep);
