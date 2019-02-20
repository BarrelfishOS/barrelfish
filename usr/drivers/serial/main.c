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

int main(int argc, char *argv[])
{

    static struct serial_main main;
    init_serial_main(&main, argc, argv);


    // Initialize serial driver
    // TODO: Figure out how to init the right module.
//    err = serial_init(&params);
//    if (err_is_fail(err)) {
//        USER_PANIC_ERR(err, "Error initializing serial driver.");
//    }
//
//    SERIAL_DEBUG("Serial driver initialized.\n"
//                 "Using driver name %s.\n", driver_name);
//
    // Stick around waiting for input
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Error dispatching events.");
        }
    }

    return EXIT_SUCCESS;

usage:
    fprintf(stderr, "Usage: %s [portbase=PORT] [irq=IRQ] [name=NAME]\n"
                    "    [membase=ADDR] [kernel]\n", argv[0]);
    return 1;
}
