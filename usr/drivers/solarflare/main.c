/**
 * \file
 * \brief Driver domain example.
 *
 * Implements a simple driver domain. This domain does two things:
 *
 * (a) it prints all the driver modules found
 *     There are a three things necessary for the driver module system to work:
 *     - A custom ELF section in the driver domain called .bfdrivers where
 *       all bfdriver structs are located.
 *     - A linker script that defines the section and two symbols
 *       `bfdrivers_start` and `bfdrivers_end` that mark the end and beginning
 *       of the section.
 *     - A special way to link the driver modules (using addModules) in hake,
 *       which makes sure the symbols are included in the binary without explicitly
 *       referencing them.
 *     For more information on how works, check the driverkit.h file as well
 *     along with modules.c in lib/driverkit.
 *
 * (b) it connects to the driver domain controller (Kaluga) and makes
 *     sure it handles it's requests (defined in ddomain.if) to create,
 *     destroy driver instances.
 *     The corresponding code for (b) is found in the driverkit library
 *     specifically in file ddomain_service.c
 */
/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <barrelfish/nameservice_client.h>

/**
 * Instantiate the driver domain.
 *
 * Connect to Kaluga and wait for eventual ddomain requests.
 */
int main(int argc, char** argv)
{
    iref_t kaluga_iref = 0;
    errval_t err = nameservice_blocking_lookup("ddomain_controller", &kaluga_iref);
    assert(err_is_ok(err));
    err = ddomain_communication_init(kaluga_iref, atoi(argv[argc-1]));
    assert(err_is_ok(err));

    while(1) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error in event_dispatch for messages_wait_and_handle_next hack");
        }
    }

    return 0;
}
