/**
 * \file
 * \brief Tests for octopus get/set/del API
 */

/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <if/monitor_defs.h>
#include <if/monitor_blocking_rpcclient_defs.h>

int main(int argc, char *argv[])
{
    errval_t err;
    printf("%s:%d start cpumigrate test program.\n", __FILE__, __LINE__);

    struct monitor_binding *mb = get_monitor_binding();

    printf("%s:%d We are on core=%"PRIuCOREID"\n", 
           __FILE__, __LINE__, disp_get_core_id());
    for (volatile int i=0; i<0xFFFFF; i++);

    printf("%s:%d: attempt to migrate\n", __FILE__, __LINE__);
    assert(disp_get_core_id() == 1);
    err = mb->tx_vtbl.migrate_dispatcher_request(mb, NOP_CONT, 0, NULL_CAP, NULL_CAP);
    assert(err_is_ok(err));

    printf("%s:%d: migrated...\n", __FILE__, __LINE__);
    for (volatile int i=0; i<0xFFFFF; i++);

    return EXIT_SUCCESS;
}