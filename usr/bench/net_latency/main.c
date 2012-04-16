/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>


static void start_run(void)
{
    errval_t r;
    domainid_t new_domain;
    uint8_t code;
    char* const argv[] = { "e10k_queue_elb", "queue=0", NULL };

    r = spawn_program(0, argv[0], argv, NULL, SPAWN_FLAGS_DEFAULT, &new_domain);
    assert(err_is_ok(r));

    r = spawn_wait(new_domain, &code, false);
    assert(err_is_ok(r));

}


int main(int argc, char **argv)
{
    printf("Net latency benchmark start\n");
    printf("Starting first run\n");
    start_run();
    printf("Starting second run\n");
    start_run();
    printf("Net latency benchmark done\n");
    return 0;
}

