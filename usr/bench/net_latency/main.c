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

#include "sleep.h"


static void start_run(uint8_t core)
{
    errval_t r;
    domainid_t new_domain;
    //uint8_t code;
    char* const argv[] = { "e10k_queue_elb", "queue=0", NULL };

    r = spawn_program(core, argv[0], argv, NULL, SPAWN_NEW_DOMAIN, &new_domain);
    assert(err_is_ok(r));

    /*r = spawn_wait(new_domain, &code, false);
    assert(err_is_ok(r));*/
    milli_sleep(5*1000);
}


int main(int argc, char **argv)
{
    uint8_t core = 0;
    printf("Net latency benchmark start\n");

    /*if (!strncmp(argv[1], "startcore=", strlen("startcore="))) {
        core = atol(argv[1] + strlen("startcore="));
    }*/
    for (; core < 16; core++) {
        printf("Core %d:\n  ", core);
        fflush(stdout);
        start_run(core);
    }
    printf("Net latency benchmark done\n");
    return 0;
}

