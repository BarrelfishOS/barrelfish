/**
 * \file
 * \brief Sequential runner for standalone benchmarks
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#define PANIC_IF_ERR(err, msg...) \
do { \
    if (err_is_fail(err)) { \
        USER_PANIC_ERR(err, msg); \
    } \
} while(0)

static void usage(char *progname)
{
    printf("usage: %s <benchprog> <num cores> <coreids>...\n", progname);
}

int main(int argc, char *argv[])
{
    if (argc < 3) {
        usage(argv[0]);
    }
    int numcores = atoi(argv[2]);
    if (numcores <= 0 || argc < numcores+3) {
        usage(argv[0]);
    }
    for (int i = 0; i < numcores; i++) {
        errval_t err;
        struct capref domcap;
        uint8_t exitcode;
        coreid_t core = atoi(argv[i+3]);
        // Get first run of benchmark to print the benchmark prologue with the
        // right amount of cores
        char *newargv[] = { argv[1], i==0?argv[2]:NULL, NULL };
        char *newenvp[] = { NULL };
        printf("# Spawning benchmark program %s on core %d\n", argv[1], core);
        err = spawn_program(core, argv[1], newargv, newenvp, SPAWN_FLAGS_DEFAULT, &domcap);
        PANIC_IF_ERR(err, "spawning benchmark on core %d", core);
        printf("# waiting for benchmark to complete on core %d\n", core);
        err = spawn_wait_core(core, domcap, &exitcode, false);
        PANIC_IF_ERR(err, "waiting for benchmark on core %d", core);
        if (exitcode != 0) {
            printf("# Benchmark on core exited with %d\n", exitcode);
        }
    }
    printf("# Benchmark done!\n");
    return 0;
}
