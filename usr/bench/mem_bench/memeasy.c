/**
 * \file
 * \brief Simple Memory Benchmark to test kernel infrastructure
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>

#define MAX_ITERATION 1000

int main(int argc, char** argv)
{
    static uint64_t sleep = 0;
    static uint64_t ram_bits = 0;
    if (argc > 3) {
        printf("%s:%s:%d: Usage: %s <sleep ms> <ram bits>\n",
               __FILE__, __FUNCTION__, __LINE__, argv[0]);
    }
    else if (argc == 3) {
        sleep = atoll(argv[1]);
        ram_bits = atoll(argv[2]);
    }
    printf("%s:%s:%d: Sleeping for %"PRIu64" ms inbetween.\n",
            __FILE__, __FUNCTION__, __LINE__, sleep);

    bench_init();
    cycles_t runs[MAX_ITERATION];

    errval_t err;
    uint64_t start, end;

    struct capref ram;
    err = ram_alloc(&ram, 1 << ram_bits);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ram_alloc failed.");
    }

    struct capref frame;
    err = slot_alloc(&frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "slot_alloc failed.");
    }

    for (size_t i=0; i<MAX_ITERATION; i++) {
        start = bench_tsc();
        err = cap_retype(ram, frame, ObjType_Frame, 1 << ram_bits);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_retype failed.");
        }

        err = cap_delete(frame);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_delete failed.");
        }
        end = bench_tsc();

        runs[i] = end - start;
    }

    printf("Average cycles %"PRIuCYCLES", Variance %"PRIuCYCLES"\n",
            bench_avg(runs, MAX_ITERATION),
            bench_variance(runs, MAX_ITERATION));


    return 0;
}
