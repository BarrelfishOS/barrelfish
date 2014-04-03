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
#include <barrelfish/deferred.h>
#include <bench/bench.h>

#define MAX_ITERATION 1000

static void set_true(void* arg) {
    *(bool*)arg = true;
}

static void sleep_until(delayus_t delay) {
    struct deferred_event de;
    deferred_event_init(&de);

    bool can_continue = false;

    struct event_closure ec;
    ec.handler = set_true;
    ec.arg = &can_continue;

    errval_t err = deferred_event_register(&de, get_default_waitset(),
                                           delay, ec);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "deferred event register failed.");
    }

    //printf("%s:%s:%d\n", __FILE__, __FUNCTION__, __LINE__);
    while(!can_continue) {
        messages_wait_and_handle_next();
    }
    //printf("%s:%s:%d\n", __FILE__, __FUNCTION__, __LINE__);
}

int main(int argc, char** argv)
{
    uint64_t sleep = 0;
    uint64_t ram_bits = 20;
    printf("%s:%s:%d: argc = %d\n", __FILE__, __FUNCTION__, __LINE__, argc);
    if (argc > 3) {
        printf("%s:%s:%d: Usage: %s <ram bits> <sleep ms>\n",
               __FILE__, __FUNCTION__, __LINE__, argv[0]);
    } else if (argc == 3) {
        sleep = atoll(argv[2]);
        ram_bits = atoll(argv[1]);
    } else if (argc == 2) {
        ram_bits = atoll(argv[1]);
    }
    printf("%s:%s:%d: Use ram_bits = %"PRIu64"\n",
       __FILE__, __FUNCTION__, __LINE__, ram_bits);
    printf("%s:%s:%d: Use sleep = %"PRIu64"\n",
        __FILE__, __FUNCTION__, __LINE__, sleep);

    bench_init();
    cycles_t runs[MAX_ITERATION];

    errval_t err;
    uint64_t start, end;

    struct capref ram;
    err = ram_alloc(&ram, ram_bits);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ram_alloc failed.");
    }

    struct capref frame;
    err = slot_alloc(&frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "slot_alloc failed.");
    }

    for (size_t i=0; i<MAX_ITERATION; i++) {
        //printf("%s:%s:%d: i=%"PRIu64"\n",
        //       __FILE__, __FUNCTION__, __LINE__, i);
        start = bench_tsc();
        //printf("%s:%s:%d: \n", __FILE__, __FUNCTION__, __LINE__);
        err = cap_retype(frame, ram, ObjType_Frame, ram_bits);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_retype failed.");
        }
        end = bench_tsc();

        err = cap_delete(frame);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_delete failed.");
        }

        if (sleep > 0) {
            sleep_until(sleep);
        }

        runs[i] = end - start;
    }

    runs[0] = BENCH_IGNORE_WATERMARK;

    printf("Average cycles %"PRIuCYCLES", Variance %"PRIuCYCLES"\n \
           Average ms %"PRIu64" Variance ms %"PRIu64"\n",
            bench_tsc_to_ms(bench_avg(runs, MAX_ITERATION)),
            bench_tsc_to_ms(bench_variance(runs, MAX_ITERATION)));

    return 0;
}
