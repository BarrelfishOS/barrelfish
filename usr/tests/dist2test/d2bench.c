/**
 * \file
 * \brief Simple Barrier test
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <dist2/dist2.h>

#include <dist2_server/debug.h> // for benchmarking

#define MAX_ITERATIONS 100
struct timestamp {
    cycles_t time0;
    cycles_t time1;
    cycles_t server;
};
struct timestamp timestamps[MAX_ITERATIONS] = {{ 0, 0, 0 }};

//static size_t records[] = { 1, 4, 8, 16, 256, 65536 };

/*
static void variable_records(void)
{
    size_t exps = sizeof(records) / sizeof(size_t);

    for (size_t i=0; i < exps; i++) {
        errval_t err = dist_set("object%lu", i);
        assert(err_is_ok(err));
    }


    errval_t error_code;
    char* data = NULL;

    struct dist2_rpc_client* cl = get_dist_rpc_client();

    for(size_t i=0; i < MAX_ITERATIONS; i++) {
        timestamps[i].time0 = bench_tsc();
        cl->vtbl.get(cl, "object1", NOP_TRIGGER, &data, &error_code);
        timestamps[i].time1 = bench_tsc();

        free(data);
    }

    for(size_t i=0; i < MAX_ITERATIONS; i++) {
        printf("%lu %"PRIuCYCLES"\n", i,
                timestamps[i].time1 - timestamps[i].time0 - bench_tscoverhead());
    }
}*/

static void one_record(void)
{
    errval_t err = dist_set("object1");
    assert(err_is_ok(err));

    errval_t error_code;
    char* data = NULL;

    struct dist2_rpc_client* cl = get_dist_rpc_client();

    for(size_t i=0; i < MAX_ITERATIONS; i++) {
        timestamps[i].time0 = bench_tsc();
        cl->vtbl.get(cl, "object1", NOP_TRIGGER, &data, &error_code, &timestamps[i].server);
        timestamps[i].time1 = bench_tsc();

        free(data);
    }

    for(size_t i=0; i < MAX_ITERATIONS; i++) {
        printf("%lu %"PRIuCYCLES" %"PRIuCYCLES"\n", i,
                timestamps[i].time1 - timestamps[i].time0 - bench_tscoverhead(), timestamps[i].server);
    }
}

int main(int argc, char** argv) {
    dist_init();
    bench_init();

    one_record();
}
