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

struct timestamp {
    cycles_t time0;
    cycles_t time1;
};

#define MAX_ITERATIONS 1000
struct timestamp timestamp[MAX_ITERATIONS];

int main(int argc, char** argv) {
    dist_init();
    bench_init();

    errval_t err = dist_set("object1 { attr: 'string', attr2: 12, attr3: 12.0 }");
    assert(err_is_ok(err));

    errval_t error_code;
    char* data = NULL;

    for(size_t i=0; i < MAX_ITERATIONS; i++) {
        struct dist2_rpc_client* cl = get_dist_rpc_client();

        timestamp[i].time0 = bench_tsc();
        cl->vtbl.get(cl, "object1", NOP_TRIGGER, &data, &error_code);
        timestamp[i].time1 = bench_tsc();

        //printf("data: %s\n", data);
        free(data);
    }

    for(size_t i=0; i < MAX_ITERATIONS; i++) {
        printf("%lu %"PRIuCYCLES"\n", i,
                timestamp[i].time1 - timestamp[i].time0 - bench_tscoverhead());
    }

}
