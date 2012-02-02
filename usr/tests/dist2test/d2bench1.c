/**
 * \file
 * \brief Benchmark publish / subscribe throughput.
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
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <dist2/dist2.h>
#include <skb/skb.h>

#define EXP_RUNTIME_MS 10000

int main(int argc, char** argv)
{
    dist_init();
    bench_init();

    char payload[256] = { 'a' };
    payload[255] = '\0';

    dist_set("rec { attr: '1' }");

    struct dist2_rpc_client* cl = get_dist_rpc_client();
    assert(cl != NULL);
    cycles_t time0 = bench_tsc();
    cycles_t time1 = bench_tsc();
    size_t cur = 0;
    char* record;
    errval_t error_code;
    cycles_t server;
    uint8_t busy;

    while ( bench_tsc_to_ms(time1-time0) < EXP_RUNTIME_MS ) {
        cl->vtbl.get(cl, "rec", NOP_TRIGGER, &record, &error_code, &server, &busy);
        cur++;
        if(cur % 5000) {
        	time1 = bench_tsc();
        }
        free(record);
    }
    printf("GET Operations per second: %lu", cur / (EXP_RUNTIME_MS/1000) );


    return EXIT_SUCCESS;
}
