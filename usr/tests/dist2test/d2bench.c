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
#include <skb/skb.h>

#define MAX_ITERATIONS 1000
struct timestamp {
    cycles_t time0;
    cycles_t time1;
    cycles_t server;
    uint8_t busy;
};
struct timestamp timestamps[MAX_ITERATIONS] = { { 0, 0, 0, 0 } };
static size_t records[] = { 0, 8, 16, 256, 512, 768, 1000, 1500, 2000, 2500,
        4000, 5000, 6000, 7000, 8000, 9000, 10000, 12000, 20000 };

static void variable_records(void)
{
    size_t exps = sizeof(records) / sizeof(size_t);
    for (size_t i = 1; i < exps; i++) {
        printf("# Run experiment with %lu records:\n", records[i]);

        for (size_t j = records[i - 1]; j < records[i]; j++) {
            errval_t err = dist_set("object%lu { attr: 'object%lu' }", j, j);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "set");
                exit(0);
            }
        }

        errval_t error_code;
        char* data = NULL;

        struct dist2_rpc_client* cl = get_dist_rpc_client();
        assert(cl != NULL);

        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            size_t get_nr = k % records[i];
            char buf[100];
            sprintf(buf, "object%lu", get_nr);

            timestamps[k].time0 = bench_tsc();
            cl->vtbl.get(cl, buf, NOP_TRIGGER, &data, &error_code,
                    &timestamps[k].server, &timestamps[k].busy);
            timestamps[k].time1 = bench_tsc();
            if (err_is_fail(error_code)) {
                DEBUG_ERR(error_code, "get");
                exit(0);
            }
            free(data);
        }

        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            printf(
                    "%lu %"PRIuCYCLES" %"PRIuCYCLES" %d %lu\n",
                    k,
                    timestamps[k].time1 - timestamps[k].time0
                            - bench_tscoverhead(), timestamps[k].server,
                    timestamps[k].busy, records[i]);
        }

    }
}

static void one_record(void)
{
    errval_t err = dist_set("object0");
    assert(err_is_ok(err));

    errval_t error_code;
    char* data = NULL;

    struct dist2_rpc_client* cl = get_dist_rpc_client();

    for (size_t i = 0; i < MAX_ITERATIONS; i++) {

        timestamps[i].time0 = bench_tsc();
        cl->vtbl.get(cl, "object0", NOP_TRIGGER, &data, &error_code,
                &timestamps[i].server, &timestamps[i].busy);
        timestamps[i].time1 = bench_tsc();

        assert(err_is_ok(error_code));
        free(data);
        for(size_t j=0; j<1<<20; j++) {}
    }

    for (size_t i = 0; i < MAX_ITERATIONS; i++) {
        printf("%lu %"PRIuCYCLES" %"PRIuCYCLES" %d\n", i,
                timestamps[i].time1 - timestamps[i].time0 - bench_tscoverhead(),
                timestamps[i].server, timestamps[i].busy);
    }
}

static void unnamed_record(void)
{
    errval_t err = dist_set("object0 { attr1: 'bla', attr2: 12.0 }");
    assert(err_is_ok(err));

    while(1) {
        char* data = NULL;
        err = dist_get(&data, "_ { }");
        if (err_is_fail(err)) DEBUG_ERR(err, "get");
        printf("dist_get returned: %s\n", data);
        free(data);
        assert(err_is_ok(err));

        err = dist_get(&data, "_ { attr2: 12.0 }");
        if (err_is_fail(err)) DEBUG_ERR(err, "get");
        printf("dist_get returned: %s\n", data);
        free(data);
        assert(err_is_ok(err));
    }
}

int main(int argc, char** argv)
{
    bench_init();
    dist_init();

    one_record();
    if (0) variable_records();
    if (0) unnamed_record();
}
