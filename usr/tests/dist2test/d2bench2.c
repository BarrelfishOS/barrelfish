/**
 * \file
 * \brief Benchmarking random workload for get/set queries with no
 * specified record name.
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

#define MAX_ITERATIONS 1000
struct timestamp {
    cycles_t time0;
    cycles_t time1;
};
struct timestamp timestamps[MAX_ITERATIONS] = {{ 0, 0 }};
static size_t records[] = { 0, 8, 16, 256, 512, 768, 1000, 1500, 2000, 2500,
        4000, 5000, 6000, 7000, 8000, 9000, 10000, 12000  };

static inline uint64_t get_cycle_counter(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}

static uint32_t my_random(void)
{
    static bool seeded = false;
    static uint32_t m_z = 0xdeadbeef;
    static uint32_t m_w = 0;
    if (!seeded) {
        m_w = (uint32_t) get_cycle_counter();
        seeded = true;
    }

    m_z = 36969 * (m_z & 65535) + (m_z >> 16);
    m_w = 18000 * (m_w & 65535) + (m_w >> 16);

    return (m_z << 16) + m_w;  // 32-bit result
}

static char* attrs[] = {
        "weight",
        "value",
        "attr",
        "bench",
        "rand",
        "type",
        "lock",
        "barrier",
        "lock",
        "time",
        "range",
        "irq",
        "port",
        "iref",
        "description",
        "portrange",
        "from",
        "to",
        "version",
        "owner",
};

static char* values[] = {
        "'test description'",
        "'value'",
        "'name'",
        "'1500-12333'",
        "'x == 2'",
        "11.0",
        "21.0",
        "1.12312",
        "2.34221",
        "9.123",
        "12",
        "849456",
        "1235",
        "1111",
        "2937",
};

static char buf[255];
static void construct_record(size_t attributes) {
    static char* name = "record_";

    int pos = 0;
    pos += sprintf(buf+pos, "%s {", name);

    for(size_t i=0; i<attributes; i++) {
        if(i > 0) {
            pos += sprintf(buf+pos, ", ");
        }

        size_t idx = my_random() % (sizeof(attrs) / sizeof(char*));
        pos += sprintf(buf+pos, "%s: ", attrs[idx]);

        idx = my_random() % (sizeof(values) / sizeof(char*));
        pos += sprintf(buf+pos, "%s", values[idx]);
    }

    pos += sprintf(buf+pos, "}");

}

static void construct_query(size_t attributes) {

    int pos = 0;
    pos += sprintf(buf+pos, "_ {");

    for(size_t i=0; i<attributes; i++) {
        if(i > 0) {
            pos += sprintf(buf+pos, ", ");
        }

        size_t idx = my_random() % (sizeof(attrs) / sizeof(char*));
        pos += sprintf(buf+pos, "%s: _", attrs[idx]);
    }

    pos += sprintf(buf+pos, "}");
}

static void add_workload(void) {

    size_t exps = sizeof(records) / sizeof(size_t);
    struct dist2_rpc_client* cl = get_dist_rpc_client();
    assert(cl != NULL);

    errval_t error_code;
    char* ret = NULL;

    char record[100];
    construct_record(2);
    strcpy(record, buf);
    printf("record: %s\n", record);

    char add_record[100];
    //construct_record(2);
    strcpy(add_record, buf);
    printf("add_record: %s\n", add_record);

    for (size_t i = 1; i < exps; i++) {
        printf("# Run add_workload with %lu records:\n", records[i]);

        for (size_t j = records[i - 1]; j < records[i]; j++) {
            //printf("add to system: %s\n", record);
            cl->vtbl.set(cl, record, SET_SEQUENTIAL, NOP_TRIGGER, false, &ret, &error_code);
            assert(ret == NULL);
            if(err_is_fail(error_code)) { DEBUG_ERR(error_code, "add"); exit(0); }
        }

        /*
        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            //printf("set: %s\n", add_record);
            timestamps[k].time0 = bench_tsc();
            cl->vtbl.set(cl, add_record, SET_SEQUENTIAL, NOP_TRIGGER, false, &ret, &error_code);
            timestamps[k].time1 = bench_tsc();
            assert(ret == NULL);
            if (err_is_fail(error_code)) { DEBUG_ERR(error_code, "measure add"); exit(0); };
        }

        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            printf("%lu %"PRIuCYCLES" %lu\n",
                    k,
                    timestamps[k].time1 - timestamps[k].time0 - bench_tscoverhead(),
                    records[i]);
        }*/
    }


}

static void no_name_get_workload(void)
{
    size_t exps = sizeof(records) / sizeof(size_t);
    for (size_t i = 1; i < exps; i++) {
        printf("# Run no_name_get_workload with %lu records:\n", records[i]);

        struct dist2_rpc_client* cl = get_dist_rpc_client();
        assert(cl != NULL);
        errval_t error_code;
        char* record;

        for (size_t j = records[i - 1]; j < records[i]; j++) {
            construct_record(5);
            cl->vtbl.set(cl, buf, SET_SEQUENTIAL, NOP_TRIGGER, false, &record, &error_code);
            if(err_is_fail(error_code)) { DEBUG_ERR(error_code, "set"); exit(0); }
        }

        // construct_query(2); ?
        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            construct_query(2);

            cycles_t server;
            uint8_t busy;

            //printf("get: %s\n", buf);
            timestamps[k].time0 = bench_tsc();
            cl->vtbl.get(cl, buf, NOP_TRIGGER, &record, &error_code,
                    &server, &busy);
            //printf("got: %s\n", record);
            timestamps[k].time1 = bench_tsc();
            if (err_is_fail(error_code) && err_no(error_code) != DIST2_ERR_NO_RECORD) {
                DEBUG_ERR(error_code, "get"); exit(0);
            }
            free(record);
        }

        for (size_t k = 0; k < MAX_ITERATIONS; k++) {
            printf("%lu %"PRIuCYCLES" %lu\n",
                    k,
                    timestamps[k].time1 - timestamps[k].time0 - bench_tscoverhead(),
                    records[i]);
        }

    }
}

int main(int argc, char** argv)
{
    dist_init();
    bench_init();

    if (0) no_name_get_workload();
    add_workload();

}
