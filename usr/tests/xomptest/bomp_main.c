/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>
#include <limits.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <bench/bench.h>
#include <omp.h>
#include <xomp/xomp.h>

#include <flounder/flounder_support_ump.h>

#include "xomptest.h"

static inline cycles_t calculate_time(cycles_t tsc_start,
                                      cycles_t tsc_end)
{
    cycles_t result;
    if (tsc_end < tsc_start) {
        result = (LONG_MAX - tsc_start) + tsc_end - bench_tscoverhead();
    } else {
        result = (tsc_end - tsc_start - bench_tscoverhead());
    }
    return result;
}

uint32_t asrc[MAX];
uint32_t adst[MAX];

int main(int argc,
                char *argv[])
{
    errval_t err;

    debug_printf("BOMP bench started. (MASTER) %u\n", argc);

    bench_init();

    debug_printf("Bomp init...\n");
    bomp_custom_init(NULL);

    debug_printf("spawning domains...%u\n", BOMP_NTHREADS);
    backend_span_domain(BOMP_NTHREADS, 0);

    debug_printf("setting threads...%u\n", BOMP_NTHREADS);
    omp_set_num_threads(BOMP_NTHREADS);

    cycles_t tsc_start, tsc_end;
    cycles_t result;
    uint64_t tscperus;
    bench_ctl_t *ctl;

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    debug_printf("tsc_perus: %lu\n", tscperus);


    debug_printf("BENCHMARK: single loop\n");
    debug_printf("=========================================================\n");
    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_N_RUNS);
    do {
        tsc_start = bench_tsc();
        do_process_single(asrc, adst);
        tsc_end = bench_tsc();
        result = calculate_time(tsc_start, tsc_end);
    }while (!bench_ctl_add_run(ctl, &result));

    bench_ctl_dump_analysis(ctl, 0, "single", tscperus);

    bench_ctl_destroy(ctl);

    debug_printf("=========================================================\n");

    debug_printf("BENCHMARK: omp loop\n");
    debug_printf("=========================================================\n");
    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_N_RUNS);
    do {
        tsc_start = bench_tsc();
        do_process(asrc, adst);
        tsc_end = bench_tsc();
        result = calculate_time(tsc_start, tsc_end);
    }while (!bench_ctl_add_run(ctl, &result));

    bench_ctl_dump_analysis(ctl, 0, "omp", tscperus);
    bench_ctl_destroy(ctl);

    debug_printf("=========================================================\n");

   /* debug_printf("Verifying");

    for (uint32_t i = 0; i < MAX; ++i) {
        if (adst[i] != asrc[i]) {
            USER_PANIC("test failed: data[%u]=%u, expected %u\n", i, adst[i], asrc[i]);
        }
    }

    debug_printf("SUCCESSS!!!!!\n");*/
    while (1) {
        messages_wait_and_handle_next();
    }

    return 0;
}
