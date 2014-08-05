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


static uint32_t *asrc = NULL;
static uint32_t *adst = NULL;

#ifndef __k1om__

static errval_t initialize_master(int argc,
                char *argv[])
{
    errval_t err;
    debug_printf("Initializing Master: elements:%u\n", MAX);

    struct capref frame;
    err = frame_alloc(&frame, WORK_SIZE, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    void *addr;
    err = vspace_map_one_frame(&addr, WORK_SIZE, frame, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    err = xomp_master_add_memory(frame, (lvaddr_t) addr, XOMP_FRAME_TYPE_SHARED_RW);
    if (err_is_fail(err)) {
        return err;
    }

    asrc = addr;
    adst = asrc + MAX;

    for (uint32_t i = 0; i < MAX; ++i) {
        asrc[i] = i;
        adst[i] = 0;
    }

    debug_printf("Initialization done.\n");

    return SYS_ERR_OK;
#if 0
    debug_printf("=========================================================\n");
    debug_printf("Distributing work:\n");

    err = xomp_master_process_array((lvaddr_t) do_process, (lvaddr_t) addr, NUM_WORKERS);
    if (err_is_fail(err)) {
        return err;
    }
    debug_printf("=========================================================\n");
    debug_printf("Verification:\n");

    for (uint32_t i = 0; i < (WORK_SIZE / sizeof(uint32_t)) - 2; ++i) {
        if (data[i] != i + 1) {
            USER_PANIC("test failed: data[%u]=%u, expected %u\n", i, data[i], i + 1);
        }
    }
    debug_printf("=========================================================\n");
    debug_printf("SUCCESS!:\n");
    debug_printf("=========================================================\n");

    return SYS_ERR_OK;
#endif
}

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
#endif


#ifdef __k1om__
int main(int argc,
         char *argv[])
{
    errval_t err;

    debug_printf("XOMP worker started.\n");

    xomp_wid_t wid;
    err = xomp_worker_parse_cmdline(argc, argv, &wid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not parse the command line");
    }

    err = xomp_worker_init(wid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the worker\n");
    }

    do_process(asrc, adst);

    while (1) {
        messages_wait_and_handle_next();
    }

}
#else
int main(int argc,
                char *argv[])
{
    errval_t err;

    debug_printf("XOMP Test started. (MASTER) %u\n", argc);

    bench_init();

    struct xomp_master_args args = {
        .num_phi = 2,
        .path = "/k1om/sbin/xomp_test",
        .argc = argc,
        .argv = argv
    };

    bomp_custom_init(&args);

    backend_span_domain(NTHREADS, 0);

    omp_set_num_threads(NTHREADS);

    err = initialize_master(argc, argv);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "initializing mastser");
    }

    cycles_t tsc_start, tsc_end;
    cycles_t result;
    uint64_t tscperus;
    bench_ctl_t *ctl;

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;


    debug_printf("BENCHMARK: single loop\n");
    debug_printf("=========================================================\n");
    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_N_RUNS);
    do {
        tsc_start = bench_tsc();
        for (int j=0; j<IT; j++) {
            for (int i=0; i<MAX; i+=IT) {
                adst[i+j] = asrc[i+j];
            }
        }
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
#endif
