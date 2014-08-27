/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <limits.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish/sys_debug.h>
#include <bench/bench.h>
#include <omp.h>
#include <xomp/xomp.h>


#include "xomptest.h"

static uint32_t *asrc = NULL;
static uint32_t *adst = NULL;

static errval_t initialize_master(int argc,
                                  char *argv[])
{
    errval_t err;
    debug_printf("Initializing Master: elements:%lu\n", MAX);

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

    err = xomp_master_add_memory(frame, (lvaddr_t) addr,
                                 XOMP_FRAME_TYPE_SHARED_RW);
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

errval_t start_master(int argc,
                      char *argv[])
{
    errval_t err;

    bench_init();
    struct xomp_args args = {
        .type = XOMP_ARG_TYPE_UNIFORM,
        .core_stride = 0,
        .args = {
            .uniform = {
                .nthreads = 0,
                .worker_loc = XOMP_WORKER_LOC_MIXED,
                .nphi = 2,
                .argc = argc,
                .argv = argv
            }
        }
    };

    bomp_xomp_init(&args);

    omp_set_num_threads(NTHREADS);

    err = initialize_master(argc, argv);
    if (err_is_fail(err)) {
        return err;
    }

    cycles_t tsc_start, tsc_end;
    cycles_t result;
    uint64_t tscperus;
    bench_ctl_t *ctl;

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    uint32_t run = 1;

    debug_printf("Benchmark: working set size: 2x %lu kB\n",
                 (WORK_SIZE / 2) / 1024);
    debug_printf("BENCHMARK: single loop\n");
    debug_printf("=========================================================\n");
    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_N_RUNS);
    do {
        if (!(run % 10)) {
            debug_printf("  run: %u\n", run);
        }
        tsc_start = bench_tsc();
        for (int j = 0; j < IT; j++) {
            for (int i = 0; i < MAX; i += IT) {
                adst[i + j] = asrc[i + j];
            }
        }
        tsc_end = bench_tsc();
        result = calculate_time(tsc_start, tsc_end);
        run++;
    } while (!bench_ctl_add_run(ctl, &result));

    bench_ctl_dump_analysis(ctl, 0, "single", tscperus);

    bench_ctl_destroy(ctl);

    debug_printf("=========================================================\n");

    run = 1;
    debug_printf("BENCHMARK: omp loop\n");
    debug_printf("=========================================================\n");
    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_N_RUNS);
    do {
        for (uint32_t i = 0; i < MAX; ++i) {
            adst[i] = 0;
        }
        if (!(run % 10)) {
            debug_printf("  run: %u\n", run);
        }
        tsc_start = bench_tsc();
        do_process(asrc, adst);
        tsc_end = bench_tsc();
        result = calculate_time(tsc_start, tsc_end);
        run++;
        for (uint32_t i = 0; i < MAX; ++i) {
            if (adst[i] != asrc[i]) {
                USER_PANIC("test failed: data[%u]=%u, expected %u\n", i, adst[i],
                       asrc[i]);
            }
        }
    } while (!bench_ctl_add_run(ctl, &result));

    bench_ctl_dump_analysis(ctl, 0, "omp", tscperus);
    bench_ctl_destroy(ctl);

    debug_printf("=========================================================\n");

    debug_printf("Verifying\n");



    debug_printf("SUCCESSS!!!!!\n");
    return SYS_ERR_OK;
}

void handle_messages(void)
{
    while(1) {
        messages_wait_and_handle_next();
    }
}

void do_process_single(uint32_t *src,
                       uint32_t *dst)
{
    for (int j = 0; j < IT; j++) {
        for (int i = 0; i < MAX; i += IT) {
            dst[i + j] = src[i + j];
        }
    }
}
