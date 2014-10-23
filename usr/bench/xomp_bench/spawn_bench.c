/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>
#include <stdlib.h>
#include <omp.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_domain.h>

#include <bench/bench.h>
#include <xomp/xomp.h>

#define BENCH_MEASURE_LOCAL 0

#define BENCH_RUN_COUNT 25
#define BENCH_RUN_SINGLE 0

#define DEBUG(x...) debug_printf(x)

#define EXPECT_SUCCESS(errval, msg) \
    if (err_is_fail(err)) {USER_PANIC_ERR(err, msg);}

static uint32_t nthreads;

static cycles_t timer_xompinit;

#define SCHEDULE static
#define CHUNK    25

#define MATRIX_TYPE uint64_t
#define MATRIX_ROWS 1000
#define MATRIX_COLS 1000
#define MATRIX_ELEMENTS (MATRIX_ROWS * MATRIX_COLS)

static void prepare_bomp(void)
{
    debug_printf("prepare_bomp\n");
    cycles_t tsc_start = bench_tsc();
    bomp_bomp_init(nthreads);
    cycles_t tsc_end = bench_tsc();
    timer_xompinit = bench_time_diff(tsc_start, tsc_end);
}

static int prepare_xomp(int argc,
                        char *argv[])
{
    errval_t err;

    xomp_wloc_t location = XOMP_WORKER_LOC_MIXED;
    for (int i = 3; i < argc; ++i) {
        if (!strncmp(argv[i], "--location=", 11)) {
            char *p = strchr(argv[i], '=');
            p++;
            if (!strcmp(p, "local")) {
                location = XOMP_WORKER_LOC_LOCAL;
            }
        }
    }

    if (location == XOMP_WORKER_LOC_MIXED) {
        debug_printf("waiting for xeon phi to be ready\n");
        err = xeon_phi_domain_blocking_lookup("xeon_phi.0.ready", NULL);
        EXPECT_SUCCESS(err, "nameservice_blocking_lookup");
        err = xeon_phi_domain_blocking_lookup("xeon_phi.1.ready", NULL);
        EXPECT_SUCCESS(err, "nameservice_blocking_lookup");

#if XOMP_BENCH_ENABLED
        xomp_master_bench_enable(BENCH_RUN_COUNT, nthreads,
                        XOMP_MASTER_BENCH_MEM_ADD);
#endif
    }

    struct xomp_spawn local_info = {
        .argc = argc,
        .argv = argv,
#ifdef __k1om__
        .path = "/k1om/sbin/benchmarks/bomp_mm",
#else
                    .path = "/x86_64/sbin/benchmarks/bomp_mm",
#endif
                };

    struct xomp_spawn remote_info = {
        .argc = argc,
        .argv = argv,
        .path = "/k1om/sbin/benchmarks/bomp_mm",
    };

    struct xomp_args xomp_arg = {
        .type = XOMP_ARG_TYPE_DISTINCT,
        .core_stride = 0,  // use default
        .args = {
            .distinct = {
                .nthreads = nthreads,
                .worker_loc = location,
                .nphi = 2,
                .local = local_info,
                .remote = remote_info
            }
        }
    };

    cycles_t tsc_start = bench_tsc();
    if (bomp_xomp_init(&xomp_arg)) {
        debug_printf("bomp init failed!\n");
        exit(1);
    }
    cycles_t tsc_end = bench_tsc();
    timer_xompinit = bench_time_diff(tsc_start, tsc_end);

    return (location == XOMP_WORKER_LOC_LOCAL);
}

int main(int argc,
         char *argv[])
{
    errval_t err;
    xomp_wid_t wid;

    bench_init();

    err = xomp_worker_parse_cmdline(argc, argv, &wid);
    if (err_is_ok(err)) {
        struct xomp_args xw_arg = {
            .type = XOMP_ARG_TYPE_WORKER,
            .args = {
                .worker = {
                    .id = wid
                }
            }
        };
        bomp_xomp_init(&xw_arg);
    }

    if (argc < 4) {
        debug_printf("Usage: %s <size> <numthreats>\n", argv[0]);
        exit(1);
    }

    nthreads = strtoul(argv[1], NULL, 10);
    if (nthreads == 0) {
        debug_printf("num threads must be >0\n");
        exit(1);
    }

    DEBUG("\n");
    DEBUG("======================================================\n");
    debug_printf("Num Threads: %u\n", nthreads);

    uint8_t is_shared = 0;
    for (int i = 2; i < argc; ++i) {
        if (!strcmp(argv[i], "bomp")) {
            prepare_bomp();
            is_shared = 1;
        } else if (!strcmp(argv[i], "xomp")) {
            is_shared = prepare_xomp(argc, argv);
        } else {
            debug_printf("ignoring argument {%s}\n", argv[i]);
        }
    }

    debug_printf("-------------------------------------\n");
    debug_printf("init time: %lu\n", timer_xompinit);
    debug_printf("-------------------------------------\n");
#if XOMP_BENCH_ENABLED
    xomp_master_bench_print_results();
#endif
    while (1)
        ;

}

