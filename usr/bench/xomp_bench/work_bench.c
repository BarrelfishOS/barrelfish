/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/dispatch.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_domain.h>

#include <bench/bench.h>
#include <xomp/xomp.h>

#define BENCH_MEASURE_LOCAL 0

#define BENCH_RUN_COUNT 5000

#define BENCH_STEP_SIZE 10

#define DEBUG(x...) debug_printf(x)

#define EXPECT_SUCCESS(errval, msg) \
    if (err_is_fail(err)) {USER_PANIC_ERR(err, msg);}

static uint32_t nthreads;

static cycles_t timer_xompinit;

static volatile int counter = 0;

static void work_omp(void)
{
#pragma omp parallel
    {
        int num_threads = omp_get_num_threads() * 2;
#pragma omp  for nowait schedule (static, 2)
        for (int i = 0; i < num_threads; i++) {
            counter++;
            //debug_printf("thread: %u\n", omp_get_thread_num());
        }
    }
}

#ifndef __k1om__
static void prepare_bomp(void)
{
    cycles_t tsc_start = bench_tsc();
    bomp_bomp_init(nthreads);
    cycles_t tsc_end = bench_tsc();
    timer_xompinit = bench_time_diff(tsc_start, tsc_end);
}
#endif

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
#if XOMP_BENCH_ENABLED
        xomp_master_bench_enable(BENCH_RUN_COUNT, nthreads,
                        XOMP_MASTER_BENCH_DO_WORK);
#endif
        debug_printf("waiting for xeon phi to be ready\n");
        err = xeon_phi_domain_blocking_lookup("xeon_phi.0.ready", NULL);
        EXPECT_SUCCESS(err, "nameservice_blocking_lookup");
        err = xeon_phi_domain_blocking_lookup("xeon_phi.1.ready", NULL);
        EXPECT_SUCCESS(err, "nameservice_blocking_lookup");
    }

    struct xomp_spawn local_info = {
        .argc = argc,
        .argv = argv,
#ifdef __k1om__
        .path = "/k1om/sbin/benchmarks/xomp_work",
#else
		.path = "/x86_64/sbin/benchmarks/xomp_work",
#endif
    };

    struct xomp_spawn remote_info = {
        .argc = argc,
        .argv = argv,
        .path = "/k1om/sbin/benchmarks/xomp_work",
    };

    struct xomp_args xomp_arg = {
        .type = XOMP_ARG_TYPE_DISTINCT,
        .core_stride = (location == XOMP_WORKER_LOC_LOCAL) ? 1 : 2,
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

    cycles_t tsc_start, tsc_end;

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

    if (argc < 3) {
        debug_printf("Usage: %s <numthreads>\n", argv[0]);
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

    uint8_t is_bomp = 0;
#ifdef __k1om__
    if (disp_xeon_phi_id()) {
        prepare_xomp(argc, argv);
    } else {
        prepare_xomp(argc, argv);
    }

#else
    for (int i = 2; i < argc; ++i) {
        if (!strcmp(argv[i], "bomp")) {
            prepare_bomp();
            is_bomp = 1;
        } else if (!strcmp(argv[i], "xomp")) {
            prepare_xomp(argc, argv);
        } else {
            debug_printf("ignoring argument {%s}\n", argv[i]);
        }
    }
#endif

    DEBUG("\n");
    DEBUG("======================================================\n");
    DEBUG("work_omp\n");

    cycles_t tscperus = bench_tsc_per_us();

    bench_ctl_t *ctl_omp;

    cycles_t timer_omp = 0;
    char buf[20];

#ifdef __k1om__
    uint8_t do_work = !disp_xeon_phi_id();
#endif

    for (uint32_t i = 1; i <= nthreads; ++i) {
        if (i % BENCH_STEP_SIZE) {
            if (i != nthreads && i != 2) {
                continue;
            }
#ifdef __k1om__
            if (!do_work) {
                do_work = !do_work;
                continue;
            }
#endif
        }

#ifdef __k1om__
    do_work = !do_work;
#endif
        omp_set_num_threads(i);

        ctl_omp = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
        do {
            tsc_start = bench_tsc();
            work_omp();
            tsc_end = bench_tsc();
            timer_omp = bench_time_diff(tsc_start, tsc_end);
            if (is_bomp) {
#ifdef __k1om__
                for (uint32_t j = 0; j < 500 * i; ++j) {
                    thread_yield();
                }
#else

                for (uint32_t j = 0; j < 1000 * i; ++j) {
                    thread_yield();
                }

#endif
            }
        } while (!bench_ctl_add_run(ctl_omp, &timer_omp));

        snprintf(buf, 20, "threads=%u", i);

        bench_ctl_dump_analysis(ctl_omp, 0, buf, tscperus);

        bench_ctl_destroy(ctl_omp);
    }

    debug_printf("-------------------------------------\n");

#if XOMP_BENCH_ENABLED
    xomp_master_bench_print_results();
#endif
    debug_printf("-------------------------------------\n");

    while (1)
        ;

}

