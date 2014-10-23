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
#define BENCH_MEASURE_MAP_ONLY 0

#define BENCH_RUN_COUNT 250
#define BENCH_RUN_SINGLE 0

#define DEBUG(x...) debug_printf(x)

#define EXPECT_SUCCESS(errval, msg) \
    if (err_is_fail(err)) {USER_PANIC_ERR(err, msg);}

static uint32_t nthreads;

cycles_t timer_xompinit = 0;

#if BENCH_MEASURE_LOCAL
static bench_ctl_t *ctl_local;
#endif

#if BENCH_MEASURE_MAP_ONLY
static void measure_mapping(struct capref frame)
{
    errval_t err;

    debug_printf("\n");
    debug_printf("==========================================\n");
    debug_printf("Mapping of frame\n");

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    EXPECT_SUCCESS(err, "invoke_frame_identify");

    size_t frame_size = (1UL << id.bits);

    bench_ctl_t *b_ctl = NULL;
    cycles_t tsc_start, tsc_end, elapsed;

    b_ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);

    struct capref copy;
    err = slot_alloc(&copy);
    EXPECT_SUCCESS(err, "slot alloc");

    err = cap_copy(copy, frame);
    EXPECT_SUCCESS(err, "cap copy");

    do {

        void *addr;
        tsc_start = bench_tsc();
        err = vspace_map_one_frame(&addr, frame_size, copy, NULL, NULL);
        tsc_end = bench_tsc();
        EXPECT_SUCCESS(err, "vspace map one frame");

        err = vspace_unmap(addr);
        EXPECT_SUCCESS(err, "vspace unmap");
        elapsed = bench_time_diff(tsc_start, tsc_end);

    }while (!bench_ctl_add_run(b_ctl, &elapsed));

    debug_printf("\n");
    debug_printf("%% %lu bytes\n", frame_size);
    bench_ctl_dump_analysis(b_ctl, 0, "map local", bench_tsc_per_us());
    debug_printf("==========================================\n");
}
#endif

static void prepare_bomp(void)
{
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
        .path = "/k1om/sbin/benchmarks/xomp_share",
#else
        .path = "/x86_64/sbin/benchmarks/xomp_share",
#endif
	};

    struct xomp_spawn remote_info = {
        .argc = argc,
        .argv = argv,
        .path = "/k1om/sbin/benchmarks/xomp_share",
    };

    struct xomp_args xomp_arg = {
        .type = XOMP_ARG_TYPE_DISTINCT,
        .core_stride = 1,  // use default
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
    errval_t err, repl_err = SYS_ERR_OK;
    xomp_wid_t wid;

    cycles_t t_share, t_repl;

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
        debug_printf("Usage: %s  <numthreats>\n", argv[0]);
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

    lvaddr_t vbase = (10UL * 1024 * 1024 * 1024);

    DEBUG("\n");
    DEBUG("======================================================\n");
    DEBUG("sharing of 4k\n");

#define FRAME_SIZE_0 4096UL
#define FRAME_SIZE_1 (1024UL * 1024)
#define FRAME_SIZE_2 (32UL * 1024 * 1024)

    size_t frame_size;
    struct capref frame;


#if BENCH_MEASURE_MAP_ONLY
    measure_mapping(frame);
#else
    cycles_t tsc_start, tsc_end;

    cycles_t tscperus = bench_tsc_per_us();
#if 1
    bench_ctl_t *ctl_share_4k = NULL, *ctl_repl_4k = NULL;

    ctl_share_4k = bench_ctl_init(BENCH_MODE_FIXEDRUNS,1, BENCH_RUN_COUNT);
    ctl_repl_4k = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
    do {
        err = frame_alloc(&frame, FRAME_SIZE_0, &frame_size);
        EXPECT_SUCCESS(err, "frame alloc");
        assert(frame_size == FRAME_SIZE_0);

        tsc_start = bench_tsc();
        err = xomp_master_add_memory(frame, vbase, XOMP_FRAME_TYPE_SHARED_RW);
        tsc_end = bench_tsc();
        EXPECT_SUCCESS(err, "xomp_master_add_memory mA_shared\n");
        t_share = bench_time_diff(tsc_start, tsc_end);

        vbase += FRAME_SIZE_0;

        if (err_is_ok(err)) {
            tsc_start = bench_tsc();
            repl_err = xomp_master_add_memory(frame, vbase, XOMP_FRAME_TYPE_REPL_RW);
            tsc_end = bench_tsc();
            if (err_is_ok(err)) {
                t_repl = bench_time_diff(tsc_start, tsc_end);
                vbase += FRAME_SIZE_0;
                bench_ctl_add_run(ctl_repl_4k, &t_repl);
            }
        }
    } while (!bench_ctl_add_run(ctl_share_4k, &t_share));


    bench_ctl_dump_analysis(ctl_share_4k, 0, "4k shared", tscperus);
    bench_ctl_dump_analysis(ctl_repl_4k, 0, "4k repl", tscperus);
#endif
#endif
    DEBUG("\n");
    DEBUG("======================================================\n");
    DEBUG("sharing of 1M\n");

    err = frame_alloc(&frame, FRAME_SIZE_1, &frame_size);
    EXPECT_SUCCESS(err, "frame alloc");
    assert(frame_size == FRAME_SIZE_1);

    vbase = (vbase + FRAME_SIZE_1) & ~(FRAME_SIZE_1 - 1);

#if BENCH_MEASURE_MAP_ONLY
    measure_mapping(frame);
#else
#if 1
    bench_ctl_t *ctl_share_1M, *ctl_repl_1M;
    ctl_share_1M = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
    ctl_repl_1M = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
    do {
        err = frame_alloc(&frame, FRAME_SIZE_1, &frame_size);
        EXPECT_SUCCESS(err, "frame alloc");
        assert(frame_size == FRAME_SIZE_1);

        tsc_start = bench_tsc();
        err = xomp_master_add_memory(frame, vbase, XOMP_FRAME_TYPE_SHARED_RW);
        tsc_end = bench_tsc();
        EXPECT_SUCCESS(err, "xomp_master_add_memory mA_shared\n");
        t_share = bench_time_diff(tsc_start, tsc_end);
        vbase += FRAME_SIZE_1;

        if (err_is_ok(repl_err)) {
            tsc_start = bench_tsc();
            repl_err = xomp_master_add_memory(frame, vbase, XOMP_FRAME_TYPE_REPL_RW);
            tsc_end = bench_tsc();
            if(err_is_ok(err)) {
                EXPECT_SUCCESS(err, "xomp_master_add_memory mA_shared\n");
                t_repl = bench_time_diff(tsc_start, tsc_end);
                bench_ctl_add_run(ctl_repl_1M, &t_repl);
            }
            vbase += FRAME_SIZE_1;
        }
    } while (!bench_ctl_add_run(ctl_share_1M, &t_share));
    bench_ctl_dump_analysis(ctl_share_1M, 0, "1M shared", tscperus);
    bench_ctl_dump_analysis(ctl_repl_1M, 0, "1M repl", tscperus);
#endif
#endif
    DEBUG("\n");
    DEBUG("======================================================\n");
    DEBUG("sharing of 256M\n");

    err = frame_alloc(&frame, FRAME_SIZE_2, &frame_size);
    EXPECT_SUCCESS(err, "frame alloc");
    assert(frame_size == FRAME_SIZE_2);

#if BENCH_MEASURE_MAP_ONLY
    measure_mapping(frame);
#else

    vbase = (vbase + FRAME_SIZE_2) & ~(FRAME_SIZE_2 - 1);

    bench_ctl_t *ctl_share_256M = NULL;
    bench_ctl_t *ctl_repl_256M = NULL;
    uint32_t counter = 0;

    ctl_share_256M = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
    ctl_repl_256M = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
    do {
        tsc_start = bench_tsc();
        err = xomp_master_add_memory(frame, vbase, XOMP_FRAME_TYPE_SHARED_RW);
        tsc_end = bench_tsc();
        EXPECT_SUCCESS(err, "xomp_master_add_memory mA_shared\n");
        t_share = bench_time_diff(tsc_start, tsc_end);

        vbase += FRAME_SIZE_2;


        if (err_is_ok(repl_err)) {
            tsc_start = bench_tsc();
            repl_err = xomp_master_add_memory(frame, vbase, XOMP_FRAME_TYPE_REPL_RW);
            if (err_is_ok(repl_err)) {
                tsc_end = bench_tsc();
                t_repl = bench_time_diff(tsc_start, tsc_end);
                bench_ctl_add_run(ctl_repl_256M, &t_repl);
            }
            vbase += FRAME_SIZE_2;
        } 
        counter ++;
    } while (!bench_ctl_add_run(ctl_share_256M, &t_share));
    bench_ctl_dump_analysis(ctl_share_256M, 0, "256M shared", tscperus);
    bench_ctl_dump_analysis(ctl_repl_256M, 0, "256M repl", tscperus);
#endif
    debug_printf("-------------------------------------\n");

#if XOMP_BENCH_ENABLED
    xomp_master_bench_print_results();
#endif

    while (1)
        ;

}

