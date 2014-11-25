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

#define SCHEDULE static
#define CHUNK    25

#define MATRIX_TYPE uint64_t
#define MATRIX_ROWS 1000
#define MATRIX_COLS 1000
#define MATRIX_ELEMENTS (MATRIX_ROWS * MATRIX_COLS)

static MATRIX_TYPE *mA_repl;
static MATRIX_TYPE *mB_repl;
static MATRIX_TYPE *mC_repl;

static MATRIX_TYPE *mA_shared;
static MATRIX_TYPE *mB_shared;
static MATRIX_TYPE *mC_shared;

struct capref frame_mAB;
struct capref frame_mC;

struct mm_frame {
    MATRIX_TYPE sum;
    uint64_t rows;
    uint64_t cols;
};

static struct mm_frame *g_mm_frame;
struct capref frame_mm_frame;

MATRIX_TYPE g_sum;

#define RAM_MIN_BASE (128ULL * 1024 * 1024 * 1024)
#define RAM_MAX_LIMIT (512ULL * 1024 * 1024 * 1024)

/*
 * benchmark timers
 */
cycles_t timer_xompinit = 0;
cycles_t timer_share = 0;

static lvaddr_t vbase_map = (128ULL * 1024 * 1024 * 1024);

static void matrix_alloc(uint64_t rows,
                         uint64_t cols)
{
    errval_t err;

#ifndef __k1om__
    uint64_t min_base, max_limit;

    ram_get_affinity(&min_base, &max_limit);
    ram_set_affinity(RAM_MIN_BASE, RAM_MAX_LIMIT);

#endif
    size_t matrix_size = (rows * cols) * sizeof(MATRIX_TYPE);

    err = frame_alloc(&frame_mAB, 2 * matrix_size, &matrix_size);
    EXPECT_SUCCESS(err, "frame allocate\n");

    err = vspace_map_one_frame_fixed(vbase_map, matrix_size, frame_mAB, NULL, NULL);
    EXPECT_SUCCESS(err, "vspace_map_one_frame\n");

    mA_repl = (void *)vbase_map;
    mB_repl = (void *)(vbase_map + ((rows * cols) * sizeof(MATRIX_TYPE)));

    debug_printf("frameAB: [%lx, %lx]\n", vbase_map, vbase_map + matrix_size);

    vbase_map += matrix_size;

    struct capref copy;
    err = slot_alloc(&copy);
    EXPECT_SUCCESS(err, "slot_alloc\n");

    err = cap_copy(copy, frame_mAB);
    EXPECT_SUCCESS(err, "cap_copy\n");

    err = vspace_map_one_frame_fixed(vbase_map, matrix_size, copy, NULL, NULL);
    EXPECT_SUCCESS(err, "vspace_map_one_frame\n");

    mA_shared = (void *)vbase_map;
    mB_shared = (void *)(vbase_map + ((rows * cols) * sizeof(MATRIX_TYPE)));

    debug_printf("sharedAB: [%lx, %lx]\n", vbase_map, vbase_map + matrix_size);

    vbase_map += matrix_size;

    matrix_size = (rows * cols) * sizeof(MATRIX_TYPE);

    err = frame_alloc(&frame_mC, matrix_size, &matrix_size);
    EXPECT_SUCCESS(err, "frame allocate\n");

    err = vspace_map_one_frame_fixed(vbase_map, matrix_size, frame_mC, NULL, NULL);
    EXPECT_SUCCESS(err, "vspace_map_one_frame\n");

    mC_repl = (void *)vbase_map;

    debug_printf("frameC: [%lx, %lx]\n", vbase_map, vbase_map + matrix_size);

    vbase_map += matrix_size;

    err = slot_alloc(&copy);
    EXPECT_SUCCESS(err, "slot_alloc\n");

    err = cap_copy(copy, frame_mC);
    EXPECT_SUCCESS(err, "cap_copy\n");

    err = vspace_map_one_frame_fixed(vbase_map, matrix_size, copy, NULL, NULL);
    EXPECT_SUCCESS(err, "vspace_map_one_frame\n");

    mC_shared = (void *)vbase_map;

    debug_printf("sharedC: [%lx, %lx]\n", vbase_map, vbase_map + matrix_size);

    vbase_map += matrix_size;

    size_t mm_frame_size = sizeof(struct mm_frame);
    err = frame_alloc(&frame_mm_frame, mm_frame_size, &mm_frame_size);
    EXPECT_SUCCESS(err, "frame allocate\n");

    err = vspace_map_one_frame_fixed(vbase_map, mm_frame_size, frame_mm_frame, NULL, NULL);
    EXPECT_SUCCESS(err, "vspace_map_one_frame\n");

    g_mm_frame = (void *)vbase_map;

    vbase_map += mm_frame_size;

    g_mm_frame->cols = cols;
    g_mm_frame->rows = rows;
    g_mm_frame->sum = 0;

#ifndef __k1om__
    ram_set_affinity(min_base, max_limit);
#endif
}

static void matrix_init(void)
{
    debug_printf("g_mm %p, %p, %p\n", g_mm_frame, mA_repl, mB_repl);
    for (int i = 0; i < g_mm_frame->rows; i++) {
        uint64_t row = i * g_mm_frame->rows;
        MATRIX_TYPE *a_row = mA_repl + (row);
        MATRIX_TYPE *b_row = mB_shared + (row);
        for (int j = 0; j < g_mm_frame->cols; j++) {
            if (j == i) {
                a_row[j] = 1;
                b_row[j] = 1;
            } else {
                a_row[j] = 0;
                b_row[j] = 0;
            }
        }
    }
}

static void matrix_share(xomp_wloc_t location)
{
    errval_t err;
    DEBUG("==========================================================\n");
    DEBUG("frame_mAB, (lvaddr_t) mA_shared, XOMP_FRAME_TYPE_SHARED_RW\n");
    err = xomp_master_add_memory(frame_mAB, (lvaddr_t) mA_shared,
                                 XOMP_FRAME_TYPE_SHARED_RW);
    EXPECT_SUCCESS(err, "xomp_master_add_memory mA_shared\n");

    DEBUG("==========================================================\n");
    DEBUG("frame_mC, (lvaddr_t) mC_shared, XOMP_FRAME_TYPE_SHARED_RW\n");
    err = xomp_master_add_memory(frame_mC, (lvaddr_t) mC_shared,
                                 XOMP_FRAME_TYPE_SHARED_RW);
    EXPECT_SUCCESS(err, "xomp_master_add_memory mC_shared\n");

    DEBUG("==========================================================\n");
    DEBUG("frame_mm_frame, (lvaddr_t) g_mm_frame, XOMP_FRAME_TYPE_SHARED_RW\n");
    err = xomp_master_add_memory(frame_mm_frame, (lvaddr_t) g_mm_frame,
                                 XOMP_FRAME_TYPE_SHARED_RW);
    EXPECT_SUCCESS(err, "xomp_master_add_memory g_mm_frame\n");

    if (location == XOMP_WORKER_LOC_MIXED) {
        DEBUG("==========================================================\n");
        DEBUG("frame_mAB, (lvaddr_t) mA_repl, XOMP_FRAME_TYPE_REPL_RW\n");
        err = xomp_master_add_memory(frame_mAB, (lvaddr_t) mA_repl,
                                     XOMP_FRAME_TYPE_REPL_RW);
        EXPECT_SUCCESS(err, "xomp_master_add_memory mA_repl\n");

        DEBUG("==========================================================\n");
        DEBUG("frame_mC, (lvaddr_t) mC_repl, XOMP_FRAME_TYPE_REPL_RW\n");
        err = xomp_master_add_memory(frame_mC, (lvaddr_t) mC_repl,
                                     XOMP_FRAME_TYPE_REPL_RW);
        EXPECT_SUCCESS(err, "xomp_master_add_memory mC_repl\n");
    }
}

#if BENCH_RUN_SINGLE
static void mm_single(MATRIX_TYPE *a,
                      MATRIX_TYPE *b,
                      MATRIX_TYPE *c)
{
    g_sum = 0;
    for (int i = 0; i < g_mm_frame->rows; i++) {
        uint64_t row = i * g_mm_frame->rows;
        MATRIX_TYPE *c_row_i = c + row;
        MATRIX_TYPE *a_row_i = a + row;
        for (int k = 0; k < g_mm_frame->rows; ++k) {
            MATRIX_TYPE *b_row_k = b + k * g_mm_frame->rows;
            MATRIX_TYPE a_elem = a_row_i[k];
            for (int j = 0; j < g_mm_frame->cols; ++j) {
                c_row_i[j] += a_elem * b_row_k[j];
            }
        }

        for (int j = 0; j < g_mm_frame->cols; ++j) {
            g_sum += c_row_i[j];
        }
    }
}
#endif


#if BENCH_MEASURE_LOCAL
static bench_ctl_t *ctl_local;
#endif

static void mm_init(void)
{
#pragma omp parallel
    {
        debug_printf("initializing omp library\n");
        bench_init();
    }
}

static void mm_omp(MATRIX_TYPE *a,
                   MATRIX_TYPE *b,
                   MATRIX_TYPE *c,
                   struct mm_frame *mm_frame)
{
#pragma omp parallel
    {
        uint64_t nrows = mm_frame->rows;
        uint64_t ncols = mm_frame->cols;
        uint64_t counter = 0;
        MATRIX_TYPE sum = 0;
#if BENCH_MEASURE_LOCAL
        if (ctl_local == NULL) {
            ctl_local = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
        }
        cycles_t start = bench_tsc();
#endif
#pragma omp for nowait schedule (static, 1)
        for (int i = 0; i < nrows; i++) {
            counter++;
            uint64_t row = i * nrows;
            MATRIX_TYPE *c_row_i = c + row;
            MATRIX_TYPE *a_row_i = a + row;
            for (int k = 0; k < nrows; ++k) {
                MATRIX_TYPE *b_row_k = b + k * nrows;
                MATRIX_TYPE a_elem = a_row_i[k];
                for (int j = 0; j < ncols; ++j) {
                    c_row_i[j] += a_elem * b_row_k[j];
                }
            }

            for (int j = 0; j < ncols; ++j) {
                sum += c_row_i[j] > 0;
            }
        }

        // TODO: atomic add;
        __sync_fetch_and_add(&mm_frame->sum, sum);
#if BENCH_MEASURE_LOCAL
        cycles_t end = bench_tsc();
        cycles_t elapsed = bench_time_diff(start, end);
        if (bench_ctl_add_run(ctl_local, &elapsed)) {
            bench_ctl_dump_analysis(ctl_local, 0, "LOCAL", bench_tsc_per_us());
        }
#endif
    }
}

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
        xomp_master_bench_enable(BENCH_RUN_COUNT, nthreads, XOMP_MASTER_BENCH_DO_WORK);
#endif
    }

    struct xomp_spawn local_info =  {
        .argc = argc,
        .argv = argv,
#ifdef __k1om__
        .path = "/k1om/sbin/benchmarks/bomp_mm",
#else
        .path = "/x86_64/sbin/benchmarks/bomp_mm",
#endif
    };

    struct xomp_spawn remote_info =  {
            .argc = argc,
            .argv = argv,
            .path = "/k1om/sbin/benchmarks/bomp_mm",
        };

    struct xomp_args xomp_arg = {
        .type = XOMP_ARG_TYPE_DISTINCT,
        .core_stride = 0, // use default
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

    tsc_start = bench_tsc();
    matrix_share(location);
    tsc_end = bench_tsc();
    timer_share = bench_time_diff(tsc_start, tsc_end);

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

    if (argc < 4) {
        debug_printf("Usage: %s <size> <numthreats>\n", argv[0]);
        exit(1);
    }

    uint64_t rows, cols;
    rows = strtoul(argv[1], NULL, 10);
    cols = rows;

    nthreads = strtoul(argv[2], NULL, 10);
    if (nthreads == 0) {
        debug_printf("num threads must be >0\n");
        exit(1);
    }

    DEBUG("\n");
    DEBUG("======================================================\n");
    debug_printf("Matrix Size: [%lu x %lu]\n", rows, cols);
    debug_printf("Matrix Size: %lu kB\n", rows * cols * sizeof(MATRIX_TYPE) >> 10);
    debug_printf("Num Threads: %u\n", nthreads);

    DEBUG("\n");
    DEBUG("======================================================\n");
    DEBUG("matrix_alloc\n");
    tsc_start = bench_tsc();
    matrix_alloc(rows, cols);
    tsc_end = bench_tsc();
    cycles_t timer_alloc = bench_time_diff(tsc_start, tsc_end);

    DEBUG("\n");
    DEBUG("======================================================\n");
    DEBUG("matrix_init\n");
    tsc_start = bench_tsc();
    matrix_init();
    tsc_end = bench_tsc();
    cycles_t timer_init = bench_time_diff(tsc_start, tsc_end);

    uint8_t is_shared = 0;
    for (int i = 3; i < argc; ++i) {
        if (!strcmp(argv[i], "bomp")) {
            prepare_bomp();
            is_shared = 1;
        } else if (!strcmp(argv[i], "xomp")) {
            is_shared = prepare_xomp(argc, argv);
        } else {
            debug_printf("ignoring argument {%s}\n", argv[i]);
        }
    }
    if (0) {
        mm_init();
    }
#if BENCH_RUN_SINGLE
    DEBUG("\n");
    DEBUG("======================================================\n");
    DEBUG("mm_single\n");

    bench_ctl_t *ctl_single;
    cycles_t timer_single;

    ctl_single = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
    do {
        tsc_start = bench_tsc();
        //mm_single(mA_repl, mB_repl, mC_repl);
        mm_single(mA_shared, mB_shared, mC_shared);
        tsc_end = bench_tsc();
        timer_single = bench_time_diff(tsc_start, tsc_end);
        memset(mC_shared, 0, rows * cols * sizeof(MATRIX_TYPE));
    } while (!bench_ctl_add_run(ctl_single, &timer_single));
#endif
    DEBUG("\n");
    DEBUG("======================================================\n");
    DEBUG("mm_omp_repl\n");

    bench_ctl_t *ctl_omp_repl = NULL;
    cycles_t timer_omp_repl = 0;

    if (!is_shared) {
        ctl_omp_repl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
        do {
            tsc_start = bench_tsc();
            mm_omp(mA_repl, mB_repl, mC_repl, g_mm_frame);
            tsc_end = bench_tsc();
            timer_omp_repl = bench_time_diff(tsc_start, tsc_end);
            debug_printf("took: %lu cycles\n", timer_omp_repl);
            if (g_mm_frame->sum != g_mm_frame->rows) {
                debug_printf("ERROR: sum was not identical: %lu / %lu\n", g_mm_frame->sum,
                             g_mm_frame->rows);
            }
            g_mm_frame->sum = 0;
        } while (!bench_ctl_add_run(ctl_omp_repl, &timer_omp_repl));
    }
    DEBUG("\n");
    DEBUG("======================================================\n");
    DEBUG("mm_omp_shared\n");

    //memset(mC_shared, 0, rows * cols * sizeof(MATRIX_TYPE));
    bench_ctl_t *ctl_omp_shared;
    cycles_t timer_omp_shared = 0;

    ctl_omp_shared = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
    do {
        tsc_start = bench_tsc();
        mm_omp(mA_shared, mB_shared, mC_shared, g_mm_frame);
        tsc_end = bench_tsc();
        timer_omp_shared = bench_time_diff(tsc_start, tsc_end);
        if (g_mm_frame->sum != g_mm_frame->rows) {
            debug_printf("ERROR: sum was not identical: %lu / %lu\n", g_mm_frame->sum,
                         g_mm_frame->rows);
        }

        g_mm_frame->sum = 0;
        debug_printf("took: %lu cycles\n", timer_omp_shared);
        memset(mC_shared, 0, rows * cols * sizeof(MATRIX_TYPE));
    } while (!bench_ctl_add_run(ctl_omp_shared, &timer_omp_shared));

    debug_printf("-------------------------------------\n");

    debug_printf("BOMP init:      %lu (%lu ms)\n", timer_xompinit,
                 bench_tsc_to_ms(timer_xompinit));

    debug_printf("Alloc Time:     %lu (%lu ms)\n", timer_alloc,
                 bench_tsc_to_ms(timer_alloc));

    debug_printf("Init Time:      %lu (%lu ms)\n", timer_init,
                 bench_tsc_to_ms(timer_init));

    debug_printf("Share Time:     %lu (%lu ms)\n", timer_share,
                 bench_tsc_to_ms(timer_share));
#if BENCH_RUN_SINGLE
    debug_printf("Single Time:    %lu (%lu ms)\n", timer_single,
                 bench_tsc_to_ms(timer_single));
#endif
    if (!is_shared) {
        debug_printf("OMP Repl:       %lu (%lu ms)\n", timer_omp_repl,
                     bench_tsc_to_ms(timer_omp_repl));
    }
    debug_printf("OMP Shared:     %lu (%lu ms)\n", timer_omp_shared,
                 bench_tsc_to_ms(timer_omp_shared));

    cycles_t timer_common = timer_alloc + timer_init;
    cycles_t timer_xomp = timer_common + timer_xompinit + timer_share;
#if BENCH_RUN_SINGLE
    debug_printf("Total (Single): %lu (%lu ms)\n", timer_single + timer_common,
                 bench_tsc_to_ms(timer_single + timer_common));
#endif
    if (!is_shared) {
        debug_printf("Total (Repl):   %lu (%lu ms)\n", timer_omp_repl + timer_xomp,
                     bench_tsc_to_ms(timer_omp_repl + timer_xomp));
    }

    debug_printf("Total (Shared): %lu (%lu ms)\n", timer_omp_shared + timer_xomp,
                 bench_tsc_to_ms(timer_omp_shared + timer_xomp));

    debug_printf("-------------------------------------\n");

    cycles_t tscperus = bench_tsc_per_us();
#if BENCH_RUN_SINGLE
    bench_ctl_dump_analysis(ctl_single, 0, "Single", tscperus);
#endif
    if (!is_shared) {
        bench_ctl_dump_analysis(ctl_omp_repl, 0, "OMP Replicated", tscperus);
    }
    bench_ctl_dump_analysis(ctl_omp_shared, 0, "OMP Shared", tscperus);

    debug_printf("-------------------------------------\n");
#if XOMP_BENCH_ENABLED
    xomp_master_bench_print_results();
#endif
    debug_printf("-------------------------------------\n");
    while (1)
        ;

}

