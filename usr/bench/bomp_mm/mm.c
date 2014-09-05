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

static void matrix_alloc(uint64_t rows,
                         uint64_t cols)
{
    errval_t err;

    uint64_t min_base, max_limit;

    ram_get_affinity(&min_base, &max_limit);
    ram_set_affinity(RAM_MIN_BASE, RAM_MAX_LIMIT);

    size_t matrix_size = (rows * cols) * sizeof(MATRIX_TYPE);

    err = frame_alloc(&frame_mAB, 2 * matrix_size, &matrix_size);
    EXPECT_SUCCESS(err, "frame allocate\n");

    void *addr;
    err = vspace_map_one_frame(&addr, matrix_size, frame_mAB, NULL, NULL);
    EXPECT_SUCCESS(err, "vspace_map_one_frame\n");

    mA_repl = addr;
    mB_repl = addr + ((rows * cols) * sizeof(MATRIX_TYPE));

    struct capref copy;
    err = slot_alloc(&copy);
    EXPECT_SUCCESS(err, "slot_alloc\n");

    err = cap_copy(copy, frame_mAB);
    EXPECT_SUCCESS(err, "cap_copy\n");

    err = vspace_map_one_frame(&addr, matrix_size, copy, NULL, NULL);
    EXPECT_SUCCESS(err, "vspace_map_one_frame\n");

    mA_shared = addr;
    mB_shared = addr + ((rows * cols) * sizeof(MATRIX_TYPE));

    matrix_size = (rows * cols) * sizeof(MATRIX_TYPE);

    err = frame_alloc(&frame_mC, matrix_size, &matrix_size);
    EXPECT_SUCCESS(err, "frame allocate\n");

    err = vspace_map_one_frame(&addr, matrix_size, frame_mC, NULL, NULL);
    EXPECT_SUCCESS(err, "vspace_map_one_frame\n");

    mC_repl = addr;

    err = slot_alloc(&copy);
    EXPECT_SUCCESS(err, "slot_alloc\n");

    err = cap_copy(copy, frame_mC);
    EXPECT_SUCCESS(err, "cap_copy\n");

    err = vspace_map_one_frame(&addr, matrix_size, copy, NULL, NULL);
    EXPECT_SUCCESS(err, "vspace_map_one_frame\n");

    mC_shared = addr;

    size_t mm_frame_size = sizeof(struct mm_frame);
    err = frame_alloc(&frame_mm_frame, mm_frame_size, &mm_frame_size);
    EXPECT_SUCCESS(err, "frame allocate\n");

    err = vspace_map_one_frame(&addr, mm_frame_size, frame_mm_frame, NULL, NULL);
    EXPECT_SUCCESS(err, "vspace_map_one_frame\n");

    g_mm_frame = addr;

    g_mm_frame->cols = cols;
    g_mm_frame->rows = rows;
    g_mm_frame->sum = 0;

    ram_set_affinity(min_base, max_limit);
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

static void matrix_share(void)
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

    DEBUG("==========================================================\n");
    DEBUG("rame_mAB, (lvaddr_t) mA_repl, XOMP_FRAME_TYPE_REPL_RW\n");
    err = xomp_master_add_memory(frame_mAB, (lvaddr_t) mA_repl,
                                 XOMP_FRAME_TYPE_REPL_RW);
    EXPECT_SUCCESS(err, "xomp_master_add_memory mA_repl\n");

    DEBUG("==========================================================\n");
    DEBUG("frame_mC, (lvaddr_t) mC_repl, XOMP_FRAME_TYPE_REPL_RW\n");
    err = xomp_master_add_memory(frame_mC, (lvaddr_t) mC_repl,
                                 XOMP_FRAME_TYPE_REPL_RW);
    EXPECT_SUCCESS(err, "xomp_master_add_memory mC_repl\n");
}

static void mm_single(MATRIX_TYPE *a,
                      MATRIX_TYPE *b,
                      MATRIX_TYPE *c)
{
    for (int i = 0; i < g_mm_frame->rows; i++) {
        uint64_t row = i * g_mm_frame->rows;
        MATRIX_TYPE *a_row = a + (row);
        MATRIX_TYPE *c_row = c + (row);
        for (int j = 0; j < g_mm_frame->cols; j++) {
            MATRIX_TYPE cval = 0;
            for (int k = 0; k < g_mm_frame->rows; k++) {
                cval += (a_row[k] * b[k * g_mm_frame->rows + j]);
            }
            c_row[j] = cval;
            g_sum += cval;
        }
    }
    debug_printf("sum was: %lu\n", g_sum);
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
        MATRIX_TYPE sum = 0;
        uint32_t counter = 0;
        cycles_t start = bench_tsc();
#pragma omp for nowait //schedule (SCHEDULE, CHUNK)
        for (int i = 0; i < nrows; i++) {
            uint64_t row = i * nrows;
            MATRIX_TYPE *a_row = a + (row);
            MATRIX_TYPE *c_row = c + (row);
            for (int j = 0; j < ncols; j++) {
                MATRIX_TYPE cval = 0;
                for (int k = 0; k < nrows; k++) {
                    cval += (a_row[k] * b[k * nrows + j]);
                }

                c_row[j] = cval;

                if (i == j) {
                    counter++;
                }
                sum += cval;
            }
        }

        cycles_t end = bench_tsc();
        debug_printf("done: %lu cycles, %lu ms\n", end-start, bench_tsc_to_ms(end-start));

        // TODO: atomic add;
        __sync_fetch_and_add(&mm_frame->sum, sum);
    }

    if (mm_frame->sum != g_sum) {
        debug_printf("ERROR: sum was not identical: %lu / %lu\n", g_mm_frame->sum,
                     g_sum);
    }
}

int main(int argc,
         char *argv[])
{
    errval_t err;
    xomp_wid_t wid;
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

#if 1
    debug_printf("waiting for xeon phi to be ready\n");

    err = xeon_phi_domain_blocking_lookup("xeon_phi.0.ready", NULL);
    EXPECT_SUCCESS(err, "nameservice_blocking_lookup");
    err = xeon_phi_domain_blocking_lookup("xeon_phi.1.ready", NULL);
    EXPECT_SUCCESS(err, "nameservice_blocking_lookup");
#endif

    bench_init();

    struct xomp_args xomp_arg = {
        .type = XOMP_ARG_TYPE_UNIFORM,
        .core_stride = 2,
        .args = {
            .uniform = {
                .nthreads = nthreads,
                .worker_loc = XOMP_WORKER_LOC_MIXED,
                .nphi = 2,
                .argc = argc,
                .argv = argv
            }
        }
    };

    cycles_t timer_start = bench_tsc();
    if (bomp_xomp_init(&xomp_arg)) {
        debug_printf("bomp init failed!\n");
        exit(1);
    }
    cycles_t timer_xompinit = bench_tsc() - timer_start;

    debug_printf("Matrix Size: [%lu x %lu]\n", rows, cols);
    debug_printf("Matrix Size: %lu kB\n", rows * cols * sizeof(MATRIX_TYPE) >> 10);
    debug_printf("Num Threads: %u\n", nthreads);

    DEBUG("matrix_alloc\n");
    timer_start = bench_tsc();
    matrix_alloc(rows, cols);
    cycles_t timer_alloc = bench_tsc() - timer_start;

    DEBUG("matrix_init\n");
    timer_start = bench_tsc();
    matrix_init();
    cycles_t timer_init = bench_tsc() - timer_start;

    DEBUG("matrix_share\n");
    timer_start = bench_tsc();
    matrix_share();
    cycles_t timer_share = bench_tsc() - timer_start;

if (0){
    DEBUG("mm_single\n");
    timer_start = bench_tsc();
    //mm_single(mA_repl, mB_repl, mC_repl);
    mm_single(mA_shared, mB_shared, mC_shared);
}
    cycles_t timer_single = bench_tsc() - timer_start;

    memset(mC_shared, 0, rows * cols * sizeof(MATRIX_TYPE));

    DEBUG("mm_omp\n");
    timer_start = bench_tsc();
    mm_omp(mA_repl, mB_repl, mC_repl, g_mm_frame);
    cycles_t timer_omp_repl = bench_tsc() - timer_start;

#if 0
    g_mm_frame->sum = 0;
    memset(mC_shared, 0, rows * cols * sizeof(MATRIX_TYPE));

    DEBUG("mm_omp\n");
    timer_start = bench_tsc();
    mm_omp(mA_shared, mB_shared, mC_shared, g_mm_frame);
#endif
    cycles_t timer_omp_shared = bench_tsc() - timer_start;

    debug_printf("-------------------------------------\n");

    debug_printf("BOMP init:      %lu (%lu ms)\n", timer_xompinit,
                 bench_tsc_to_ms(timer_xompinit));

    debug_printf("Alloc Time:     %lu (%lu ms)\n", timer_alloc,
                 bench_tsc_to_ms(timer_alloc));

    debug_printf("Init Time:      %lu (%lu ms)\n", timer_init,
                 bench_tsc_to_ms(timer_init));

    debug_printf("Share Time:     %lu (%lu ms)\n", timer_share,
                 bench_tsc_to_ms(timer_share));

    debug_printf("Single Time:    %lu (%lu ms)\n", timer_single,
                 bench_tsc_to_ms(timer_single));

    debug_printf("OMP Repl:       %lu (%lu ms)\n", timer_omp_repl,
                 bench_tsc_to_ms(timer_omp_repl));

    debug_printf("OMP Shared:     %lu (%lu ms)\n", timer_omp_shared,
                 bench_tsc_to_ms(timer_omp_shared));

    cycles_t timer_common = timer_alloc + timer_init;
    cycles_t timer_xomp = timer_common + timer_xompinit + timer_share;

    debug_printf("Total (Single): %lu (%lu ms)\n", timer_single + timer_common,
                 bench_tsc_to_ms(timer_single + timer_common));

    debug_printf("Total (Repl):   %lu (%lu ms)\n", timer_omp_repl + timer_xomp,
                 bench_tsc_to_ms(timer_omp_repl + timer_xomp));

    debug_printf("Total (Shared): %lu (%lu ms)\n", timer_omp_shared + timer_xomp,
                 bench_tsc_to_ms(timer_omp_shared + timer_xomp));

    debug_printf("-------------------------------------\n");

}

