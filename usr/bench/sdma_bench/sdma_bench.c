/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <dist/barrier.h>

#include <thc/thc.h>

#include <if/omap_sdma_defs.h>
#include <if/omap_sdma_thc.h>

#include <stdio.h>
#include <string.h>

#define MIN_FRAME_BITS 12 // 4KB
#define MAX_FRAME_BITS 25 // 32MB

#define FRAME_COUNT (MAX_FRAME_BITS-MIN_FRAME_BITS+1)

#define MIN_2DCOUNT_BITS 7
#define MAX_2DCOUNT_BITS MAX_FRAME_BITS

#define OMAP44XX_SDMA_MAX_FN_BITS    15
#define OMAP44XX_SDMA_MAX_EN_BITS    23

#define MIN(a,b) ((a) < (b) ? (a) : (b))

#define MEASURE(outval, CALL) {                     \
    uint64_t start, end;                            \
    sys_debug_hardware_global_timer_read(&start);   \
    CALL;                                           \
    sys_debug_hardware_global_timer_read(&end);     \
    assert(start > measure_overhead);               \
    start -= measure_overhead;                      \
    assert(end > measure_overhead);                 \
    end -= measure_overhead;                        \
    outval = end - start;                           \
}

static size_t num_workers;
static int32_t bench_id, bench_min, bench_max;

static struct capref src_frame[FRAME_COUNT];
static struct capref dst_frame[FRAME_COUNT];
static uint8_t *src_buf[FRAME_COUNT];
static uint8_t *dst_buf[FRAME_COUNT];

static uint64_t measure_overhead;

static void measure_timer_overhead(void)
{
    size_t runs = 10000;
    uint64_t start, end;
    sys_debug_hardware_global_timer_read(&start);
    for(int i=0; i<runs; i++) {
        sys_debug_hardware_global_timer_read(&end);
    }

    measure_overhead = (end - start) / runs;
}

static void bench_wait_for_all(char *event)
{
    errval_t err;
    if (bench_id == 0) {
        err = nsb_master(bench_min, bench_max, event);
    } else {
        err = nsb_worker(bench_id, event);
    }

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bench_wait_for_all");
    }
}

static void frame_allocate_and_map(void **retbuf, struct capref *retcap, size_t bytes)
{
    errval_t err;
    size_t retbytes;

    err = frame_alloc(retcap, bytes, &retbytes);
    assert(err_is_ok(err));
    assert(retbytes == bytes);


    err = vspace_map_one_frame_attr(retbuf, bytes, *retcap,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    assert(err_is_ok(err));
}

enum color_mode {
    COLOR_MODE_COPY,
    COLOR_MODE_FILL,
    COLOR_MODE_TRANS_COPY
};

#define BENCH_RUNS 10

static void bench_1d(struct omap_sdma_thc_client_binding_t *cl,
                     const char *desc, enum color_mode mode, bool synchronized)
{
    for(uint8_t bits=MIN_FRAME_BITS; bits <= MAX_FRAME_BITS; bits++) {
        errval_t err;

        size_t idx = bits - MIN_FRAME_BITS;

        if (synchronized) {
            char key[128];
            snprintf(key, sizeof(key), "%s.%u", desc, bits);
            bench_wait_for_all(key);
        }


        uint64_t cycles[BENCH_RUNS];
        for (size_t run=0; run<BENCH_RUNS; run++) {
            if (mode == COLOR_MODE_COPY) {
                MEASURE(cycles[run], {
                    cl->call_fifo.mem_copy(cl, dst_frame[idx], src_frame[idx], &err);
                });
            } else {
                MEASURE(cycles[run], {
                    cl->call_fifo.mem_fill(cl, dst_frame[idx], 0, &err);
                });
            }

            if(err_is_fail(err)) {
                USER_PANIC_ERR(err, "bench_1d failed");
            }
        }

        uint8_t x_bits = MIN(bits, OMAP44XX_SDMA_MAX_EN_BITS);
        uint8_t y_bits = bits - x_bits;
        size_t en = 1 << x_bits;
        size_t fn = 1 << y_bits;
        size_t count = en*fn;

        size_t nt = synchronized ? num_workers+1 : 1;

        for (size_t run=0; run<BENCH_RUNS; run++) {
        printf("%2"PRIu32"|%8"PRIu32"|%6"PRIu32"|%9"PRIu32"|%11"PRIu64"|%2zu|%2d| %s\n",
           1, en, fn, count, cycles[run], nt, bench_id, desc);
        }
    }
}


static void bench_2d(struct omap_sdma_thc_client_binding_t *cl,
                     const char *desc, enum color_mode mode, bool synchronized)
{
    errval_t err;

    omap_sdma_count_2d_t count = {
        .pixel_size = omap_sdma_DATA_TYPE_8BIT,
        .x_count = 0,
        .y_count = 0,
    };

    omap_sdma_addr_2d_t addr = {
        .x_start = 0,
        .y_start = 0,
        .x_modify = 1,
        .y_modify = 1,
    };

    omap_sdma_addr_2d_t src_addr = addr;
    src_addr.cap = src_frame[FRAME_COUNT-1];

    omap_sdma_addr_2d_t dst_addr = addr;
    dst_addr.cap = dst_frame[FRAME_COUNT-1];

    for (uint8_t bits = MIN_2DCOUNT_BITS; bits <= MAX_2DCOUNT_BITS; bits++) {

        uint8_t x_bits = MIN(bits, OMAP44XX_SDMA_MAX_EN_BITS);
        uint8_t y_bits = bits - x_bits;

        count.x_count = 1 << x_bits;
        count.y_count = 1 << y_bits;

        bool trans_copy = mode == COLOR_MODE_TRANS_COPY;

        if (synchronized) {
            char key[128];
            snprintf(key, sizeof(key), "%s.%u", desc, bits);
            bench_wait_for_all(key);
        }

        uint64_t cycles[BENCH_RUNS];
        for (size_t run=0; run<BENCH_RUNS; run++) {
            if (mode == COLOR_MODE_COPY) {
                MEASURE(cycles[run], {
                    cl->call_fifo.mem_copy_2d(cl, dst_addr, src_addr, count,
                                                trans_copy, 0, &err);
                });
            } else {
                MEASURE(cycles[run], {
                    cl->call_fifo.mem_fill_2d(cl, dst_addr, 0, count, &err);
                });
            }

            if(err_is_fail(err)) {
                USER_PANIC_ERR(err, "bench_2d failed");
            }
        }

        size_t nt = synchronized ? num_workers+1 : 1;

        for (size_t run=0; run<BENCH_RUNS; run++) {
            printf("%2"PRIu32"|%8"PRIu32"|%6"PRIu32"|%9"PRIu32"|%11"PRIu64"|%2zu|%2d| %s\n",
                1, count.x_count, count.y_count, count.x_count*count.y_count,
                cycles[run], nt, bench_id, desc);
        }
    }
}

static void run_client(struct omap_sdma_thc_client_binding_t *cl)
{
    bench_wait_for_all("sdma_bench_ready");

    if (bench_id == 0) {
        bench_1d(cl, "mem_copy_1d_single", COLOR_MODE_COPY, false);
        bench_2d(cl, "mem_copy_2d_single", COLOR_MODE_COPY, false);

        bench_1d(cl, "mem_fill_1d_single", COLOR_MODE_FILL, false);
        bench_2d(cl, "mem_fill_2d_single", COLOR_MODE_FILL, false);

        bench_1d(cl, "mem_trans_1d_single", COLOR_MODE_FILL, false);
        bench_2d(cl, "mem_trans_2d_single", COLOR_MODE_FILL, false);
    }

    bench_wait_for_all("sdma_bench_concurrent_1d_copy");
    bench_1d(cl, "mem_copy_1d_concurrent", COLOR_MODE_COPY, true);

    bench_wait_for_all("sdma_bench_concurrent_1d_fill");
    bench_1d(cl, "mem_fill_1d_concurrent", COLOR_MODE_FILL, true);

    bench_wait_for_all("sdma_bench_concurrent_1d_trans");
    bench_1d(cl, "mem_trans_1d_concurrent", COLOR_MODE_TRANS_COPY, true);

    bench_wait_for_all("sdma_bench_concurrent_2d_copy");
    bench_2d(cl, "mem_copy_2d_concurrent", COLOR_MODE_COPY, true);

    bench_wait_for_all("sdma_bench_concurrent_2d_fill");
    bench_2d(cl, "mem_fill_2d_concurrent", COLOR_MODE_FILL, true);

    bench_wait_for_all("sdma_bench_concurrent_2d_trans");
    bench_2d(cl, "mem_trans_2d_concurrent", COLOR_MODE_TRANS_COPY, true);

    bench_wait_for_all("sdma_bench_finished");

    if (bench_id == 0) {
        printf("---------------------------------------------------\n");
    }
}

static void start_client(void)
{
    errval_t err;

    struct omap_sdma_binding *b;
    struct omap_sdma_thc_client_binding_t *cl;

    err = omap_sdma_thc_connect_by_name("sdma",
                                      get_default_waitset(),
                                      IDC_BIND_FLAGS_DEFAULT,
                                      &b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not bind (thc)");
    }

    cl = malloc(sizeof(struct omap_sdma_thc_client_binding_t));
    assert(cl != NULL);

    err = omap_sdma_thc_init_client(cl, b, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not init client (thc)");
    }

    run_client(cl);
}

int main(int argc, char *argv[])
{
    if(argc == 4 && strcmp(argv[1], "worker") == 0) {
        bench_id = atoi(argv[2]);
        num_workers = atoi(argv[3]);
        assert(bench_id > 0);
        debug_printf("worker bench id: %"PRIi32"\n", bench_id);
    } else if(argc == 3 && strcmp(argv[1], "master") == 0) {
        printf("ES|   EN   |  FN  |  BYTES  |   TICKS   |NT|ID| CONFIG\n");

        num_workers = atoi(argv[2]);

        bench_id = 0;
        bench_min = 1;
        bench_max = num_workers;

        debug_printf("master bench, spawning %zu workers...\n", num_workers);

        coreid_t core_id = disp_get_core_id();
        for (uint32_t id = 1; id <= num_workers; id++) {
            char id_str[10];
            snprintf(id_str, sizeof(id_str), "%lu", id);
            char num_str[10];
            snprintf(num_str, sizeof(num_str), "%lu", num_workers);
            char* worker_argv[] = {argv[0], "worker", id_str, num_str, NULL};
            errval_t err = spawn_program(core_id, "/armv7/sbin/sdma_bench", worker_argv,
                            NULL, SPAWN_FLAGS_DEFAULT, NULL);
            assert(err_is_ok(err));
        }
    } else {
        printf("sdma_bench [master|worker] ...\n\n");
        printf("    master NUM      Master which will spawn NUM worker\n");
        printf("    worker ID NUM   Worker with client id ID, NUM as info\n");
        exit(1);
    }

    // make sure overhead is measured correctly
    if(bench_id > 0) {
        nsb_worker(bench_id, "sdma_bench_measure_timer_start");
        nsb_wait_n(bench_id-1, "sdma_bench_measure_timer_next");
    } else {
        nsb_master(bench_min, bench_max, "sdma_bench_measure_timer_start");
    }

    // this will measure the time it takes to read out the timer
    measure_timer_overhead();

    // notify next worker
    nsb_register_n(bench_id, "sdma_bench_measure_timer_next");

    bench_wait_for_all("sdma_bench_measure_timer_finished");

    printf("timer overhead: %"PRIu64"\n", measure_overhead);

    // allocate frames of different sizes
    for (size_t i=0; i<FRAME_COUNT; i++) {
        size_t bytes = 1 << (MIN_FRAME_BITS + i);
        frame_allocate_and_map((void**) &src_buf[i], &src_frame[i], bytes);
        frame_allocate_and_map((void**) &dst_buf[i], &dst_frame[i], bytes);
    }

    start_client();

    return 0;
}
