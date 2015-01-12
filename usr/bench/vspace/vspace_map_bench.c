/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <barrelfish/barrelfish.h>

#include <bench/bench.h>

#define BENCH_MEASURE_MAP_ONLY 0

#define BENCH_RUN_COUNT 100
#define FRAME_BITS_MIN 12
#ifdef __k1om__
#define FRAME_BITS_MAX 30
#else
#define FRAME_BITS_MAX 32
#endif
#define FRAME_BITS_INC 2

#define EXPECT_SUCCESS(errval, msg) \
    if (err_is_fail(err)) {USER_PANIC_ERR(err, msg);}

int main(int argc,
         char *argv[])
{
    errval_t err;

    bench_init();

    debug_printf("=======================================\n");
    debug_printf("VSPACE Map benchmark started\n");
    debug_printf("=======================================\n");

    char buf[20];
    struct capref frame;
    void *addr;
    cycles_t tsc_start, tsc_end, elapsed;
    bench_ctl_t *b_ctl;

    err = frame_alloc(&frame, 1UL << FRAME_BITS_MAX, NULL);
    EXPECT_SUCCESS(err, "frame alloc");

    for (uint32_t i = FRAME_BITS_MIN; i <= FRAME_BITS_MAX; i += FRAME_BITS_INC) {
        size_t map_size = (1UL << i);

        b_ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);

        do {
            tsc_start = bench_tsc();
            err = vspace_map_one_frame(&addr, map_size, frame, NULL, NULL);
            tsc_end = bench_tsc();
            EXPECT_SUCCESS(err, "vspace map one frame");
            elapsed = bench_time_diff(tsc_start, tsc_end);

            err = vspace_unmap(addr);
            EXPECT_SUCCESS(err, "vspace unmap");

        } while (!bench_ctl_add_run(b_ctl, &elapsed));

        snprintf(buf, 20, "%u", i);
        bench_ctl_dump_analysis(b_ctl, 0, buf, bench_tsc_per_us());

        bench_ctl_destroy(b_ctl);

    }

    debug_printf("=======================================\n");
    debug_printf("benchmark done\n");
    debug_printf("=======================================\n");
}

