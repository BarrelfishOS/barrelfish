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

// switch to enable memory touching
//#define DO_MEMORY_TOUCH 1

#define PRINT_CSV_VALUES 1

// number of repetitions for the operations
#define BENCH_RUN_COUNT 30
#define BENCH_RUN_COUNT2 30

// minimum size of the buffer to map in bits
#define FRAME_BITS_MIN 12
#ifdef __k1om__
#define FRAME_BITS_MAX 30
#elif defined(__ARM_ARCH_7A__)
#define FRAME_BITS_MAX 29
#else
// maximum size of the buffer in bits
#define FRAME_BITS_MAX 35
#endif
// increment of the buffer size
#define FRAME_BITS_INC 1

#define EXPECT_SUCCESS(errval, msg) \
    if (err_is_fail(err)) {USER_PANIC_ERR(err, msg);}


/*
 * Do touching the memory. This is for comparing the implementation with Linux
 * first touch policy i.e. touch once per page
 */
#ifdef DO_MEMORY_TOUCH
static char do_memory_touch(void *addr, size_t size, size_t pagesize)
{
    char *end = ((char *)addr)+size;
    char buf;
    for (char *ptr = addr; ptr < end; ptr += pagesize) {
        buf = *ptr;
    }
    return buf;
}
#endif


static void run_map_protect_unmap(struct capref frame, vregion_flags_t flags)
{
    errval_t err;

    char buf[50];
    void *addr;
    cycles_t tsc_start, tsc_end, elapsed;
    bench_ctl_t *m_ctl, *u_ctl, *p_ctl;

    char p = 'B';
    uint32_t bits_start = FRAME_BITS_MIN;
    size_t alignment = BASE_PAGE_SIZE;
    if (flags & VREGION_FLAGS_LARGE) {
        bits_start = LARGE_PAGE_BITS;
        p = 'L';
        alignment = LARGE_PAGE_SIZE;
#ifndef __ARM_ARCH_7A__
    } else if (flags & VREGION_FLAGS_HUGE) {
        bits_start = HUGE_PAGE_BITS;
        p = 'H';
        alignment = HUGE_PAGE_SIZE;
#endif
    }

    flags |= VREGION_FLAGS_READ_WRITE;

    debug_printf("\n");
    debug_printf("starting rounds with map/protect/unmap...\n");
    debug_printf("---------------------------------------\n");
    for (uint32_t i = bits_start; i <= FRAME_BITS_MAX; i += FRAME_BITS_INC) {
        size_t map_size = (1UL << i);
        size_t bits = (i > 20) ? i - 20 : i - 10;
        char *label = (i > 20) ? "MB" : "KB";
        debug_printf("Buffer size of %lu %s\n", (1UL << bits), label);

        m_ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
        u_ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
        p_ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT);
        do {
            struct vregion *vregion;
            struct memobj *memobj;

            /* mapping it */
            tsc_start = bench_tsc();
            err = vspace_map_one_frame_attr_aligned(&addr, map_size, frame, flags,
                                                    alignment, &memobj, &vregion);
#ifdef DO_MEMORY_TOUCH
            do_memory_touch(addr, map_size, alignment);
#endif
            tsc_end = bench_tsc();
            EXPECT_SUCCESS(err, "vspace map one frame");
            elapsed = bench_time_diff(tsc_start, tsc_end);
            bench_ctl_add_run(m_ctl, &elapsed);

            /* change protection rights */
            tsc_start = bench_tsc();
            err = memobj->f.protect(memobj, vregion, 0, map_size, VREGION_FLAGS_READ);
            tsc_end = bench_tsc();
            EXPECT_SUCCESS(err, "change protection rights");
            elapsed = bench_time_diff(tsc_start, tsc_end);
            bench_ctl_add_run(p_ctl, &elapsed);

            /* unmap it */
            tsc_start = bench_tsc();
            err = vspace_unmap(addr);
            tsc_end = bench_tsc();
            elapsed = bench_time_diff(tsc_start, tsc_end);

            EXPECT_SUCCESS(err, "vspace unmap");

        } while (!bench_ctl_add_run(u_ctl, &elapsed));

        /* print analysis */
        snprintf(buf, sizeof(buf), "map: %u", i);
        bench_ctl_dump_analysis(m_ctl, 0, buf, bench_tsc_per_us());
#if PRINT_CSV_VALUES
        snprintf(buf, sizeof(buf), "map_u_%c_%u", p, i);
        bench_ctl_dump_csv(m_ctl, buf, bench_tsc_per_us());
#endif

        snprintf(buf, sizeof(buf), "unmap: %u", i);
        bench_ctl_dump_analysis(u_ctl, 0, buf, bench_tsc_per_us());
#if PRINT_CSV_VALUES
        snprintf(buf, sizeof(buf), "unmap_u_%c_%u", p, i);
        bench_ctl_dump_csv(u_ctl, buf, bench_tsc_per_us());
#endif

        snprintf(buf, sizeof(buf), "protect %u", i);
        bench_ctl_dump_analysis(p_ctl, 0, buf, bench_tsc_per_us());
#if PRINT_CSV_VALUES
        snprintf(buf, sizeof(buf), "protect_u_%c_%u", p, i);
        bench_ctl_dump_csv(p_ctl, buf, bench_tsc_per_us());
#endif

        /* destroy the benchmark objects */
        bench_ctl_destroy(m_ctl);
        bench_ctl_destroy(p_ctl);
        bench_ctl_destroy(u_ctl);
    }
}

static void run_map_no_unmap(struct capref frame, vregion_flags_t flags)
{
    errval_t err;

    char buf[50];
    cycles_t tsc_start, tsc_end, elapsed;
    bench_ctl_t *m_ctl, *u_ctl;

    char p = 'B';
    uint32_t bits_start = FRAME_BITS_MIN;
    size_t alignment = BASE_PAGE_SIZE;
    if (flags & VREGION_FLAGS_LARGE) {
        bits_start = LARGE_PAGE_BITS;
        p = 'L';
        alignment = LARGE_PAGE_SIZE;
#ifndef __ARM_ARCH_7A__
    } else if (flags & VREGION_FLAGS_HUGE) {
        bits_start = HUGE_PAGE_BITS;
        p = 'H';
        alignment = HUGE_PAGE_SIZE;
#endif
    }

    flags |= VREGION_FLAGS_READ_WRITE;

    debug_printf("starting rounds without unmap...\n");
    debug_printf("---------------------------------------\n");

    struct capref cnode_cap;
    struct capref frames;
    cnode_create(&cnode_cap, &frames.cnode, BENCH_RUN_COUNT2, NULL);
    for (int slot = 0; slot < BENCH_RUN_COUNT2; ++slot) {
        frames.slot = slot;
        err = cap_copy(frames, frame);
        EXPECT_SUCCESS(err, "allocating a slot");
    }

    for (uint32_t i = bits_start; i <= FRAME_BITS_MAX; i += FRAME_BITS_INC) {
        size_t map_size = (1UL << i);
        size_t bits = (i > 20) ? i - 20 : i - 10;
        char *label = (i > 20) ? "MB" : "KB";
        debug_printf("Buffer size of %lu %s\n", (1UL << bits), label);
        void *addrs[BENCH_RUN_COUNT2+1];
        int current = 0;

        m_ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT2);
        u_ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_RUN_COUNT2);
        do {
            frames.slot = current;
            tsc_start = bench_tsc();
            err = vspace_map_one_frame_attr_aligned(&addrs[current], map_size, frames, flags,
                                                    alignment, NULL, NULL);
#ifdef DO_MEMORY_TOUCH
            do_memory_touch(addrs[current], map_size, alignment);
#endif
            tsc_end = bench_tsc();
            EXPECT_SUCCESS(err, "vspace map one frame");
            elapsed = bench_time_diff(tsc_start, tsc_end);
            current++;
        } while (!bench_ctl_add_run(m_ctl, &elapsed));

        current = 0;
        do {
            tsc_start = bench_tsc();
            err = vspace_unmap(addrs[current]);
            tsc_end = bench_tsc();
            EXPECT_SUCCESS(err, "vspace map one frame");
            elapsed = bench_time_diff(tsc_start, tsc_end);
            current++;
        } while (!bench_ctl_add_run(u_ctl, &elapsed));

        snprintf(buf, sizeof(buf), "map: %u", i);
        bench_ctl_dump_analysis(m_ctl, 0, buf, bench_tsc_per_us());
#if PRINT_CSV_VALUES
        snprintf(buf, sizeof(buf), "map_no_%c_%u", p, i);
        bench_ctl_dump_csv(m_ctl, buf, bench_tsc_per_us());
#endif
        snprintf(buf, sizeof(buf), "unmap: %u", i);
        bench_ctl_dump_analysis(u_ctl, 0, buf, bench_tsc_per_us());
#if PRINT_CSV_VALUES
        snprintf(buf, sizeof(buf), "unmap_no_%c_%u", p, i);
        bench_ctl_dump_csv(u_ctl, buf, bench_tsc_per_us());
#endif
        bench_ctl_destroy(m_ctl);
        bench_ctl_destroy(u_ctl);
    }
}


int main(int argc,
         char *argv[])
{
    errval_t err;

    bench_init();

    struct capref frame;

    uint32_t run_no_unmap = 0;
    for (int i = 0; i < argc; ++i) {
        if (strncmp(argv[i], "nounmap", 7)) {
            run_no_unmap = 1;
        }
    }


    debug_printf("=======================================\n");
    debug_printf("VSPACE Map benchmark started\n");
    debug_printf("=======================================\n");
    debug_printf("\n");
    debug_printf("Allocating frame of %lu MB...\n", (1UL << (FRAME_BITS_MAX-20)));

    cycles_t tsc_start, tsc_end, elapsed;
    tsc_start = bench_tsc();
    err = frame_alloc(&frame, 1UL << FRAME_BITS_MAX, NULL);
    EXPECT_SUCCESS(err, "frame alloc");
    tsc_end = bench_tsc();
    elapsed = bench_time_diff(tsc_start, tsc_end);
    debug_printf("Allocation took %lu us\n", bench_tsc_to_us(elapsed));

    if (run_no_unmap) {
        run_map_no_unmap(frame, VREGION_FLAGS_HUGE);
        run_map_no_unmap(frame, VREGION_FLAGS_LARGE);
        run_map_no_unmap(frame, 0);
    } else {
        run_map_protect_unmap(frame, VREGION_FLAGS_HUGE);
        run_map_protect_unmap(frame, VREGION_FLAGS_LARGE);
        run_map_protect_unmap(frame, 0);
    }

    debug_printf("=======================================\n");
    debug_printf("benchmark done\n");
    debug_printf("=======================================\n");
}

