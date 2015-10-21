/*
 * \brief demandpaging.c
 *
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <demandpaging.h>

#define NUM_PAGES 8UL
#define NUM_FRAMES 4
#define PAGESIZE BASE_PAGE_SIZE
#define REGION_SIZE (NUM_PAGES * PAGESIZE)

#define EX_STACK_SIZE (128 * 1024)
static char ex_stack[EX_STACK_SIZE];

#define BENCH_NUM_RUNS 100



int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("demand paging test started\n");

    cycles_t t_start, t_end, t_elapsed;
    size_t offset;
    uint64_t tmp;

    bench_init();

    err = demand_paging_init(ex_stack, EX_STACK_SIZE);
    assert(err_is_ok(err));

    debug_printf("creating region\n");
    struct demand_paging_region *dpr;
    err = demand_paging_region_create(REGION_SIZE, PAGESIZE, NUM_FRAMES, &dpr);
    assert(err_is_ok(err));

    uintptr_t addr = (uintptr_t)demand_paging_get_base_address(dpr);


    tmp = 0;
    debug_printf("starting with read pagefaults\n");
    for (offset = 0; offset < REGION_SIZE; offset += PAGESIZE) {
        uint64_t *i = (uint64_t *)(addr + offset);
        tmp += *i;
       // debug_printf("######## page[%lx] = %lx\n", (addr + offset), *i);
    }

    bench_ctl_t *ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_NUM_RUNS);
    if (!ctl) {USER_PANIC("malloc failed");}

    offset = 0;
    do {
        uint64_t *elem = (uint64_t *)(addr + offset);

        t_start = bench_tsc();
        tmp += *elem;
        t_end = bench_tsc();

        offset += PAGESIZE;
        if (offset >= REGION_SIZE) {
            offset = 0;
        }
        t_elapsed = bench_time_diff(t_start, t_end);
    } while (!bench_ctl_add_run(ctl, &t_elapsed));

    bench_ctl_dump_analysis(ctl, 0, "clean reads", bench_tsc_per_us());
    bench_ctl_destroy(ctl);

    if (tmp != 0) {
        debug_printf("tmp was not zero!\n");
        tmp = 0;
    }



    debug_printf("starting with write pagefaults\n");
    for (offset = 0; offset < REGION_SIZE; offset += PAGESIZE) {
        uint64_t *i = (uint64_t *)(addr + offset);
        *i = (addr + offset);
        //debug_printf("######## page[%lx] <- %lx\n", (addr + offset), *i);
    }

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, BENCH_NUM_RUNS);
    if (!ctl) {USER_PANIC("malloc failed");}

    offset = 0;
    do {
        uint64_t *elem = (uint64_t *)(addr + offset);

        t_start = bench_tsc();
        tmp += *elem;
        t_end = bench_tsc();

        offset += PAGESIZE;
        if (offset >= REGION_SIZE) {
            offset = 0;
        }
        t_elapsed = bench_time_diff(t_start, t_end);
    } while (!bench_ctl_add_run(ctl, &t_elapsed));

    bench_ctl_dump_analysis(ctl, 0, "dirty reads", bench_tsc_per_us());
    bench_ctl_destroy(ctl);

    if (tmp == 0) {
        debug_printf("tmp was zero!\n");
    }


    for (offset = 0; offset < REGION_SIZE; offset += PAGESIZE) {
        uint64_t *i = (uint64_t *)(addr + offset);
        if ((addr + offset) != *i) {
            USER_PANIC("######## page[%lx] != %lx\n", (addr + offset), *i);
        }
        //debug_printf("######## page[%lx] = %lx\n", (addr + offset), *i);=
    }

    debug_printf("full page writes\n");
    char val = 0x11;
    for (offset = 0; offset < REGION_SIZE; offset += PAGESIZE) {
        memset((void *)(addr + offset), val++, PAGESIZE);
    }

    val = 0x11;
    for (offset = 0; offset < REGION_SIZE; offset += PAGESIZE) {
        char *data = (char *)addr + offset;
        for (int i = 0; i < PAGESIZE; ++i) {
            if (data[i] != val) {
                USER_PANIC("data[%i]=%x != val=%x\n", i, data[i], val);
            }
        }
        val++;

    }

    debug_printf("DONE.\n");
}
