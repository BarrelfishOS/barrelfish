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
#include <demandpaging.h>

#define NUM_PAGES 8UL
#define NUM_FRAMES 4
#define PAGESIZE BASE_PAGE_SIZE
#define REGION_SIZE (NUM_PAGES * PAGESIZE)

#define EX_STACK_SIZE (128 * 1024)
static char ex_stack[EX_STACK_SIZE];




int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("demand paging test started\n");

    err = demand_paging_init(ex_stack, EX_STACK_SIZE);
    assert(err_is_ok(err));

    debug_printf("creating region\n");
    struct demand_paging_region *dpr;
    err = demand_paging_region_create(REGION_SIZE, PAGESIZE, NUM_FRAMES, &dpr);
    assert(err_is_ok(err));

    uintptr_t addr = (uintptr_t)demand_paging_get_base_address(dpr);

    /* read page faults */
    debug_printf("starting with read pagefaults\n");
    for (size_t offset = 0; offset < REGION_SIZE; offset += PAGESIZE) {
        uint64_t *i = (uint64_t *)(addr + offset);
        debug_printf("######## page[%lx] = %lx\n", (addr + offset), *i);
    }

    debug_printf("starting with write pagefaults\n");
    for (size_t offset = 0; offset < REGION_SIZE; offset += PAGESIZE) {
        uint64_t *i = (uint64_t *)(addr + offset);
        *i = (addr + offset);
        debug_printf("######## page[%lx] <- %lx\n", (addr + offset), *i);
    }

    for (size_t offset = 0; offset < REGION_SIZE; offset += PAGESIZE) {
        uint64_t *i = (uint64_t *)(addr + offset);
        if ((addr + offset) != *i) {
            USER_PANIC("######## page[%lx] != %lx\n", (addr + offset), *i);
        }
        debug_printf("######## page[%lx] = %lx\n", (addr + offset), *i);

    }


    debug_printf("DONE.\n");
}
