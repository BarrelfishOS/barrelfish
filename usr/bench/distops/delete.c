/**
 * \file
 * \brief no-op dummy for testing the benchmark framework
 *
 * Use this as a template if you want to write your own benchmark
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <if/bench_distops_defs.h>

#include "benchapi.h"

#define NUM_COPIES 20

//{{{1 debugging helpers
static void debug_capref(const char *prefix, struct capref cap)
{
    char buf[128];
    debug_print_capref(buf, 128, cap);
    printf("%s capref = %s\n", prefix, buf);
}

//{{{1 shared commands
enum bench_cmd {
    BENCH_CMD_CREATE_COPIES,
    BENCH_CMD_COPIES_DONE,
};

//{{{1 Managment node: implement orchestration for benchmark

//{{{2 Management node: state management

struct global_state {
    struct capref ram;
    int nodecount;
    int copies_done;
};

errval_t mgmt_init_benchmark(void **st, int nodecount)
{
     *st = malloc(sizeof(struct global_state));
     if (!*st) {
         return LIB_ERR_MALLOC_FAIL;
     }
     struct global_state *gs = *st;
     errval_t err;
     err = ram_alloc(&gs->ram, BASE_PAGE_BITS);
     gs->nodecount = nodecount;
     gs->copies_done = 0;
     return err;
}

struct mgmt_node_state {
};

errval_t mgmt_init_node(void **st)
{
     *st = malloc(sizeof(struct mgmt_node_state));
     if (!*st) {
         return LIB_ERR_MALLOC_FAIL;
     }
    return SYS_ERR_OK;
}

//{{{2 Management node: benchmark impl
void mgmt_run_benchmark(void *st)
{
    printf("All clients sent hello! Benchmark starting...\n");

    printf("Distributing capability copies...\n");

    struct global_state *gs = st;
    broadcast_caps(BENCH_CMD_CREATE_COPIES, gs->ram, NULL_CAP);

    // TODO implement actual benchmark

    printf("Delete benchmark done!\n");
}

void mgmt_cmd(uint32_t cmd, struct bench_distops_binding *b)
{
    struct global_state *gs = get_global_state(b);
    switch(cmd) {
        case BENCH_CMD_COPIES_DONE:
            gs->copies_done++;
            if (gs->copies_done == gs->nodecount) {
                printf("All copies made!\n");
            }
            break;
        default:
            printf("mgmt node got unknown command %d over binding %p\n", cmd, b);
            break;
    }
}

void mgmt_cmd_caps(uint32_t cmd, struct capref cap1, struct capref cap2,
                   struct bench_distops_binding *b)
{
    printf("mgmt node got caps + command %"PRIu32" over binding %p:\n", cmd, b);
    debug_capref("cap1:", cap1);
    debug_capref("cap2:", cap2);
}

//{{{1 Node

struct node_state {
    struct capref copies[NUM_COPIES];
};

static coreid_t my_core_id = -1;

void init_node(struct bench_distops_binding *b)
{
    printf("%s: binding = %p\n", __FUNCTION__, b);

    my_core_id = disp_get_core_id();

    // Allocate client state struct
    b->st = malloc(sizeof(struct node_state));
    assert(b->st);
    if (!b->st) {
        USER_PANIC("state malloc() in client");
    }
}

void node_cmd(uint32_t cmd, struct bench_distops_binding *b)
{
    printf("node %d got command %"PRIu32"\n", my_core_id, cmd);
}

void node_cmd_caps(uint32_t cmd, struct capref cap1, struct capref cap2,
                   struct bench_distops_binding *b)
{
    errval_t err;

    struct node_state *ns = b->st;

    switch (cmd) {
        case BENCH_CMD_CREATE_COPIES:
            printf("node %d: creating cap copies\n", my_core_id);
            for (int i = 0; i < NUM_COPIES; i++) {
                err = slot_alloc(&ns->copies[i]);
                PANIC_IF_ERR(err, "slot_alloc for copy %d\n", i);
                err = cap_copy(ns->copies[i], cap1);
                PANIC_IF_ERR(err, "cap_copy for copy %d\n", i);
            }
            err = bench_distops_basic__tx(b, NOP_CONT, BENCH_CMD_COPIES_DONE);
            PANIC_IF_ERR(err, "signaling cap_copy() done\n");
            break;
        default:
            printf("node %d got caps + unknown command %"PRIu32":\n", my_core_id, cmd);
            debug_capref("cap1:", cap1);
            debug_capref("cap2:", cap2);
            break;
    }
}
