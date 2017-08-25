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

//{{{1 debugging helpers
static void debug_capref(const char *prefix, struct capref cap)
{
    char buf[128];
    debug_print_capref(buf, 128, cap);
    printf("%s capref = %s\n", prefix, buf);
}

//{{{1 Managment node: implement orchestration for benchmark

//{{{2 Management node: state management

struct global_state {
};

errval_t mgmt_init_benchmark(void **st, int nodecount)
{
     *st = malloc(sizeof(struct global_state));
     if (!*st) {
         return LIB_ERR_MALLOC_FAIL;
     }
     return SYS_ERR_OK;
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
    printf("All clients sent hello! Benchmark would start.\n");
    printf("NOOP Benchmark test done!\n");
}

void mgmt_cmd(uint32_t cmd, uint32_t arg, struct bench_distops_binding *b)
{
    printf("mgmt node got command %d(arg = %d) over binding %p\n", cmd, arg, b);
}

void mgmt_cmd_caps(uint32_t cmd, uint32_t arg, struct capref cap1,
                   struct bench_distops_binding *b)
{
    printf("mgmt node got caps + command %"PRIu32", arg=%d over binding %p:\n",
            cmd, arg, b);
    debug_capref("cap1:", cap1);
}

//{{{1 Node

struct node_state {
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

void node_cmd(uint32_t cmd, uint32_t arg, struct bench_distops_binding *b)
{
    printf("node %d got command %"PRIu32", arg=%d\n", my_core_id, cmd, arg);
}

void node_cmd_caps(uint32_t cmd, uint32_t arg, struct capref cap1,
                   struct bench_distops_binding *b)
{
    printf("node %d got caps + command %"PRIu32", arg=%d:\n", my_core_id, cmd, arg);
    debug_capref("cap1:", cap1);
}
