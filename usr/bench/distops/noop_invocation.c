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

#include <bitmacros.h>

#include <bench/bench.h>

#include "benchapi.h"

#define NUM_COPIES_START 256
#define NUM_COPIES_END 65536
#define ITERS 1000

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
    BENCH_CMD_CAP_FOR_BENCH,
    BENCH_CMD_GOT_CAP,
    BENCH_CMD_DO_DELETE,
    BENCH_CMD_PRINT_STATS,
    BENCH_CMD_PRINT_DONE,
};

//{{{1 Managment node: implement orchestration for benchmark

//{{{2 Management node: state management

struct global_state {
    struct capref ram;
    struct capref cap;
    int nodecount;
    int copies_done;
    int printnode;
    int currcopies;
};

errval_t mgmt_init_benchmark(void **st, int nodecount)
{
     *st = malloc(sizeof(struct global_state));
     if (!*st) {
         return LIB_ERR_MALLOC_FAIL;
     }
     struct global_state *gs = *st;
     errval_t err;
     gs->nodecount = nodecount;
     gs->copies_done = 0;
     gs->printnode = 1;
     err = ram_alloc(&gs->ram, BASE_PAGE_BITS);
     if (err_is_fail(err)) {
         return err;
     }
     err = ram_alloc(&gs->cap, BASE_PAGE_BITS);
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
    struct global_state *gs = st;

    printf("All clients sent hello! Benchmark starting...\n");

    printf("# Benchmarking NOOP INVOCATION: nodes=%d\n", gs->nodecount);

    printf("# Starting out with %d copies, will increase by factor of two up to %d...\n",
            NUM_COPIES_START, NUM_COPIES_END);

    gs->currcopies = NUM_COPIES_START;
    broadcast_caps(BENCH_CMD_CREATE_COPIES, NUM_COPIES_START, gs->ram);
}

void mgmt_cmd(uint32_t cmd, uint32_t arg, struct bench_distops_binding *b)
{
    struct global_state *gs = get_global_state(b);
    switch(cmd) {
        case BENCH_CMD_COPIES_DONE:
            if (arg == 1) {
                gs->copies_done++;
                if (gs->copies_done == gs->nodecount) {
                    printf("# All copies made!\n");
                    broadcast_cmd(BENCH_CMD_DO_DELETE, ITERS);
                    unicast_cmd(gs->printnode, BENCH_CMD_PRINT_STATS, 0);
                }
            } else {
                bench_distops_caps__tx(b, NOP_CONT, BENCH_CMD_CAP_FOR_BENCH,
                        0, gs->cap);
            }
            break;
        case BENCH_CMD_GOT_CAP:
            gs->copies_done++;
            if (gs->copies_done == gs->nodecount) {
                printf("# All copies made!\n");
                broadcast_cmd(BENCH_CMD_DO_DELETE, ITERS);
                unicast_cmd(gs->printnode, BENCH_CMD_PRINT_STATS, 0);
            }
            break;
        case BENCH_CMD_PRINT_DONE:
            if (gs->printnode == gs->nodecount) {
                if (gs->currcopies == NUM_COPIES_END) {
                    printf("# Benchmark done!\n");
                    return;
                }
                printf("# Round done!\n");
                // Reset counters for next round
                gs->currcopies *= 2;
                gs->copies_done = 0;
                gs->printnode = 1;
                // Start new round
                broadcast_cmd(BENCH_CMD_CREATE_COPIES, gs->currcopies);
                return;
            }
            unicast_cmd(++gs->printnode, BENCH_CMD_PRINT_STATS, 0);
            break;
        default:
            printf("mgmt node got unknown command %d over binding %p\n", cmd, b);
            break;
    }
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
    struct capref cap;
    struct capref ram;
    uint32_t numcopies;
    struct capref *copies;
    uint64_t *delcycles;
    uint32_t benchcount;
};

static coreid_t my_core_id = -1;

void init_node(struct bench_distops_binding *b)
{
    printf("%s: binding = %p\n", __FUNCTION__, b);

    my_core_id = disp_get_core_id();

    bench_init();

    // Allocate client state struct
    b->st = malloc(sizeof(struct node_state));
    assert(b->st);
    if (!b->st) {
        USER_PANIC("state malloc() in client");
    }
}

static void node_create_copies(struct node_state *ns)
{
    errval_t err;
    ns->copies = calloc(ns->numcopies, sizeof(struct capref));
    for (int i = 0; i < ns->numcopies; i++) {
        err = slot_alloc(&ns->copies[i]);
        PANIC_IF_ERR(err, "slot_alloc for copy %d\n", i);
        err = cap_copy(ns->copies[i], ns->ram);
        PANIC_IF_ERR(err, "cap_copy for copy %d\n", i);
    }
}

void node_cmd(uint32_t cmd, uint32_t arg, struct bench_distops_binding *b)
{
    struct node_state *ns = b->st;
    errval_t err;
    size_t cap_base_count = 0;
    err = sys_debug_get_mdb_size(&cap_base_count);
    assert(err_is_ok(err));

    switch(cmd) {
        case BENCH_CMD_CREATE_COPIES:
            printf("# node %d: %zu caps before creating copies\n", my_core_id, cap_base_count);
            printf("# node %d: creating %d cap copies\n", my_core_id, arg);
            ns->numcopies = arg;
            node_create_copies(ns);
            err = bench_distops_cmd__tx(b, NOP_CONT, BENCH_CMD_COPIES_DONE, 1);
            PANIC_IF_ERR(err, "signaling cap_copy() done\n");
            break;
        case BENCH_CMD_DO_DELETE:
            ns->benchcount = arg;
            ns->delcycles = calloc(arg, sizeof(uint64_t));
            assert(ns->delcycles);
            //printf("node %d: doing delete\n", my_core_id);
            for (int i = 0; i < ns->benchcount; i++) {
                uint64_t start, end;
                start = bench_tsc();
                err = invoke_ram_noop(ns->cap);
                end = bench_tsc();
                ns->delcycles[i] = end - start;
                assert(err_is_ok(err));
            }
            //printf("node %d: deletes done\n", my_core_id);
            break;
        case BENCH_CMD_PRINT_STATS:
            printf("# node %d: tsc_per_us = %ld; numcopies = %d\n", my_core_id, bench_tsc_per_us(), ns->numcopies);
            printf("# delete latency in cycles\n");
            for (int i = 0; i < ns->benchcount; i++) {
                printf("%ld\n", ns->delcycles[i]);
            }
            err = bench_distops_cmd__tx(b, NOP_CONT, BENCH_CMD_PRINT_DONE, 0);
            assert(err_is_ok(err));
            // Cleanup before next round
            for (int i = 0; i < ns->numcopies; i++) {
                err = cap_destroy(ns->copies[i]);
                assert(err_is_ok(err));
            }
            free(ns->copies);
            free(ns->delcycles);
            break;
        default:
            printf("node %d got command %"PRIu32"\n", my_core_id, cmd);
            break;
    }
}

void node_cmd_caps(uint32_t cmd, uint32_t arg, struct capref cap1,
                   struct bench_distops_binding *b)
{
    errval_t err;

    struct node_state *ns = b->st;

    switch (cmd) {
        case BENCH_CMD_CREATE_COPIES:
            printf("# node %d: creating %d cap copies\n", my_core_id, arg);
            ns->ram = cap1;
            ns->numcopies = arg;
            node_create_copies(ns);
            err = bench_distops_cmd__tx(b, NOP_CONT, BENCH_CMD_COPIES_DONE, 0);
            PANIC_IF_ERR(err, "signaling cap_copy() done\n");
            break;
        case BENCH_CMD_CAP_FOR_BENCH:
            printf("# node %d: storing cap for benchmark\n", my_core_id);
            ns->cap = cap1;
            err = bench_distops_cmd__tx(b, NOP_CONT, BENCH_CMD_GOT_CAP, 0);
            PANIC_IF_ERR(err, "signaling cap rx done\n");
            break;
        default:
            printf("node %d got caps + command %"PRIu32", arg=%d:\n",
                my_core_id, cmd, arg);
            debug_capref("cap1:", cap1);
            break;
    }
}
