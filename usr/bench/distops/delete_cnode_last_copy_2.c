/**
 * \file
 * \brief Benchmark deleting cnode with varying number of occupied slots
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

//{{{1 debugging helpers
static void debug_capref(const char *prefix, struct capref cap)
{
    char buf[128];
    debug_print_capref(buf, 128, cap);
    printf("%s capref = %s\n", prefix, buf);
}

//{{{1 shared commands
enum bench_cmd {
    BENCH_CMD_DO_DELETE,
    BENCH_CMD_PRINT_STATS,
    BENCH_CMD_PRINT_DONE,
};

//{{{1 Managment node: implement orchestration for benchmark

//{{{2 Management node: state management

struct global_state {
    struct capref ram;
    int nodecount;
    int copies_done;
    int printnode;
    int currcopies;
};

errval_t mgmt_init_benchmark(void **st, int nodecount)
{
    NUM_COPIES_START = 1;
    NUM_COPIES_END = 256;

     *st = malloc(sizeof(struct global_state));
     if (!*st) {
         return LIB_ERR_MALLOC_FAIL;
     }
     struct global_state *gs = *st;
     gs->nodecount = nodecount;
     gs->copies_done = 0;
     gs->printnode = 1;
     gs->currcopies = NUM_COPIES_START;
     return ram_alloc(&gs->ram, BASE_PAGE_BITS);
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

    printf("# Benchmarking DELETE CNODE NO REMOTE CONTENTS: nodes=%d\n", gs->nodecount);

    broadcast_cmd(BENCH_CMD_DO_DELETE, ITERS);
    unicast_cmd(gs->printnode, BENCH_CMD_PRINT_STATS, 0);
}

void mgmt_cmd(uint32_t cmd, uint32_t arg, struct bench_distops_binding *b)
{
    struct global_state *gs = get_global_state(b);
    switch(cmd) {
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
                broadcast_cmd(BENCH_CMD_DO_DELETE, ITERS);
                unicast_cmd(gs->printnode, BENCH_CMD_PRINT_STATS, 0);
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

    NUM_COPIES_START = 1;
    NUM_COPIES_END = 256;

    bench_init();

    // Allocate client state struct
    b->st = malloc(sizeof(struct node_state));
    assert(b->st);
    if (!b->st) {
        USER_PANIC("state malloc() in client");
    }
    struct node_state *ns = b->st;
    printf("# node %d: allocating ram for cnode\n", my_core_id);
    errval_t err;
    err = ram_alloc(&ns->cap, L2_CNODE_BITS+OBJBITS_CTE);
    assert(err_is_ok(err));
    ns->numcopies = NUM_COPIES_START;
}

void node_cmd(uint32_t cmd, uint32_t arg, struct bench_distops_binding *b)
{
    struct node_state *ns = b->st;
    errval_t err;

    switch(cmd) {
        case BENCH_CMD_DO_DELETE:
            ns->benchcount = arg;
            ns->delcycles = calloc(arg, sizeof(uint64_t));
            assert(ns->delcycles);
            struct capref cn, slot, tmp;
            printf("# node %d: doing deletes\n", my_core_id);
            for (int i = 0; i < ns->benchcount; i++) {
                // create CNode
                err = slot_alloc_root(&cn);
                assert(err_is_ok(err));
                assert(!capref_is_null(ns->cap));
                err = cnode_create_from_mem(cn, ns->cap, ObjType_L2CNode,
                        &slot.cnode, L2_CNODE_SLOTS);
                assert(err_is_ok(err));
                // put caps in
                for (slot.slot = 0; slot.slot < ns->numcopies; slot.slot++) {
                    err = ram_alloc(&tmp, BASE_PAGE_BITS);
                    assert(err_is_ok(err));
                    err = cap_copy(slot, tmp);
                    assert(err_is_ok(err));
                    err = cap_destroy(tmp);
                    assert(err_is_ok(err));
                }
                // delete CNode
                uint64_t start, end;
                start = bench_tsc();
                err = cap_delete(cn);
                end = bench_tsc();
                ns->delcycles[i] = end - start;
                assert(err_is_ok(err));
                err = slot_free(cn);
                assert(err_is_ok(err));
            }
            //printf("node %d: deletes done\n", my_core_id);
            break;
        case BENCH_CMD_PRINT_STATS:
            printf("# node %d: tsc_per_us = %ld; numcopies = %d\n",
                    my_core_id, bench_tsc_per_us(), ns->numcopies);
            printf("# delete latency in cycles\n");
            for (int i = 0; i < ns->benchcount; i++) {
                printf("%ld\n", ns->delcycles[i]);
            }
            err = bench_distops_cmd__tx(b, NOP_CONT, BENCH_CMD_PRINT_DONE, 0);
            assert(err_is_ok(err));
            // Cleanup before next round
            free(ns->delcycles);
            ns->numcopies *= 2;
            break;
        default:
            printf("node %d got command %"PRIu32"\n", my_core_id, cmd);
            break;
    }
}

void node_cmd_caps(uint32_t cmd, uint32_t arg, struct capref cap1,
                   struct bench_distops_binding *b)
{
    switch (cmd) {
        default:
            printf("node %d got caps + command %"PRIu32", arg=%d:\n",
                my_core_id, cmd, arg);
            debug_capref("cap1:", cap1);
            break;
    }
}
