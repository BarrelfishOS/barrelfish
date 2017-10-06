/**
 * \file
 * \brief delete last copy of moveable cap on owned core with copies existing
 *        on other cores
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
#include <trace/trace.h>

//#define DEBUG_PROTOCOL
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
    BENCH_CMD_CREATE_COPIES,
    BENCH_CMD_COPIES_DONE,
    BENCH_CMD_DO_ALLOC,
    BENCH_CMD_DO_DELETE,
    BENCH_CMD_FORWARD_COPIES,
    BENCH_CMD_COPIES_RX_DONE,
    BENCH_CMD_PRINT_DONE,
};

//{{{1 shared helper functions
static size_t get_mdb_size(void)
{
    errval_t err;
    size_t cap_base_count = 0;
    err = sys_debug_get_mdb_size(&cap_base_count);
    assert(err_is_ok(err));
    return cap_base_count;
}


//{{{1 Managment node: implement orchestration for benchmark

//{{{2 Management node: state management

struct global_state {
    struct capref ram;
    struct capref fwdcap;
    int nodecount;
    int copies_done;
    int copycount;
    int rx_seen;
    int masternode;
    int currcopies;
};

static void after_prepare(void *arg)
{
    bool *done = arg;
    *done = true;
}

errval_t mgmt_init_benchmark(void **st, int nodecount)
{
    errval_t err;
    // Initialize benchmarking
    bench_init();
    // Initialize tracing
    printf("# mgmt node: initializing tracing\n");
    trace_reset_all();
    trace_set_autoflush(true);
    // Trace everything
    trace_set_all_subsys_enabled(false);
    trace_set_subsys_enabled(TRACE_SUBSYS_CAPOPS, true);
    err = trace_control(TRACE_EVENT(TRACE_SUBSYS_CAPOPS,
                                    TRACE_EVENT_CAPOPS_START, 0),
                        TRACE_EVENT(TRACE_SUBSYS_CAPOPS,
                                    TRACE_EVENT_CAPOPS_STOP, 0),
                        0);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "unable to enable capops tracing");
    }
    bool trace_prepare_done = false;
    trace_prepare(MKCLOSURE(after_prepare, &trace_prepare_done));

    while (!trace_prepare_done) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "event_dispatch while waiting for trace_prepare");
        }
    }

    *st = malloc(sizeof(struct global_state));
    if (!*st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    struct global_state *gs = *st;
    gs->nodecount = nodecount;
    gs->copies_done = 0;
    gs->rx_seen = 0;
    gs->masternode = 1; // TODO
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

    printf("# Benchmarking DELETE LAST: nodes=%d\n", gs->nodecount);

    // only for tracing
    NUM_COPIES_END = NUM_COPIES_START;

    printf("# Starting out with %d copies, will by powers of 2 up to %d...\n",
            NUM_COPIES_START, NUM_COPIES_END);

    TRACE(CAPOPS, START, 0);
    printf("# bench_tsc() just after CAPOPS_START: %"PRIu64"\n", bench_tsc());

    gs->currcopies = NUM_COPIES_START;
    broadcast_caps(BENCH_CMD_CREATE_COPIES, NUM_COPIES_START, gs->ram);
}

void mgmt_cmd(uint32_t cmd, uint32_t arg, struct bench_distops_binding *b)
{
    errval_t err;
    struct global_state *gs = get_global_state(b);

    switch(cmd) {
        case BENCH_CMD_COPIES_DONE:
            gs->copies_done++;
            if (gs->copies_done == gs->nodecount) {
                printf("# All copies made!\n");
                unicast_cmd(gs->masternode, BENCH_CMD_DO_ALLOC, ITERS);
            }
            break;
        case BENCH_CMD_COPIES_RX_DONE:
            gs->rx_seen++;
            DEBUG("got BENCH_CMD_COPIES_RX_DONE: seen = %d, expected = %d\n",
                    gs->rx_seen, gs->copycount);
            if (gs->rx_seen == gs->copycount) {
                DEBUG("# All nodes have copies of cap-to-del\n");
                err = cap_destroy(gs->fwdcap);
                assert(err_is_ok(err));
                gs->rx_seen = 0;
                unicast_cmd(gs->masternode, BENCH_CMD_DO_DELETE, 0);
            }
            break;
        case BENCH_CMD_PRINT_DONE:
            if (gs->currcopies == NUM_COPIES_END) {
                printf("# Benchmark done!\n");
                // make sure last chunk of traces is flushed
                TRACE(CAPOPS, STOP, 0);
                trace_flush(NOP_CONT);
                return;
            }
            printf("# Round done!\n");
            printf("# mgmt node mdb size: %zu\n", get_mdb_size());
            // Reset counters for next round
            gs->currcopies *= 2;
            gs->copies_done = 0;
            gs->rx_seen = 0;
            // Start new round
            broadcast_cmd(BENCH_CMD_CREATE_COPIES, gs->currcopies);
            break;
        default:
            printf("mgmt node got unknown command %d over binding %p\n", cmd, b);
            break;
    }
}

void mgmt_cmd_caps(uint32_t cmd, uint32_t arg, struct capref cap1,
                   struct bench_distops_binding *b)
{
    struct global_state *gs = get_global_state(b);
    switch (cmd) {
        case BENCH_CMD_FORWARD_COPIES:
            {
            // XXX:!
            coreid_t cores[] = {4};
            gs->copycount = 1;
            gs->fwdcap = cap1;
            multicast_caps(BENCH_CMD_FORWARD_COPIES, arg, cap1, cores, gs->copycount);
            DEBUG("cmd_fwd_copies: multicast done\n");
            break;
            }
        default:
            printf("mgmt node got caps + command %"PRIu32", arg=%d over binding %p:\n",
                    cmd, arg, b);
            debug_capref("cap1:", cap1);
            break;
    }
}

//{{{1 Node

struct node_state {
    struct capref cap;
    struct capref ram;
    struct capref *copies;
    int numcopies;
    struct capref *ramcopies;
    int numramcopies;
    uint64_t *delcycles;
    uint32_t benchcount;
    uint32_t iter;
    bool benchnode;
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

    struct node_state *ns = b->st;
    ns->benchnode = false;
    ns->ramcopies = NULL;
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

extern bool info;
void node_cmd(uint32_t cmd, uint32_t arg, struct bench_distops_binding *b)
{
    struct node_state *ns = b->st;
    errval_t err;

    switch(cmd) {
        case BENCH_CMD_CREATE_COPIES:
            if (ns->copies) {
                // Cleanup before next round
                for (int i = 0; i < ns->numcopies; i++) {
                    err = cap_destroy(ns->copies[i]);
                    assert(err_is_ok(err));
                }
                free(ns->copies);
            }
            printf("# node %d: creating %d cap copies\n", my_core_id, arg);
            ns->numcopies = arg;
            node_create_copies(ns);
            printf("# node %d: %zu capabilities on node\n", my_core_id, get_mdb_size());
            err = bench_distops_cmd__tx(b, NOP_CONT, BENCH_CMD_COPIES_DONE, 1);
            PANIC_IF_ERR(err, "signaling cap_copy() done\n");
            break;
        case BENCH_CMD_DO_DELETE:
            DEBUG("# node %d: deleting our copy for benchmark (mdb size %zu)\n",
                    my_core_id, get_mdb_size());
            uint64_t start, end;
            start = bench_tsc();
            TRACE(CAPOPS, USER_DELETE_CALL, (ns->numcopies << 16) | ns->iter);
            err = cap_delete(ns->cap);
            TRACE(CAPOPS, USER_DELETE_RESP, (ns->numcopies << 16) | ns->iter);
            end = bench_tsc();
            ns->delcycles[ns->iter] = end - start;
            assert(err_is_ok(err));
            err = slot_free(ns->cap);
            assert(err_is_ok(err));
            // increase iteration counter
            ns->iter ++;
            // fall-through to next round
        case BENCH_CMD_DO_ALLOC:
            if (arg != 0) {
                DEBUG("Initializing node %d benchmarking meta\n", my_core_id);
                // First call only
                ns->benchcount = arg;
                ns->iter = 0;
                ns->benchnode = true;
                ns->delcycles = calloc(ns->benchcount, sizeof(uint64_t));
                assert(ns->delcycles);
            }
            if (ns->iter == ns->benchcount) {
                // Exit if we've done enough iterations
                printf("# node %d: tsc_per_us = %ld; numcopies = %d\n",
                        my_core_id, bench_tsc_per_us(), ns->numcopies);
                printf("# delete latency in cycles\n");
                for (int i = 0; i < ns->benchcount; i++) {
                    printf("%ld\n", ns->delcycles[i]);
                }
                free(ns->delcycles);
                err = bench_distops_cmd__tx(b, NOP_CONT, BENCH_CMD_PRINT_DONE, 0);
                assert(err_is_ok(err));
                break;
            }
            DEBUG("# node %d: allocating cap for benchmark (mdb size %zu, iter %d)\n",
                    my_core_id, get_mdb_size(), ns->iter);
            err = ram_alloc(&ns->cap, BASE_PAGE_BITS);
            assert(err_is_ok(err));
            // TODO: #copies
            DEBUG("# node %d: forwarding copies for benchmark (mdb size %zu)\n",
                    my_core_id, get_mdb_size());
            err = bench_distops_caps__tx(b, NOP_CONT, BENCH_CMD_FORWARD_COPIES, 10, ns->cap);
            PANIC_IF_ERR(err, "fwd copies");
            assert(err_is_ok(err));
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
            printf("# node %d: %zu caps on node\n", my_core_id, get_mdb_size());
            err = bench_distops_cmd__tx(b, NOP_CONT, BENCH_CMD_COPIES_DONE, 0);
            PANIC_IF_ERR(err, "signaling cap_copy() done\n");
            break;
        case BENCH_CMD_FORWARD_COPIES:
            if (ns->benchnode) {
                printf("# node %d: just deleting forwarded copy\n", my_core_id);
                printf("# node %d: mdb size: %zu\n", my_core_id, get_mdb_size());
                // node on which we benchmark delete
                // delete forwarded copy
                err = cap_destroy(cap1);
                assert(err_is_ok(err));
            } else {
                // other nodes
                if (ns->ramcopies) {
                    DEBUG("# node %d: deleting %d old copies of cap to delete\n",
                            my_core_id, ns->numramcopies);
                    for (int i = 0; i < ns->numramcopies; i++) {
                        err = cap_destroy(ns->ramcopies[i]);
                        if (err_is_fail(err)) {
                            DEBUG_ERR(err, "cap_destroy on node %d", my_core_id);
                        }
                        assert(err_is_ok(err));
                    }
                }
                ns->numramcopies = arg;
                ns->ramcopies = malloc(arg * sizeof(struct capref));
                assert(ns->ramcopies);
                DEBUG("# node %d: creating %d copies of cap to delete\n",
                        my_core_id, ns->numramcopies);
                for (int i = 0; i < ns->numramcopies; i++) {
                    err = slot_alloc(&ns->ramcopies[i]);
                    assert(err_is_ok(err));
                    err = cap_copy(ns->ramcopies[i], cap1);
                    assert(err_is_ok(err));
                }
                err = cap_destroy(cap1);
                assert(err_is_ok(err));
            }
            err = bench_distops_cmd__tx(b, NOP_CONT, BENCH_CMD_COPIES_RX_DONE, 0);
            assert(err_is_ok(err));
            break;
        default:
            printf("node %d got caps + command %"PRIu32", arg=%d:\n",
                my_core_id, cmd, arg);
            debug_capref("cap1:", cap1);
            break;
    }
}
