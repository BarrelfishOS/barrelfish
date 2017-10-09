/**
 * \file
 * \brief Benchmark revoke of cap with no remote relations (no framework)
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

#define REVOKE_COPIES 10

//{{{1 Management node: state management

struct global_state {
    struct capref ram;
    int nodecount;
    int copies_done;
    int currcopies;
};

errval_t mgmt_init_benchmark(void **st, int nodecount)
{
     *st = malloc(sizeof(struct global_state));
     if (!*st) {
         return LIB_ERR_MALLOC_FAIL;
     }
     struct global_state *gs = *st;
     gs->nodecount = nodecount;
     gs->copies_done = 0;
     return ram_alloc(&gs->ram, BASE_PAGE_BITS);
}

struct node_state {
    struct capref cap;
    struct capref ram;
    uint32_t numcopies;
    struct capref *copies;
    uint64_t *delcycles;
    uint32_t benchcount;
};

static coreid_t my_core_id = -1;

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

static size_t get_mdb_size(void)
{
    errval_t err;
    size_t cap_base_count = 0;
    err = sys_debug_get_mdb_size(&cap_base_count);
    assert(err_is_ok(err));
    return cap_base_count;
}

int main(int argc, char *argv[])
{
    if (argc > 1) {
        printf("# Benchmarking REVOKE NO REMOTE: nodes=%d\n", atoi(argv[1]));

        printf("# Starting out with %d copies, will double up to %d...\n",
                NUM_COPIES_START, NUM_COPIES_END);
    }

    struct global_state *gs;
    errval_t err;

    // Initialize global state
    err = mgmt_init_benchmark((void**)&gs, 1);
    assert(err_is_ok(err));

    // Initialize node state
    struct node_state ns_;
    struct node_state *ns = &ns_;
    ns->ram = gs->ram;
    ns->benchcount = ITERS;
    ns->delcycles = calloc(ns->benchcount, sizeof(uint64_t));
    assert(ns->delcycles);

    my_core_id = disp_get_core_id();

    bench_init();

    // allocate slots for copies
    struct capref copies[REVOKE_COPIES];
    for (int c = 0; c < REVOKE_COPIES; c++) {
        err = slot_alloc(&copies[c]);
        assert(err_is_ok(err));
    }

    for (gs->currcopies = NUM_COPIES_START;
         gs->currcopies <= NUM_COPIES_END;
         gs->currcopies *= 2)
    {
        ns->numcopies = gs->currcopies;
        printf("# node %d: creating %d cap copies\n", my_core_id, ns->numcopies);
        node_create_copies(ns);
        printf("# node %d: %zu capabilities on node\n", my_core_id, get_mdb_size());
        struct capref cap;
        err = ram_alloc(&cap, BASE_PAGE_BITS);
        assert(err_is_ok(err));
        // printf("# node %d: starting benchmark iterations\n", my_core_id);
        for (int i = 0; i < ns->benchcount; i++) {
            uint64_t start, end;
            // Make some copies to be deleted during revoke
            // printf("# node %d: creating copies\n", my_core_id);
            for (int c = 0; c < REVOKE_COPIES; c++) {
                err = cap_copy(copies[c], cap);
                PANIC_IF_ERR(err, "creating copy for revoke");
                assert(err_is_ok(err));
            }
            // printf("# node %d: doing revoke\n", my_core_id);
            start = bench_tsc();
            err = cap_revoke(cap);
            end = bench_tsc();
            ns->delcycles[i] = end - start;
            assert(err_is_ok(err));
            PANIC_IF_ERR(err, "# core %d: revoke failed", my_core_id);
            if (i % (ns->benchcount / 10) == 0) {
                // printf("# node %d: %d percent done\n", my_core_id, i / (ns->benchcount/100));
            }
        }
        err = cap_destroy(cap);
        assert(err_is_ok(err));
        PANIC_IF_ERR(err, "# core %d: final cap_destroy", my_core_id);

        printf("# node %d: tsc_per_us = %ld; numcopies = %d\n",
                my_core_id, bench_tsc_per_us(), ns->numcopies);
        printf("# delete latency in cycles\n");
        for (int i = 0; i < ns->benchcount; i++) {
            printf("%ld\n", ns->delcycles[i]);
        }
        // Cleanup before next round
        for (int i = 0; i < ns->numcopies; i++) {
            err = cap_destroy(ns->copies[i]);
            assert(err_is_ok(err));
        }
        free(ns->copies);
        printf("# Round done!\n");
    }
    // We're not printing this line here, as it's printed from the standalone
    // runner, that allows us to run single-core benchmarks sequentially on
    // multiple cores
    // printf("# Benchmark done!\n");
    return 0;
}
