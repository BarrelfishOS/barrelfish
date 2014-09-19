/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>

/*
 * ===========================================================================
 * XXX: README
 *
 * The compilation of this file may take quite long due to heavily use of
 * expansive makros in the loop (see next)
 * ===========================================================================
 */


/*
 * ----------------------------------------------------------------------------
 * Benchmark Settings
 * ----------------------------------------------------------------------------
 */

/// the number of benchmark rounds to execute
#define RUN_COUNT 1000

/// number of loop iterations of 10k operations
#define LOOP_ITERATIONS 1000

/// loop unrolling factor {10, 50, 100, 500, 1000, 5000}
#define LOOP_UNROLLING 1000

/// working set multiplier (workset = sizeof(LL_CACHE) * multiplier
#define WORKSET_SIZE_MULT 8

/*
 * ----------------------------------------------------------------------------
 * Machine Settings
 * ----------------------------------------------------------------------------
 */

#ifdef __k1om__
#define CHACHE_L1_SIZE (32UL * 1024)
#define CHACHE_L2_SIZE (512UL * 1024)
#define CHACHE_L3_SIZE (28UL*1024*1024 + 512UL * 1024)
#define CHACHE_LL_SIZE (CHACHE_L3_SIZE)
#define CHACHE_LINE_SIZE 64
#define WORKSET_SIZE (WORKSET_SIZE_MULT * CHACHE_LL_SIZE)
#define NUMA_NODE_MAX 0
#define NUME_MEM_PER_NODE (6UL * 1024 * 1024 * 1024)
#define DIMENSIONS 2
#else
#define CHACHE_L1_SIZE (32UL * 1024)
#define CHACHE_L2_SIZE (256UL * 1024)
#define CHACHE_L3_SIZE (25UL*1024*1024)
#define CHACHE_LL_SIZE (CHACHE_L3_SIZE)
#define CHACHE_LINE_SIZE 64
#define WORKSET_SIZE (WORKSET_SIZE_MULT * CHACHE_LL_SIZE)
#define NUMA_NODE_MAX 1
#define NUME_MEM_PER_NODE (128UL * 1024 * 1024 * 1024)
#define DIMENSIONS 3
#endif


#define EXPECT_SUCCESS(_err, msg...) \
    if (err_is_fail(_err)) {USER_PANIC_ERR(_err, msg);}


#define NEXT(_e) (_e) = (_e)->next;
#define NEXT_5(_e) NEXT(_e) NEXT(_e) NEXT(_e) NEXT(_e) NEXT(_e)
#define NEXT_10(_e) NEXT_5(_e) NEXT_5(_e)
#define NEXT_50(_e) NEXT_10(_e) NEXT_10(_e) NEXT_10(_e) NEXT_10(_e) NEXT_10(_e)
#define NEXT_100(_e) NEXT_50(_e) NEXT_50(_e)
#define NEXT_500(_e) NEXT_100(_e) NEXT_100(_e) NEXT_100(_e) NEXT_100(_e) NEXT_100(_e)
#define NEXT_1000(_e) NEXT_500(_e) NEXT_500(_e)

#if LOOP_UNROLLING == 10000
#define UNROLL_NEXT(_e) NEXT_100(_e)
#elif LOOP_UNROLLING == 5000
#define UNROLL_NEXT(_e) NEXT_500(_e)
#elif LOOP_UNROLLING == 1000
#define UNROLL_NEXT(_e) NEXT_100(_e)
#elif LOOP_UNROLLING == 500
#define UNROLL_NEXT(_e) NEXT_50(_e)
#elif LOOP_UNROLLING == 100
#define UNROLL_NEXT(_e) NEXT_10(_e)
#elif LOOP_UNROLLING == 50
#define UNROLL_NEXT(_e) NEXT_5(_e)
#elif LOOP_UNROLLING == 10
#define UNROLL_NEXT(_e) NEXT(_e)
#endif


#ifndef UNROLL_NEXT
#error "UNROLL_NEXT not defined"
#endif


struct elem {
    struct elem *next;
    uint32_t val;
    uint8_t pad[CHACHE_LINE_SIZE - sizeof(void *) - sizeof(uint32_t)];
};

struct celem {
    struct celem *next;
};


uint32_t *elem_id = NULL;

static inline cycles_t calculate_time(cycles_t tsc_start,
                                      cycles_t tsc_end)
{
    cycles_t result;
    if (tsc_end < tsc_start) {
        result = (LONG_MAX - tsc_start) + tsc_end - bench_tscoverhead();
    } else {
        result = (tsc_end - tsc_start - bench_tscoverhead());
    }

    if (result < (tsc_end - tsc_start)) {
        return result;
    }

    debug_printf("result: %lu / %lu / overhead: %lu\n", result,
                 tsc_end - tsc_start, bench_tscoverhead());

    return result;
}

static uint8_t numa_node(void)
{
#ifdef __k1om__
    return 0;
#else
    return (disp_get_core_id() > 19);
#endif
}

static void generate_shuffle(size_t num)
{
    if (elem_id) {
        return;
    }

    debug_printf("generating shuffle\n");

    elem_id = malloc(sizeof(uint32_t) * num + 1);
    assert(elem_id);

    for (uint32_t i = 0; i < num; ++i) {
        elem_id[i] = i;
    }

    /*
     * shuffle the array using Knuth shuffle
     */
    for (uint32_t i = 0; i < num; ++i) {
        uint32_t idx = i + (rand() % (num - i));
        assert(idx < num + 1);
        uint32_t tmp = elem_id[i];
        elem_id[i] = elem_id[idx];
        elem_id[idx] = tmp;
    }
}

static void init_memory(struct elem *mem,
                        size_t num)
{
    generate_shuffle(num);

    for (uint32_t i = 0; i < num; ++i) {
        mem[i].val = i + 1;
    }

    /* do the linkage */
    struct elem *head = &mem[elem_id[0]];
    for (uint32_t i = 1; i < num; ++i) {
        head->next = &mem[elem_id[i]];
        head = head->next;
    }
    mem[elem_id[num-1]].next = &mem[elem_id[0]];
}

static void alloc_memory(struct elem **mem,
                         uint8_t node)
{
    assert(node <= NUMA_NODE_MAX);

    errval_t err;

#ifndef __k1om__
    uint64_t min_base, max_limit;
    ram_get_affinity(&min_base, &max_limit);

    uint64_t mem_min = NUME_MEM_PER_NODE * node;
    uint64_t mem_max = NUME_MEM_PER_NODE * (node + 1);

    debug_printf("alloc mem: numa=%u, range={%016lx, %016lx}\n", node, mem_min,
                 mem_max);

    ram_set_affinity(mem_min, mem_max);
#endif

    struct capref frame;
    err = frame_alloc(&frame, WORKSET_SIZE, NULL);
    EXPECT_SUCCESS(err, "frame alloc failed\n");

    void *addr;
    err = vspace_map_one_frame(&addr, WORKSET_SIZE, frame, NULL, NULL);
    EXPECT_SUCCESS(err, "mapping of frame failed");

#ifndef __k1om__
    ram_set_affinity(min_base, max_limit);
#endif

    if (mem) {
        *mem = addr;
    }
}

static cycles_t run_benchmark(void *buffer, volatile void **ret_elem)
{
    volatile struct elem *e = buffer;

    cycles_t tsc_start = bench_tsc();

    for (uint32_t i = 0; i < LOOP_ITERATIONS; ++i) {
        UNROLL_NEXT(e);
        UNROLL_NEXT(e);
        UNROLL_NEXT(e);
        UNROLL_NEXT(e);
        UNROLL_NEXT(e);
        UNROLL_NEXT(e);
        UNROLL_NEXT(e);
        UNROLL_NEXT(e);
        UNROLL_NEXT(e);
        UNROLL_NEXT(e);
    }
    cycles_t tsc_end = bench_tsc();

    if (ret_elem) {
        *ret_elem = e;
    }

    return calculate_time(tsc_start, tsc_end) / (LOOP_ITERATIONS * LOOP_UNROLLING);
}

int main(int argc,
         char *argv[])
{

    bench_init();

    debug_printf("memlatency benchmark started...Workset size: %lu MBytes\n",
                 WORKSET_SIZE >> 20);

    if (numa_node()) {
        USER_PANIC("must be run on numa node 0\n");
    }

    assert(sizeof(struct elem) == CACHE_LINE_SIZE);

    cycles_t tscperus = bench_tsc_per_us();

    size_t num_elements = (WORKSET_SIZE) / sizeof(struct elem);
    assert(num_elements < (0x7FFFFFFF));

    debug_printf("allocating elements list: %lu elements\n", num_elements);

    struct elem *ll_elements;
    alloc_memory(&ll_elements, 0);
    init_memory(ll_elements, num_elements);

#ifndef __k1om__
    struct elem *ll_elements_numa;
    alloc_memory(&ll_elements_numa, 1);
    init_memory(ll_elements_numa, num_elements);
#endif

    /*
     * do cache allocations
     */
    size_t cache_elements = (CHACHE_L1_SIZE / sizeof(struct celem)) >> 2;
    debug_printf("initialize cached elements, num:%lu\n", cache_elements);

    struct celem *l1_elements = malloc((cache_elements)* sizeof(struct celem));
    for (uint32_t i = 0; i < cache_elements - 1; ++i) {
        l1_elements[i].next = &l1_elements[i+1];
    }
    l1_elements[cache_elements-1].next = l1_elements;

    /*
     * BENCHMARK
     */

    debug_printf("starting benchmark %u rounds\n", RUN_COUNT);

    bench_ctl_t *ctl;
    cycles_t result[DIMENSIONS];

    uint32_t round = 0;

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, DIMENSIONS, RUN_COUNT);
    do {
        volatile void *element;
        result[0] = run_benchmark(&ll_elements[elem_id[0]], &element);
                /*  make sure the loop is not optimized away */

        /* just a access to the variable */
        if (!element) {
            debug_printf("element %p was null.\n", element);
        }


#ifndef __k1om__
        result[2] = run_benchmark(&ll_elements_numa[elem_id[0]], &element);

        /* just a access to the variable */
        if (!element) {
            debug_printf("element %p was null.\n", element);
        }
#endif

        volatile struct celem *e2 = l1_elements;

        /* warmup cache */
        for (uint32_t i = 0; i < cache_elements; ++i) {
            NEXT_1000(e2);
        }

        e2 = l1_elements;
        result[1] = run_benchmark(l1_elements, &element);

        /* just a access to the variable */
        if (!element) {
            debug_printf("element %p was null.\n", element);
        }

        debug_printf("round: %u of %u\n", ++round, RUN_COUNT);
    } while (!bench_ctl_add_run(ctl, result));

    debug_printf("---------------------------------------------------------\n");
    bench_ctl_dump_analysis(ctl, 0, "memlatency", tscperus);
#ifndef __k1om__
    bench_ctl_dump_analysis(ctl, 2, "memlatency numa", tscperus);
#endif
    bench_ctl_dump_analysis(ctl, 1, "cachelatency", tscperus);
    debug_printf("---------------------------------------------------------\n");
    return 0;
}
