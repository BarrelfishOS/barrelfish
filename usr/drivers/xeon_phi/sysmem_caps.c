/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>

#include <mm/mm.h>
#include <xeon_phi/xeon_phi.h>

#include "xeon_phi_internal.h"
#include "sysmem_caps.h"

/// the number of slots to allocate for the allocator
#define NUM_SLOTS 2048

#define NUM_CHILDREN 2

/*
 * XXX: This manager relies on the 1:1 mapping of the system memory
 *      in the system memory page tables!
 */

/// the memory manager for the system memory
static struct mm sysmem_manager;

/// offset to the base address
static lpaddr_t base_offset = 0;

/// the slot allocator
static struct range_slot_allocator sysmem_allocator;

/*
 * ----------------------------------------------------------------------------
 * System Memory Latency Benchmark
 * ----------------------------------------------------------------------------
 */
#ifdef __k1om__
#define SYSMEM_BENCH_ENABLED 0
#else
#define SYSMEM_BENCH_ENABLED 0
#endif

#if SYSMEM_BENCH_ENABLED
#include <barrelfish/nameservice_client.h>
#include <bench/bench.h>
#include <limits.h>
#include <dma/dma_bench.h>

#define EXPECT_SUCCESS(_err, msg...) \
    if (err_is_fail(_err)) {USER_PANIC_ERR(_err, msg);}

#define CHACHE_L1_SIZE (32UL * 1024)
#define CHACHE_LINE_SIZE 64
#ifdef __k1om__
#define CHACHE_LL_SIZE (28UL*1024*1024 + 512UL * 1024)
#define DIMENSIONS 4
#else
#define CHACHE_LL_SIZE (25UL*1024*1024)
#define DIMENSIONS 2
#endif
#define WORKSET_SIZE_MULT 16
#define WORKSET_SIZE (WORKSET_SIZE_MULT * CHACHE_LL_SIZE)

/// the number of benchmark rounds to execute
#define RUN_COUNT 1000

/// number of loop iterations of 10k operations
#define LOOP_ITERATIONS 1000

/// loop unrolling factor {10, 50, 100, 500, 1000, 5000}
#define LOOP_UNROLLING 1000

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
    uint8_t pad[CHACHE_LINE_SIZE - sizeof(void *)];
};

struct celem {
    struct celem *next;
};

static uint32_t *elem_id = NULL;

/**
 * \brief calculates the time difference between two time stamps with overhead
 *
 * \param tsc_start start time stamp
 * \param tsc_end   end time stamp
 *
 * \returns elapsed time in cycles
 */
static inline cycles_t sysmem_bench_calculate_time(cycles_t tsc_start,
                                                   cycles_t tsc_end)
{
    cycles_t result;
    if (tsc_end < tsc_start) {
        result = (LONG_MAX - tsc_start) + tsc_end - bench_tscoverhead();
    } else {
        result = (tsc_end - tsc_start - bench_tscoverhead());
    }

    return result;
}

/**
 * \brief generates a shuffled index array for randomized access
 *
 * \param num   number of elements in the array
 */
static void sysmem_bench_generate_shuffle(size_t num)
{
    if (elem_id) {
        return;
    }

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

static void sysmem_bench_init_memory(struct elem *mem,
                                     size_t num)
{
    sysmem_bench_generate_shuffle(num);

    /* do the linkage */
    struct elem *head = &mem[elem_id[0]];
    for (uint32_t i = 1; i < num; ++i) {
        head->next = &mem[elem_id[i]];
        head = head->next;
    }
    mem[elem_id[num-1]].next = &mem[elem_id[0]];
}

#ifdef __k1om__
static lvaddr_t requested_size = 0;
static lvaddr_t requested_size_other = (2UL * 1024 * 1024 * 1024);
#else
static lvaddr_t requested_size = 0;
#endif
static void sysmem_bench_alloc_memory(void **mem,
                                      uint8_t other_phi,
                                      size_t size)
{

    errval_t err;

    uint8_t bits = 0;
    while(size > (1UL << bits)) {
        bits++;
    }

#ifdef __k1om__
    lvaddr_t base = 0;
    if (other_phi) {
        base += 31 * XEON_PHI_SYSMEM_PAGE_SIZE;
        base += requested_size_other;
        requested_size_other += (1UL << (bits + 1));
    } else {
        base += XEON_PHI_SYSMEM_PAGE_SIZE << 1;
        base += requested_size;
        requested_size += (1UL << (bits + 1));
    }
#else
    lvaddr_t base = (2UL * 1024 * 1024 * 1024);
    base += requested_size;
    requested_size += (1UL << (bits + 1));
#endif



    debug_printf("requesting: %lx, %u bits\n", base, bits);

    struct capref frame;
    err = sysmem_cap_request(base, bits, &frame);
    EXPECT_SUCCESS(err, "sysmem cap request");

    void *addr;
    err = vspace_map_one_frame(&addr, size, frame, NULL, NULL);
    EXPECT_SUCCESS(err, "mapping of frame failed");

    if (mem) {
        *mem = addr;
    }
}

static cycles_t sysmem_bench_run_round(void *buffer, volatile void **ret_elem)
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

    return sysmem_bench_calculate_time(tsc_start, tsc_end) / (LOOP_ITERATIONS * LOOP_UNROLLING);
}

static void sysmem_bench_run(void)
{

#ifdef __k1om__
    errval_t err = nameservice_blocking_lookup("all_spawnds_up", NULL);
    EXPECT_SUCCESS(err, "all_spawnds_up");
#endif

    debug_printf("==========================================================\n");
    debug_printf("Running sysmem bench\n");
    debug_printf("==========================================================\n");

    bench_init();

    cycles_t tscperus = bench_tsc_per_us();

    assert(sizeof(struct elem) == CACHE_LINE_SIZE);

    size_t num_elements = (WORKSET_SIZE) / sizeof(struct elem);

    void *sysmem;
    sysmem_bench_alloc_memory(&sysmem, 0, 2*DMA_BENCH_BUFFER_SIZE);

    void *local = malloc(DMA_BENCH_BUFFER_SIZE);


    struct elem *ll_elements;
    sysmem_bench_alloc_memory((void **)&ll_elements, 0, WORKSET_SIZE);
    sysmem_bench_init_memory(ll_elements, num_elements);


    struct celem *l1_elements;
    sysmem_bench_alloc_memory((void **)&l1_elements, 0, CHACHE_L1_SIZE);

    size_t cache_elements = (CHACHE_L1_SIZE / sizeof(struct celem)) >> 2;
    for (uint32_t i = 0; i < cache_elements - 1; ++i) {
        l1_elements[i].next = &l1_elements[i+1];
    }
    l1_elements[cache_elements-1].next = l1_elements;

#ifdef __k1om__
    void *otherphi;
    sysmem_bench_alloc_memory(&otherphi, 1, 2*DMA_BENCH_BUFFER_SIZE);

    struct elem *oll_elements;
    sysmem_bench_alloc_memory((void **)&oll_elements, 1, WORKSET_SIZE);
    sysmem_bench_init_memory(oll_elements, num_elements);

    struct celem *l1o_elements;
    sysmem_bench_alloc_memory((void **)&l1o_elements, 1, CHACHE_L1_SIZE);

    for (uint32_t i = 0; i < cache_elements - 1; ++i) {
        l1o_elements[i].next = &l1o_elements[i+1];
    }
    l1o_elements[cache_elements-1].next = l1o_elements;
#endif

    debug_printf("starting benchmark %u rounds\n", RUN_COUNT);

    debug_printf("memcpy: LOCAL -> REMOTE\n");
    dma_bench_run_memcpy(sysmem, local);

    debug_printf("memcpy:REMOTE -> LOCAL\n");
    dma_bench_run_memcpy(local, sysmem);

#ifdef __k1om__
    debug_printf("memcpy: LOCAL -> OTHERPHI\n");
    dma_bench_run_memcpy(otherphi, local);

    debug_printf("memcpy: OTHERPHI -> LOCAL\n");
    dma_bench_run_memcpy(local, otherphi);
#endif

    bench_ctl_t *ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, DIMENSIONS, RUN_COUNT);
    cycles_t result[DIMENSIONS];
    uint32_t rounds_done = 0;

    do {
        volatile void *element;
        result[0] = sysmem_bench_run_round(&ll_elements[elem_id[0]], &element);

        /* just a access to the variable */
        if (!element) {
            debug_printf("element %p was null.\n", element);
        }

#ifdef __k1om__
        debug_printf("sysmem_bench_run_round(&oll_elements[elem_id[0]], &element);\n");
        result[2] = sysmem_bench_run_round(&oll_elements[elem_id[0]], &element);

        /* just a access to the variable */
        if (!element) {
            debug_printf("element %p was null.\n", element);
        }

        debug_printf("sysmem_bench_run_round(&l1o_elements[0], &element);\n");
        result[3] = sysmem_bench_run_round(&l1o_elements[0], &element);
        /* just a access to the variable */
        if (!element) {
            debug_printf("element %p was null.\n", element);
        }

#endif
        volatile struct celem *e2 = l1_elements;
        for (uint32_t i = 0; i < cache_elements; ++i) {
            NEXT_1000(e2);
        }

        result[1] = sysmem_bench_run_round(&l1_elements[0], &element);

        /* just a access to the variable */
        if (!element) {
            debug_printf("element %p was null.\n", element);
        }

        debug_printf("round: %u of %u\n", ++rounds_done, RUN_COUNT);

    } while (!bench_ctl_add_run(ctl, result));

    debug_printf("---------------------------------------------------------\n");
    bench_ctl_dump_analysis(ctl, 0, "memlatency sysmem", tscperus);
#ifdef __k1om__
    bench_ctl_dump_analysis(ctl, 2, "memlatency other", tscperus);
    bench_ctl_dump_analysis(ctl, 3, "memlatency other cached", tscperus);
#endif
    bench_ctl_dump_analysis(ctl, 1, "cachelatency sysmem", tscperus);
    debug_printf("---------------------------------------------------------\n");
    while(1);
}

#endif

/*
 * ----------------------------------------------------------------------------
 * Interface
 * ----------------------------------------------------------------------------
 */

/**
 * \brief Initializes the capability manager of the system memory range
 *
 * \return SYS_ERR_OK on success,
 */
errval_t sysmem_cap_manager_init(struct capref sysmem_cap)
{
    errval_t err;

    // initialize the memory allcator
    XSYSMEM_DEBUG("Initializing slot allocator of %i slots\n", NUM_SLOTS);
    err = range_slot_alloc_init(&sysmem_allocator, NUM_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC_INIT);
    }

    struct frame_identity ret;
    err = invoke_frame_identify(sysmem_cap, &ret);
    if (err_is_fail(err)) {
        return err;
    }

    base_offset = ret.base;

    XSYSMEM_DEBUG("Initializing memory manager\n");

    /*
     * initialize the memory manager.
     *
     * Important: the type has to be DevFrame, we do not want to zero out the
     *            host memory!
     */
    err = mm_init(&sysmem_manager, ObjType_DevFrame, ret.base, ret.bits,
                  NUM_CHILDREN, slab_default_refill, slot_alloc_dynamic,
                  &sysmem_allocator, false);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_INIT);
    }

    XSYSMEM_DEBUG("Adding cap: [0x%016lx, %i]\n", ret.base, ret.bits);
    err = mm_add(&sysmem_manager, sysmem_cap, ret.bits, ret.base);
    if (err_is_fail(err)) {
        return err;
    }

#if SYSMEM_BENCH_ENABLED
#ifdef __k1om__
    if (disp_xeon_phi_id()==1) {
        sysmem_bench_run();
    }
#else
    if (disp_get_core_id() >= 20) {
        sysmem_bench_run();
    } else {
        while(1)
            ;
    }
#endif
#endif
    return SYS_ERR_OK;
}

/**
 * \brief Returns a previously requested system memory capability to the
 *        cap manager
 */
errval_t sysmem_cap_return(struct capref frame)
{
    errval_t err;
    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    return mm_free(&sysmem_manager, frame, id.base, id.bits);
}

/**
 * \brief Requests a certain system memory capability based on the base and
 *        length requirements
 *
 * \param base  the base address of the system memory (host address)
 * \param bits  the size of the requested capability in bits
 * \param frame capability representing the system memory frame
 *
 * \retval SYS_ERR_OK on success
 *
 * Note: the caller must check the size and base of the frame...
 */
errval_t sysmem_cap_request(lpaddr_t base,
                            uint8_t bits,
                            struct capref *frame)
{
    errval_t err;

    XSYSMEM_DEBUG("Requesting cap for [0x%016lx, %i]\n", base, bits);
    // the size and base must not exceed the maximum range (512G)
    assert(bits < 40);
    assert(!(base & (BASE_PAGE_SIZE-1)));

    // align the base to the next 4k boundary
    //size += (base & (BASE_PAGE_SIZE-1));
    // base -= (base & (BASE_PAGE_SIZE-1));

    // size = (size+BASE_PAGE_SIZE-1) & ~(BASE_PAGE_SIZE - 1);

    // transform the address into the host memory range
    base += base_offset;

    err = mm_alloc_range(&sysmem_manager, bits, base, base + (1UL << bits), frame,
                         NULL);

    if (err_is_fail(err)) {
        XSYSMEM_DEBUG("Try reallocation for  [0x%016lx, %i]\n", base, bits);
        err = mm_realloc_range(&sysmem_manager, bits, base, frame);
        if (err_is_fail(err)) {
            return err;
        }
    }
    return SYS_ERR_OK;
}
