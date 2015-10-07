/** \file
 *  \brief Copy-on-write example application
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>
#include <barrelfish/threads.h>
#include <barrelfish/sys_debug.h>
#include <barrelfish/core_state.h>
#include <bench/bench.h>

#include <stdio.h>
#include <stdlib.h>

#include "vspace_cow.h"
#include "pmap_cow.h"

#include "debug.h"

#define BUFSIZE (32UL*1024)

#ifdef VSPACE_COW
static int vspace_cow_setup_bench(size_t buffer_size) {
    errval_t err;
    struct capref frame;
    size_t retsize;
    void *vbuf;
    struct vregion *vregion;
    uint8_t *buf;

    err = frame_alloc(&frame, buffer_size, &retsize);
    assert(retsize >= buffer_size);
    if (err_is_fail(err)) {
        DEBUG_COW("frame_alloc: %s\n", err_getstring(err));
        return 1;
    }
    DEBUG_COW("%s:%d: %zu\n", __FUNCTION__, __LINE__, retsize);
    // setup region
    err = vspace_map_one_frame_attr(&vbuf, retsize, frame,
            VREGION_FLAGS_READ_WRITE, NULL, &vregion);
    if (err_is_fail(err)) {
        DEBUG_COW("vspace_map: %s\n", err_getstring(err));
        return 1;
    }
    DEBUG_COW("vaddr: %p\n", vbuf);

    // write stuff to region
    buf = vbuf;
    DEBUG_COW("%s:%d: %p, %lu pages\n", __FUNCTION__, __LINE__, buf, buffer_size / BASE_PAGE_SIZE);
    memset(buf, 0xAA, buffer_size);

    DEBUG_COW("%s:%d\n", __FUNCTION__, __LINE__);
    // create cow copy
    //  setup exception handler

    void *cow_vbuf;
    struct vregion *cow_vregion;
    //DEBUG_COW("%s:%d\n", __FUNCTION__, __LINE__);
    cycles_t start = bench_tsc();
    //  create r/o copy of region and tell exception handler bounds
    DEBUG_COW("%s:%d\n", __FUNCTION__, __LINE__);
    err = vspace_map_one_frame_cow(&cow_vbuf, retsize, frame,
            VREGION_FLAGS_READ, NULL, &cow_vregion, BASE_PAGE_SIZE);
    if (err_is_fail(err)) {
        DEBUG_COW("vspace_map: %s\n", err_getstring(err));
        return 1;
    }

    cycles_t cow = bench_tsc();

    err = cow_init(buffer_size, BASE_PAGE_SIZE, cow_vbuf, cow_vregion);
    assert(err_is_ok(err));


    cycles_t end = bench_tsc();
    DEBUG_COW("cow_vbuf = %p\n", cow_vbuf);
    printf("[xx] %"PRIu64", %"PRIuCYCLES", %"PRIuCYCLES"\n",
           buffer_size,
           bench_tsc_to_ms(bench_time_diff(start, cow)),
           bench_tsc_to_ms(bench_time_diff(cow, end)));
    return 0;
}
#endif

int main(int argc, char *argv[])
{
    bench_init();
    DEBUG_COW("%s:%d\n", __FUNCTION__, __LINE__);

#ifdef VSPACE_COW
    printf("[xx] buffer, ms map, ms map\n");
    for (size_t buffer_bits = 27; buffer_bits < 36; buffer_bits++) {
        uint64_t buffer_size = 1 << buffer_bits; // 32*1024ULL;
        int r = vspace_cow_setup_bench(buffer_size);
        assert (r == 0);
    }
#endif

    errval_t err;
    struct vspace_mmu_aware *heap = malloc(sizeof(*heap));
    err = vspace_mmu_aware_init_aligned(heap, get_default_slot_allocator(),
                                        1ULL << 30, 1ULL << 39,
                                        VREGION_FLAGS_READ_WRITE);
    assert(err_is_ok(err));
    size_t size;
    void *buf;
    err = vspace_mmu_aware_map(heap, LARGE_PAGE_SIZE, &buf, &size);
    assert(err_is_ok(err));

    memset(buf, 0x55, LARGE_PAGE_SIZE);

    // setup pmap cow framework
    err = pmap_cow_init();
    assert(err_is_ok(err));

    // cow-enable malloc() heap
    void *newbuf;
    err = pmap_setup_cow(&heap->vregion, &newbuf);
    assert(err_is_ok(err));

    debug_printf("first byte (old) = %hhx\n", *(char *)buf);
    debug_printf("first byte (new) = %hhx\n", *(char *)newbuf);

    *(int *)buf = 0xAA;

    debug_printf("first byte (old) = %hhx\n", *(char *)buf);
    debug_printf("first byte (new) = %hhx\n", *(char *)newbuf);

    return EXIT_SUCCESS;
}
