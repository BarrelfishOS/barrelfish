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

#ifdef BARRELFISH
#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>
#include <barrelfish/threads.h>
#include <barrelfish/sys_debug.h>
#include <barrelfish/core_state.h>
#include <bench/bench.h>
#include <vfs/vfs.h>

#include "vspace_cow.h"
#include "pmap_cow.h"
#endif

#ifdef __linux__
#include <assert.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h> 
#include <inttypes.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "bench_rdtsc.h"
#define debug_printf printf
typedef uint64_t cycles_t;
#undef VSPACE_COW
#undef USE_NFS
#endif

#include <stdio.h>
#include <stdlib.h>

#include "debug.h"

#define BUFSIZE (32UL*1024)
#define LINE_SIZE 256

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

static int rand_range(int rangeLow, int rangeHigh) {
    double myRand = rand()/(1.0 + RAND_MAX); 
    int range = rangeHigh - rangeLow + 1;
    int myRand_scaled = (myRand * range) + rangeLow;
    return myRand_scaled;
}

#ifdef __linux__
static void do_fork(void* buf, size_t size);
static void do_fork(void* buf, size_t size) {
    int pid;
    if ((pid = fork())) {
        if (pid < 0) {
            printf("fork failed: %d (%s)\n", pid, strerror(pid));
            _exit(1);
        }


    } else {
        // Keep child alive to force COW
        printf("%s:%s:%d:  waiting to exit %d\n", __FILE__, __FUNCTION__, __LINE__, getpid());
        int parent = getppid();
        int status;
        int w = waitpid(pid, &status, 0); 
        if (w == -1) {
            perror("waitpid");
            exit(EXIT_FAILURE);
        }
        printf("%s:%s:%d: child exiting\n", __FILE__, __FUNCTION__, __LINE__);
        _exit(0);
    }
}
#endif

int main(int argc, char *argv[])
{
#ifdef BARRELFISH
    bench_init();
    DEBUG_COW("%s:%d\n", __FUNCTION__, __LINE__);
#if !defined(PMAP_ARRAY)
    USER_PANIC("%s currently NYI for !PMAP_ARRAY\n", argv[0])
#endif
#endif

#ifdef VSPACE_COW
    printf("[xx] buffer, ms map, ms map\n");
    for (size_t buffer_bits = 27; buffer_bits < 36; buffer_bits++) {
        uint64_t buffer_size = 1 << buffer_bits; // 32*1024ULL;
        int r = vspace_cow_setup_bench(buffer_size);
        assert (r == 0);
    }
#endif

#ifdef BARRELFISH
    errval_t err;
    struct vspace_mmu_aware *heap = malloc(sizeof(*heap));
    // create 512 GB heap for experiments
    err = vspace_mmu_aware_init_aligned(heap, get_default_slot_allocator(),
                                        511ULL << 30, 1ULL << 39,
                                        VREGION_FLAGS_READ_WRITE);
    assert(err_is_ok(err));
#endif

    if (argc < 2) {
        debug_printf("usage: %s <allocation size> (<update fraction>)\n",
                __FUNCTION__);
        return 1;
    }
    size_t size = strtol(argv[1], NULL, 0);
    size_t ratio = 10;
    if (argc >= 3) {
        ratio = strtol(argv[2], NULL, 0);
    }
#ifdef BARRELFISH
    void *buf;
    err = vspace_mmu_aware_map(heap, size, &buf, &size);
    assert(err_is_ok(err));
    debug_printf("allocated %zu bytes\n", size);

    cycles_t start = bench_tsc();
    // setup pmap cow framework
    err = pmap_cow_init();
    assert(err_is_ok(err));

    cycles_t setup = bench_tsc();
    // cow-enable heap
    void *newbuf;
    err = pmap_setup_cow(&heap->vregion, &newbuf);
    assert(err_is_ok(err));
    cycles_t end = bench_tsc();

    debug_printf("pmap_init_cow: %"PRIu64"us\n",
            bench_tsc_to_us(bench_time_diff(start, setup)));
    debug_printf("pmap_setup_cow: %"PRIu64"us\n",
            bench_tsc_to_us(bench_time_diff(setup, end)));
#else
#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

    void* buf;
    buf = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (buf == MAP_FAILED) {
        handle_error("mmap");
    }
    memset(buf, 0xAB, size);
    do_fork(buf, size);
    cycles_t start, end;
#endif

    size_t accesses = size / ratio;
    char *wbuf = buf;
    cycles_t *write_latencies = calloc(accesses, sizeof(cycles_t));
    size_t *write_indices = calloc(accesses, sizeof(size_t));
    for (size_t i = 0; i < accesses; i++) {
        size_t index = rand_range(0, size);
        start = bench_tsc();
        //assert ( ((uint8_t)wbuf[index]) == 0xAB );
        wbuf[index] = i % 256;
        end = bench_tsc();
        if (start <= end) {
            write_latencies[i] = end - start;
        } else {
            printf("s-e: %"PRIu64"\n", start-end);
        }
        write_indices[i] = index;
    }

#ifdef  USE_NFS
    vfs_init();
    err = vfs_mkdir("/nfs");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_mount");
    }
    err = vfs_mount("/nfs", "nfs://10.110.4.4/mnt/local/nfs");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_mount");
    }
    vfs_handle_t data;
    err = vfs_create("/nfs/gerbesim/cow.data", &data);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_create");
    }
#endif

    size_t written = 0, k = 0;
    char line[LINE_SIZE] = {'\0'};
    k = snprintf(line, LINE_SIZE, "TSCperUS: %"PRIu64"\n", bench_tsc_per_us());

#ifdef USE_NFS
    err = vfs_write(data, line, LINE_SIZE, &written);
    assert(written == k);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_write");
    }
#else
    written = printf("%s", line);
    assert(written == k);
#endif

    for (size_t i = 0; i < accesses; i++) {
        k = snprintf(line, LINE_SIZE, "%zu, %"PRIu64"\n",
                write_indices[i], write_latencies[i]);
#ifdef USE_NFS
        err = vfs_write(data, line, LINE_SIZE, &written);
        assert(written == k);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "vfs_write");
        }
#else
        printf("%s", line);
#endif
    }
    extern size_t get_ram_caps_count, cow_get_page_count,
           cow_pt_alloc_count, cow_pd_alloc_count, cow_pdpt_alloc_count;
    printf("\n\nDone!\n");
    printf("avg write latency: %"PRIu64"cycles\n",
            bench_avg(write_latencies, accesses));
    printf("max write latency: %"PRIu64"cycles\n",
            bench_max(write_latencies, accesses));
    printf("#get_ram_caps: %zu\n", get_ram_caps_count);
    printf("#cow_get_page: %zu\n", cow_get_page_count);
    printf("#pts   allocated: %zu\n", cow_pt_alloc_count);
    printf("#pds   allocated: %zu\n", cow_pd_alloc_count);
    printf("#pdpts allocated: %zu\n", cow_pdpt_alloc_count);

    return EXIT_SUCCESS;
}
