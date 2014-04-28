/**
 * \file
 * \brief Check if we miss a deadline
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

//sudo taskset 0x3 nice --20 ./deadxor > vacherin-linux-taskset-nice20-dead5xor-cycles.dat
//sudo taskset 0x3 nice --20 ./deadmem > vacherin-linux-taskset-nice20-dead1000mem-cycles.dat
// x86boot -dnm -x init_xor -k cpu_single boot 6
// x86boot -dnm -x init_mem -k cpu_single boot 6


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>

#include <lwip/init.h>
#include <lwip/tcpip.h>
#include <lwip/sockets.h>

#include <vfs/vfs_path.h>
#include <vfs/vfs.h>

#include <bench/bench.h>

#define ITERATION 1000000

static size_t DEADLINE_TIMER = 1000;
#define DEADLINE_MIN 0
#define DEADLINE_MAX 9000

#ifdef MEM_CYCLES
static size_t DECREMENT_COUNT = 1000;
#else
static size_t DECREMENT_COUNT = 1;
#endif

volatile size_t j = 0;

static struct {
    double hit;
    double miss;
    double percentage;
}  results[DEADLINE_MAX-DEADLINE_MIN];


static struct {
    uint64_t start;
    uint64_t stop;
} experiments[ITERATION];

#ifdef DEBUG_PRINT
    #define DEBUG(args ...) fprintf(stderr, args)
#else
    #define DEBUG(args ...)
#endif


int main(int argc, char** argv)
{
    bench_init();

    double miss = 0;
    double hit = 0;
    for (size_t k = 0; k < ITERATION; k++) {

        uint64_t start = bench_tsc();
#ifdef XOR_CYCLES
        __asm__ volatile("movq %[cnt], %%rcx\n\t"
                         "_begin_loop:      \n\t"
                         "xorq %%rax, %%rax \n\t"
                         "xorq %%rax, %%rax \n\t"
                         "xorq %%rax, %%rax \n\t"
                         "xorq %%rax, %%rax \n\t"
                         "xorq %%rax, %%rax \n\t"
                         "loop _begin_loop\n\t"
                         :
                         : [cnt] "r" (DECREMENT_COUNT)
                         : "eax", "rcx");
#endif
#ifdef MEM_CYCLES
        j = DECREMENT_COUNT;
        while (j--);
#endif
        uint64_t stop = bench_tsc();
        experiments[k].start = start;
        experiments[k].stop = stop;

        uint64_t deadline = start + DEADLINE_TIMER;

        if (stop > deadline) {
            DEBUG("MISS %"PRIu64" %"PRIu64" %"PRIu64"\n",
                   start, stop, stop-deadline);
            miss += 1;
        }
        else {
            DEBUG("HITT %"PRIu64" %"PRIu64" %"PRIu64"\n",
                   start, stop, deadline-stop);
            hit += 1;
        }
    }

    assert(miss + hit == ITERATION);

    errval_t err = lwip_init_auto();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "lwip failed.");
    }
    vfs_init();

    err = vfs_mkdir("/data");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "mkdir failed.");
    }

    err = vfs_mount("/data", "nfs://10.110.4.4/local/nfs/zgerd/website");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "mount failed.");
    }

    vfs_handle_t handle;
    err = vfs_create("/data/results-bf.dat", &handle);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "create failed.");
    }

    size_t bufsize = 1024;
    char line[bufsize];
    size_t bytes;

    for (size_t k = 0; k < ITERATION; k++) {
        size_t to_write = 0;
        to_write = snprintf(line, bufsize,
                            "%zu %"PRIu64"\n", k,
                            experiments[k].stop - experiments[k].start);
        err = vfs_write(handle, line, to_write, &bytes);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "write failed.");
        }
    }

    err = vfs_close(handle);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "close failed.");
    }


    return 0;
}