/**
 * \file
 * \brief Test program for large page code
 */

/*
 * Copyright (c) 2014, HP Labs.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h>

#define DEFAULT_RUNS 2
// 128MB buffer
#define DEFAULT_BUFSIZE (128UL*1024*1024)

int main(int argc, char *argv[])
{
    unsigned long BUFSIZE = DEFAULT_BUFSIZE;
    unsigned RUNS = DEFAULT_RUNS;
    if (argc == 2) {
        if (strcmp(argv[1], "-h") == 0) {
            debug_printf("usage: %s <bufsize> <runs>\n", argv[0]);
            debug_printf("  both arguments are optional, defaults are:\n");
            debug_printf("    BUFSIZE = %lu\n", DEFAULT_BUFSIZE);
            debug_printf("    RUNS = %u\n", DEFAULT_RUNS);
            return 0;
        }
        BUFSIZE = strtol(argv[1], NULL, 0);
    }
    if (argc == 3) {
        RUNS = strtol(argv[1], NULL, 0);
    }
    debug_printf("running malloc test with BUFSIZE = %lu, runs = %u\n", BUFSIZE, RUNS);
    void *bufs[RUNS];
    for (int k = 0; k < RUNS; k++) {
        // touch every 4k page in region
        bufs[k] = malloc(BUFSIZE);
        if (!bufs[k]) {
            debug_printf("malloc %d FAILED\n", k);
            break;
        }
        uint8_t *buf = bufs[k];
        for (int i = 0; i < BUFSIZE / BASE_PAGE_SIZE; i++) {
            buf[i*BASE_PAGE_SIZE] = i % 256;
        }
        // clear out caches
        sys_debug_flush_cache();
        int errors = 0;
        for (int i = 0; i < BUFSIZE / BASE_PAGE_SIZE; i++) {
            if (buf[i*BASE_PAGE_SIZE] != i % 256) {
                debug_printf("mismatch in page %d: expected %d, was %d\n",
                        i, i % 256, buf[i*BASE_PAGE_SIZE]);
                errors++;
            }
        }
        debug_printf("test %s\n", errors ? "FAILED" : "PASSED");
        if (errors) {
            debug_printf("  %d errors\n", errors);
        }
    }
    for (int k = 0; k < RUNS; k++) {
        debug_printf("bufs[%d] = %p\n", k, bufs[k]);
    }
    debug_dump_hw_ptables();
    for (int k = 0; k < RUNS; k++) {
        free(bufs[k]);
    }
    return 0;
}
