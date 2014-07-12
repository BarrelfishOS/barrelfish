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

#define RUNS 2

int main(void)
{
    errval_t err;
    struct capref frame;
    size_t retsize;
    void *vbuf;
    struct vregion *vregion;
    uint8_t *buf;
    int errors;

    // get frame
    err = frame_alloc(&frame, X86_64_HUGE_PAGE_SIZE, &retsize);
    assert(retsize >= X86_64_HUGE_PAGE_SIZE);
    if (err_is_fail(err)) {
        debug_printf("frame_alloc: %s\n", err_getstring(err));
        return 1;
    }

    for (int k = 0; k < RUNS; k++) {
        debug_printf("Running 2M/1G test\n");
        // map with alignment and large flag (don't need memobj)
        err = vspace_map_one_frame_attr_aligned(&vbuf, retsize, frame,
                VREGION_FLAGS_READ_WRITE | VREGION_FLAGS_LARGE,
                X86_64_HUGE_PAGE_SIZE, NULL, &vregion);
        if (err_is_fail(err)) {
            debug_printf("vspace_map: %s\n", err_getstring(err));
            return 1;
        }

        debug_printf("vaddr: %p\n", vbuf);

        // touch every 4k page in region
        buf = vbuf;
        for (int i = 0; i < X86_64_HUGE_PAGE_SIZE / X86_64_BASE_PAGE_SIZE; i++) {
            buf[i*BASE_PAGE_SIZE] = i % 256;
        }
        // clear out caches
        sys_debug_flush_cache();
        errors = 0;
        for (int i = 0; i < X86_64_HUGE_PAGE_SIZE / X86_64_BASE_PAGE_SIZE; i++) {
            if (buf[i*BASE_PAGE_SIZE] != i % 256) {
                debug_printf("mismatch in page %d: expected %d, was %d\n",
                        i, i % 256, buf[i*BASE_PAGE_SIZE]);
                errors++;
            }
        }
        debug_printf("2M/1G test %s\n", errors ? "FAILED" : "PASSED");
        if (errors) {
            debug_printf("  %d errors\n", errors);
        }

        vregion_destroy(vregion);
    }
    for (int k = 0; k < RUNS; k++) {
        debug_printf("Running 2M/2M test\n");
        // map with large flag
        err = vspace_map_one_frame_attr_aligned(&vbuf, retsize, frame,
                VREGION_FLAGS_READ_WRITE | VREGION_FLAGS_LARGE,
                X86_64_LARGE_PAGE_SIZE, NULL, &vregion);
        if (err_is_fail(err)) {
            debug_printf("vspace_map: %s\n", err_getstring(err));
            return 1;
        }

        debug_printf("vaddr: %p\n", vbuf);

        // touch every 4k page in region
        buf = vbuf;
        for (int i = 0; i < X86_64_HUGE_PAGE_SIZE / X86_64_BASE_PAGE_SIZE; i++) {
            buf[i*BASE_PAGE_SIZE] = i % 256;
        }
        // clear out caches
        sys_debug_flush_cache();
        errors = 0;
        for (int i = 0; i < X86_64_HUGE_PAGE_SIZE / X86_64_BASE_PAGE_SIZE; i++) {
            if (buf[i*BASE_PAGE_SIZE] != i % 256) {
                debug_printf("mismatch in page %d: expected %d, was %d\n",
                        i, i % 256, buf[i*BASE_PAGE_SIZE]);
                errors++;
            }
        }
        debug_printf("2M/2M test %s\n", errors ? "FAILED" : "PASSED");
        if (errors) {
            debug_printf("  %d errors\n", errors);
        }

        vregion_destroy(vregion);
    }

    uint32_t eax, ebx, ecx, edx;
    cpuid(0x80000001, &eax, &ebx, &ecx, &edx);
    if ((edx >> 26) & 1) {
        debug_printf("Running 1G/1G test\n");
        err = vspace_map_one_frame_attr_aligned(&vbuf, retsize, frame,
                VREGION_FLAGS_READ_WRITE | VREGION_FLAGS_HUGE,
                X86_64_HUGE_PAGE_SIZE, NULL, &vregion);
        if (err_is_fail(err)) {
            debug_printf("vspace_map: %s\n", err_getstring(err));
            return 1;
        }

        // touch every 4k page in region
        buf = vbuf;
        for (int i = 0; i < X86_64_HUGE_PAGE_SIZE / X86_64_BASE_PAGE_SIZE; i++) {
            buf[i*BASE_PAGE_SIZE] = i % 256;
        }
        // clear out caches
        sys_debug_flush_cache();
        errors = 0;
        for (int i = 0; i < X86_64_HUGE_PAGE_SIZE / X86_64_BASE_PAGE_SIZE; i++) {
            if (buf[i*BASE_PAGE_SIZE] != i % 256) {
                debug_printf("mismatch in page %d: expected %d, was %d\n",
                        i, i % 256, buf[i*BASE_PAGE_SIZE]);
                errors++;
            }
        }
        debug_printf("1G/1G test %s\n", errors ? "FAILED" : "PASSED");
        if (errors) {
            debug_printf("  %d errors\n", errors);
        }
    }
    return 0;
}
