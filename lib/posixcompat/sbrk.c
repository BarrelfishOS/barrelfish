/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <barrelfish/barrelfish.h>

#if __SIZEOF_POINTER__ == 8
//need lot of memory...
// Really? 61 Gigabytes??!? -AB
#define SBRK_REGION_BYTES (61*256*1024UL * BASE_PAGE_SIZE)
#else // still huge, but slightly more achievable in a 32-bit address space!
#define SBRK_REGION_BYTES (256 * 1024 * 1024)
#endif

void *sbrk(intptr_t increment)
{
    errval_t err;

    static void *base;
    static size_t offset;
    static struct memobj *memobj = NULL;
    static struct vregion *vregion = NULL;

    if (!memobj) { // Initialize
        err = vspace_map_anon_attr(&base, &memobj, &vregion, SBRK_REGION_BYTES,
                                   NULL, VREGION_FLAGS_READ_WRITE);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "vspace_map_anon_attr failed");
            return (void *)-1;
        }
    }

    if (increment < 0) {
        USER_PANIC("sbrk() called with negative increment - NYI");
    } else if (increment == 0) {
        return base + offset;
    } else if (offset + increment > SBRK_REGION_BYTES) {
        debug_printf("sbrk() exceeded static region limit of %zu bytes\n",
                     (size_t)SBRK_REGION_BYTES);
        return (void *)-1;
    }

    size_t inc_bytes = increment;
    size_t orig_offset = offset;

    struct capref frame;
    err = frame_alloc(&frame, inc_bytes, &inc_bytes);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "frame_alloc failed");
        return (void *)-1;
    }

    err = memobj->f.fill(memobj, offset, frame, inc_bytes);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "memobj->f.fill failed");
        cap_destroy(frame);
        return (void *)-1;
    }

    err = memobj->f.pagefault(memobj, vregion, offset, 0);
    offset += inc_bytes;
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err,
                  "memobj->f.pagefault failed");
        return (void *)-1;
    }

    void *ret = base + orig_offset;
    return ret;
}
