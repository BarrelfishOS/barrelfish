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
#include <barrelfish/cpu_arch.h>
#include <posixcompat.h> // for sbrk_get_*() declarations

// Uncomment this if you want to measure sbrk() collective runtime
// TODO: make this a Config.hs flag maybe? -SG, 2018-10-18.
// #define SBRK_COLLECT_STATS

#if __SIZEOF_POINTER__ == 8
#ifdef __x86_64__
// Large memory area + large pages on x86_64
#define SBRK_REGION_BYTES (128UL * HUGE_PAGE_SIZE) // 64GB
#define SBRK_FLAGS (VREGION_FLAGS_READ_WRITE | VREGION_FLAGS_LARGE)
#define SBRK_MIN_MAPPING (16 * LARGE_PAGE_SIZE)
#define SBRK_REGION_ALIGNMENT LARGE_PAGE_SIZE
#else
// Large memory area + normal pages
#define SBRK_REGION_BYTES (8UL * 512UL * LARGE_PAGE_SIZE) // 8GB
#define SBRK_FLAGS (VREGION_FLAGS_READ_WRITE)
#define SBRK_MIN_MAPPING (16 * LARGE_PAGE_SIZE)
#define SBRK_REGION_ALIGNMENT BASE_PAGE_SIZE
#endif
#else
// still huge, but slightly more achievable in a 32-bit address space!
#define SBRK_REGION_BYTES (64 * 1024 * BASE_PAGE_SIZE) // 256MB
#define SBRK_FLAGS (VREGION_FLAGS_READ_WRITE)
#define SBRK_MIN_MAPPING (2 * BASE_PAGE_SIZE)
#define SBRK_REGION_ALIGNMENT BASE_PAGE_SIZE
#endif


static void *base = NULL;
static size_t offset = 0; ///< How much is currently used
static size_t goffset = 0; ///< Maximum ever allocated
static struct memobj_append memobj_;
static struct memobj *memobj = NULL;
static struct vregion vregion_;
static struct vregion *vregion = NULL;

struct memobj_anon* sbrk_get_memobj(void)
{
    assert(memobj != NULL);
    return (struct memobj_anon*) memobj;
}

struct vregion* sbrk_get_vregion(void)
{
    assert(vregion != NULL);
    return vregion;
}

void* sbrk_get_base(void)
{
    assert(base != NULL);
    return base;
}

size_t sbrk_get_offset(void)
{
    assert(offset != 0);
    return offset;
}

#ifdef SBRK_COLLECT_STATS
uint64_t sbrk_times = 0;

static inline unsigned long bf_ticks(void)
{
   unsigned int a, d;
   __asm__ volatile("rdtsc" : "=a" (a), "=d" (d));
   return ((unsigned long) a) | (((unsigned long) d) << 32);
}
#endif


void *sbrk(intptr_t increment)
{
    uint64_t start = bf_ticks();
    errval_t err;
    size_t orig_offset;

    if (!memobj) { // Initialize
        err = vspace_map_anon_nomalloc(&base, &memobj_, &vregion_,
                                       SBRK_REGION_BYTES, NULL,
                                       SBRK_FLAGS, SBRK_REGION_ALIGNMENT);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "vspace_map_anon_nomalloc failed");
            return (void *)-1;
        }
        memobj = (struct memobj *) &memobj_;
        vregion = &vregion_;

        //debug_printf("%s:%u reserved region: %p..%p\n", __FUNCTION__, __LINE__,
        //             base, base + SBRK_REGION_BYTES);

    }

    if (increment < 0) {
      if (-increment > offset) {
        USER_PANIC("sbrk() called with negative increment beyond offset");
      } else {
        orig_offset = offset;
        offset += increment;

        void *ret = base + orig_offset;
        return ret;
      }
    } else if (increment == 0) {
        return base + offset;
    } else if (offset + increment > SBRK_REGION_BYTES) {
        debug_printf("sbrk() exceeded static region limit of %zu bytes, offset: %zu\n",
                     (size_t)SBRK_REGION_BYTES, offset);
        return (void *)-1;
    } else if (offset + increment <= goffset) {
        orig_offset = offset;
        offset += increment;

        void *ret = base + orig_offset;
        return ret;
    }

    size_t inc_bytes = offset + increment - goffset;
    if (inc_bytes < SBRK_MIN_MAPPING) {
        inc_bytes = SBRK_MIN_MAPPING;
    }


    struct capref frame;
    err = frame_alloc(&frame, inc_bytes, &inc_bytes);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "frame_alloc failed");
        return (void *)-1;
    }

    err = memobj->f.fill(memobj, goffset, frame, 0);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "memobj->f.fill failed");
        cap_destroy(frame);
        return (void *)-1;
    }

    err = memobj->f.pagefault(memobj, vregion, goffset, 0);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err,
                  "memobj->f.pagefault failed");
        return (void *)-1;
    }

    goffset += inc_bytes;

    orig_offset = offset;
    offset += increment;
#ifdef SBRK_COLLECT_TIMES
    uint64_t end = bf_ticks();
    sbrk_times += end - start;
#endif

    void *ret = base + orig_offset;
    return ret;
}
