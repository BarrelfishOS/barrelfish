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
#include <bench/bench.h>

#include <stdio.h>
#include <stdlib.h>

//#define DEBUG 1
#ifdef DEBUG
#define DEBUG_COW(x...) debug_printf(x)
#else
#define DEBUG_COW(x...) ((void)0)
#endif


int cow_setup_bench(size_t buffer_size);

#define BUFSIZE (32UL*1024)
#define EX_STACK_SIZE 16384
static char ex_stack[EX_STACK_SIZE];
static struct vregion *cow_vregion;
static void *cow_vbuf;
static struct cnoderef cow_frames;
static size_t cow_frame_count = 0;

static void handler(enum exception_type type, int subtype, void *vaddr,
        arch_registers_state_t *regs, arch_registers_fpu_state_t *fpuregs)
{
    DEBUG_COW("got exception %d(%d) on %p\n", type, subtype, vaddr);
    assert(type == EXCEPT_PAGEFAULT);
    assert(subtype == PAGEFLT_WRITE);
    uintptr_t addr = (uintptr_t) vaddr;
    uintptr_t faddr = addr & ~BASE_PAGE_MASK;
    uintptr_t base = (uintptr_t) cow_vbuf;
    if (addr < base || addr >= base + BUFSIZE) {
        DEBUG_COW("unexpected write pagefault on %p\n", vaddr);
        exit(1);
    }
    assert(cow_frame_count);
    DEBUG_COW("got expected write pagefault on %p, creating copy of page\n", vaddr);
    // get and map copy of page
    size_t frame_id = (addr - base) / BASE_PAGE_SIZE;
    DEBUG_COW("remapping frame %zu\n", frame_id);
    struct memobj *m = vregion_get_memobj(cow_vregion);
    assert(m);
    errval_t err;
    struct capref retframe;
    struct capref f = (struct capref) { .cnode = cow_frames, .slot = frame_id };
    size_t retoff;
    struct vregion *vr;
    void *buf;
    // copy data from faulting page to new page
    err = vspace_map_one_frame(&buf, BASE_PAGE_SIZE, f, NULL, &vr);
    assert(err_is_ok(err));
    memcpy(buf, (void *)faddr, BASE_PAGE_SIZE);
    vregion_destroy(vr);
    err = m->f.unfill(m, frame_id * BASE_PAGE_SIZE, &retframe, &retoff);
    assert(err_is_ok(err));
    err = m->f.fill(m, frame_id * BASE_PAGE_SIZE, f, BASE_PAGE_SIZE);
    assert(err_is_ok(err));
    err = m->f.pagefault(m, cow_vregion, frame_id * BASE_PAGE_SIZE, 0);
    assert(err_is_ok(err));
    err = m->f.protect(m, cow_vregion, frame_id * BASE_PAGE_SIZE,
            BASE_PAGE_SIZE, VREGION_FLAGS_READ_WRITE);
    assert(err_is_ok(err));
}

static errval_t cow_init(size_t bufsize, size_t granularity,
        struct cnoderef *cow_cn, size_t *frame_count)
{
    assert(cow_cn);
    assert(frame_count);

    errval_t err;
    struct capref frame, cncap;
    struct cnoderef cnode;

    // get RAM cap bufsize = (bufsize / granularity + 1) * granularity;
    err = slot_alloc(&frame);
    assert(err_is_ok(err));
    size_t rambits = log2floor(bufsize);
    DEBUG_COW("bits = %zu\n", rambits);
    err = ram_alloc(&frame, rambits);
    assert(err_is_ok(err));
    DEBUG_COW("ram alloc done\n");
    // calculate #slots
    cslot_t cap_count = bufsize / granularity;
    cslot_t slots;
    // get CNode
    err = cnode_create(&cncap, &cnode, cap_count, &slots);
    assert(err_is_ok(err));
    assert(slots >= cap_count);
    DEBUG_COW("cnode create done\n");

    // retype RAM into Frames
    struct capref first_frame = (struct capref) { .cnode = cnode, .slot = 0 };
    err = cap_retype(first_frame, frame, ObjType_Frame, log2floor(granularity));
    assert(err_is_ok(err));
    DEBUG_COW("retype done\n");

    //err = cap_destroy(frame);
    //assert(err_is_ok(err));
    //DEBUG_COW("destroy done\n");

    *frame_count = slots;
    *cow_cn = cnode;
    return SYS_ERR_OK;
}

// create cow-enabled vregion & backing
// Can copy-on-write in granularity-sized chunks
static errval_t vspace_map_one_frame_cow(void **buf, size_t size,
        struct capref frame, vregion_flags_t flags,
        struct memobj **memobj, struct vregion **vregion,
        size_t granularity)
{
    errval_t err;
    if (!memobj) {
        memobj = malloc(sizeof(*memobj));
    }
    assert(memobj);
    if (!vregion) {
        vregion = malloc(sizeof(*vregion));
    }
    assert(vregion);
    err = vspace_map_anon_attr(buf, memobj, vregion, size, &size, flags);
    assert(err_is_ok(err));

    size_t chunks = size / granularity;
    cslot_t slots;
    struct capref cncap;
    struct cnoderef cnode;
    err = cnode_create(&cncap, &cnode, chunks, &slots);
    assert(err_is_ok(err));
    assert(slots >= chunks);

    cycles_t fstart = 0;
    cycles_t pstart = 0;

    struct capref fc = (struct capref) { .cnode = cnode, .slot = 0 };
    for (int i = 0; i < chunks; i++) {
        err = cap_copy(fc, frame);
        assert(err_is_ok(err));

        cycles_t start = bench_tsc();
        err = (*memobj)->f.fill_foff(*memobj, i * granularity, fc, granularity, i*granularity);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "call failed.");
        }
        cycles_t end = bench_tsc();
        fstart += end-start;

        start = bench_tsc();
        err = (*memobj)->f.pagefault(*memobj, *vregion, i * granularity, 0);
        assert(err_is_ok(err));
        fc.slot++;
        end = bench_tsc();
        pstart += end-start;
    }

    printf("fill_foff: %"PRIu64"\n", bench_tsc_to_ms(fstart));
    printf("pagefault: %"PRIu64"\n", bench_tsc_to_ms(pstart));

    return SYS_ERR_OK;
}

int cow_setup_bench(size_t buffer_size) {
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
    thread_set_exception_handler(handler, NULL, ex_stack,
            ex_stack+EX_STACK_SIZE, NULL, NULL);
    assert(err_is_ok(err));


    //DEBUG_COW("%s:%d\n", __FUNCTION__, __LINE__);
    cycles_t start = bench_tsc();

    err = cow_init(buffer_size, BASE_PAGE_SIZE, &cow_frames, &cow_frame_count);
    assert(err_is_ok(err));
    //  create r/o copy of region and tell exception handler bounds

    cycles_t map = bench_tsc();
    DEBUG_COW("%s:%d\n", __FUNCTION__, __LINE__);
    err = vspace_map_one_frame_cow(&cow_vbuf, retsize, frame,
            VREGION_FLAGS_READ, NULL, &cow_vregion, BASE_PAGE_SIZE);
    if (err_is_fail(err)) {
        DEBUG_COW("vspace_map: %s\n", err_getstring(err));
        return 1;
    }

    cycles_t end = bench_tsc();
    DEBUG_COW("cow_vbuf = %p\n", cow_vbuf);
    printf("[xx] %"PRIu64", %"PRIuCYCLES", %"PRIuCYCLES"\n",
           buffer_size,
           bench_tsc_to_ms(bench_time_diff(start, map)),
           bench_tsc_to_ms(bench_time_diff(map, end)));
    return 0;
}

int main(int argc, char *argv[])
{
    bench_init();
    DEBUG_COW("%s:%d\n", __FUNCTION__, __LINE__);

    printf("[xx] buffer, ms map, ms map\n");
    for (size_t buffer_bits = 27; buffer_bits < 36; buffer_bits++) {
        uint64_t buffer_size = 1 << buffer_bits; // 32*1024ULL;
        int r = cow_setup_bench(buffer_size);
        assert (r == 0);
    }

    return EXIT_SUCCESS;
}
