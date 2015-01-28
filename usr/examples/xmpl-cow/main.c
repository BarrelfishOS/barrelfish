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
#include <stdio.h>
#include <stdlib.h>
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
    debug_printf("got exception %d(%d) on %p\n", type, subtype, vaddr);
    assert(type == EXCEPT_PAGEFAULT);
    assert(subtype == PAGEFLT_WRITE);
    uintptr_t addr = (uintptr_t) vaddr;
    uintptr_t faddr = addr & ~BASE_PAGE_MASK;
    uintptr_t base = (uintptr_t) cow_vbuf;
    if (addr < base || addr >= base + BUFSIZE) {
        debug_printf("unexpected write pagefault on %p\n", vaddr);
        exit(1);
    }
    assert(cow_frame_count);
    debug_printf("got expected write pagefault on %p, creating copy of page\n", vaddr);
    // get and map copy of page
    size_t frame_id = (addr - base) / BASE_PAGE_SIZE;
    debug_printf("remapping frame %zu\n", frame_id);
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
    debug_printf("bits = %zu\n", rambits);
    err = ram_alloc(&frame, rambits);
    assert(err_is_ok(err));
    // calculate #slots
    cslot_t cap_count = bufsize / granularity;
    cslot_t slots;
    // get CNode
    err = cnode_create(&cncap, &cnode, cap_count, &slots);
    assert(err_is_ok(err));
    assert(slots >= cap_count);
    // retype RAM into Frames
    struct capref first_frame = (struct capref) { .cnode = cnode, .slot = 0 };
    err = cap_retype(first_frame, frame, ObjType_Frame, log2floor(granularity));
    assert(err_is_ok(err));
    err = cap_destroy(frame);
    assert(err_is_ok(err));
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
    struct capref fc = (struct capref) { .cnode = cnode, .slot = 0 };
    for (int i = 0; i < chunks; i++) {
        err = cap_copy(fc, frame);
        assert(err_is_ok(err));
        err = (*memobj)->f.fill_foff(*memobj, i * granularity, fc, granularity, i*granularity);
        assert(err_is_ok(err));
        err = (*memobj)->f.pagefault(*memobj, *vregion, i * granularity, 0);
        assert(err_is_ok(err));
        fc.slot++;
    }
    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    errval_t err;
    struct capref frame;
    size_t retsize;
    void *vbuf;
    struct vregion *vregion;
    uint8_t *buf;

    debug_printf("%s:%d\n", __FUNCTION__, __LINE__);
    err = frame_alloc(&frame, BUFSIZE, &retsize);
    assert(retsize >= BUFSIZE);
    if (err_is_fail(err)) {
        debug_printf("frame_alloc: %s\n", err_getstring(err));
        return 1;
    }
    debug_printf("%s:%d: %zu\n", __FUNCTION__, __LINE__, retsize);
    // setup region
    err = vspace_map_one_frame_attr(&vbuf, retsize, frame,
            VREGION_FLAGS_READ_WRITE, NULL, &vregion);
    if (err_is_fail(err)) {
        debug_printf("vspace_map: %s\n", err_getstring(err));
        return 1;
    }
    debug_printf("vaddr: %p\n", vbuf);

    // write stuff to region
    buf = vbuf;
    debug_printf("%s:%d: %p, %lu pages\n", __FUNCTION__, __LINE__, buf, BUFSIZE / BASE_PAGE_SIZE);
    memset(buf, 0xAA, BUFSIZE);

    debug_printf("%s:%d\n", __FUNCTION__, __LINE__);
    // create cow copy
    //  setup exception handler
    thread_set_exception_handler(handler, NULL, ex_stack,
            ex_stack+EX_STACK_SIZE, NULL, NULL);
    assert(err_is_ok(err));
    debug_printf("%s:%d\n", __FUNCTION__, __LINE__);
    err = cow_init(BUFSIZE, BASE_PAGE_SIZE, &cow_frames, &cow_frame_count);
    assert(err_is_ok(err));
    //  create r/o copy of region and tell exception handler bounds
    debug_printf("%s:%d\n", __FUNCTION__, __LINE__);
    err = vspace_map_one_frame_cow(&cow_vbuf, retsize, frame,
            VREGION_FLAGS_READ, NULL, &cow_vregion, BASE_PAGE_SIZE);
    if (err_is_fail(err)) {
        debug_printf("vspace_map: %s\n", err_getstring(err));
        return 1;
    }
    debug_printf("cow_vaddr: %p\n", cow_vbuf);

    // do stuff cow copy
    uint8_t *cbuf = cow_vbuf;
    for (int i = 0; i < BUFSIZE / BASE_PAGE_SIZE; i+=2) {
        cbuf[i * BASE_PAGE_SIZE + 1] = 0x55;
    }
    // verify results
    for (int i = 0; i < BUFSIZE / BASE_PAGE_SIZE; i++) {
        printf("page %d\n", i);
        printf("buf[0] = %d; cbuf[0] = %d\n", buf[i*BASE_PAGE_SIZE],
                cbuf[i*BASE_PAGE_SIZE]);
        printf("buf[1] = %d; cbuf[1] = %d\n", buf[i*BASE_PAGE_SIZE+1],
                cbuf[i*BASE_PAGE_SIZE+1]);
    }
    debug_dump_hw_ptables();
    return EXIT_SUCCESS;
}
