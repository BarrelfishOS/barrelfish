/**
 * \file
 * \brief test that we cannot create mappings outside of provided frame
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#define FLAGS (VREGION_FLAGS_READ | VREGION_FLAGS_WRITE)

int main(int argc, char *argv[])
{
    errval_t err;

    struct capref vnode;
    err = slot_alloc(&vnode);
    assert(err_is_ok(err));
#ifdef __x86_64__
    enum objtype vntype = ObjType_VNode_x86_64_ptable;
#elif defined(__arm__)
    enum objtype vntype = ObjType_VNode_ARM_l2;
#elif defined(__aarch64__)
    enum objtype vntype = ObjType_VNode_AARCH64_l3;
#else
#error Need to define vntype for this arch
#endif
    err = vnode_create(vnode, vntype);
    assert(err_is_ok(err));

    struct capref mapping;
    err = slot_alloc(&mapping);
    assert(err_is_ok(err));

    struct capref frame;
    size_t rb;
    err = frame_alloc(&frame, 4096, &rb);
    assert(err_is_ok(err));
    assert(rb == 4096);

    struct frame_identity id;
    err = frame_identify(frame, &id);
    assert(err_is_ok(err));
    printf("base = %" PRIxGENPADDR ", bytes = %" PRIuGENSIZE "\n", id.base, id.bytes);
    assert(id.bytes == 4096);

    err = slot_alloc(&mapping);
    assert(err_is_ok(err));
    // try to map 128 pages with 1page frame
    err = vnode_map(vnode, frame, 0, FLAGS, 0, 128, mapping);
    if (err_no(err) != SYS_ERR_FRAME_OFFSET_INVALID) {
        printf("%s: FAILURE: got %s from vnode_map(128 pages); expected SYS_ERR_FRAME_OFFSET_INVALID\n",
                argv[0], err_getcode(err));
        exit(1);
    }
    assert(err_no(err) == SYS_ERR_FRAME_OFFSET_INVALID);
    err = vnode_map(vnode, frame, 0, FLAGS, 0, 1, mapping);
    assert(err_is_ok(err));

    err = frame_alloc(&frame, 4096, &rb);
    assert(err_is_ok(err));
    assert(rb == 4096);

    err = frame_identify(frame, &id);
    assert(err_is_ok(err));
    assert(id.bytes == 4096);
    printf("base = %" PRIxGENPADDR ", bytes = %" PRIuGENSIZE "\n", id.base, id.bytes);

    err = slot_alloc(&mapping);
    assert(err_is_ok(err));
    err = vnode_map(vnode, frame, 1, FLAGS, 0, 1, mapping);
    if(err_is_fail(err)) {
      DEBUG_ERR(err, "vnode_map");
    }
    assert(err_is_ok(err));

    printf("%s: SUCCESS\n", argv[0]);

    exit(0);
}
