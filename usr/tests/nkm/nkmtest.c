/**
 * \file
 * \brief Map/unmap new kernel memory test
 */

/*
 * Copyright (c) 2012, 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/cap_predicates.h>
#include <stdio.h>
#include "debug.h"
#include "tests.h"
#include "vspace_dump.h"

int map_unmap(void)
{
    errval_t err;
    struct capref mem;

    DEBUG_MAP_UNMAP("ram_alloc\n");
    err = ram_alloc(&mem, BASE_PAGE_BITS);
    if (err_is_fail(err)) {
        printf("ram_alloc: %s (%"PRIuERRV")\n", err_getstring(err), err);
        return 1;
    }

    struct capref frame;
    DEBUG_MAP_UNMAP("retype\n");
    err = slot_alloc(&frame);
    if (err_is_fail(err)) {
        printf("slot_alloc: %s (%"PRIuERRV")\n", err_getstring(err), err);
        return 1;
    }
    err = cap_retype(frame, mem, 0, ObjType_Frame, BASE_PAGE_SIZE, 1);
    if (err_is_fail(err)) {
        printf("cap_retype: %s (%"PRIuERRV")\n", err_getstring(err), err);
        return 1;
    }

    DEBUG_MAP_UNMAP("delete ram cap\n");
    err = cap_destroy(mem);
    if (err_is_fail(err)) {
        printf("cap_delete(mem): %s (%"PRIuERRV")\n", err_getstring(err), err);
        return 1;
    }

    struct frame_identity fi;
    err = invoke_frame_identify(frame, &fi);
    if (err_is_fail(err)) {
        printf("frame_identify: %s (%"PRIuERRV")\n", err_getstring(err), err);
        return 1;
    }
    DEBUG_MAP_UNMAP("frame: base = 0x%"PRIxGENPADDR", bits = %d\n", fi.base, fi.bits);

#ifdef NKMTEST_DEBUG_MAP_UNMAP
    dump_pmap(get_current_pmap());
#endif

    struct vregion *vr;
    struct memobj *memobj;
    void *vaddr;
    DEBUG_MAP_UNMAP("map\n");
    err = vspace_map_one_frame(&vaddr, BASE_PAGE_SIZE, frame, &memobj, &vr);
    if (err_is_fail(err)) {
        printf("vspace_map_one_frame: %s (%"PRIuERRV")\n", err_getstring(err), err);
    }
    char *memory = vaddr;
    DEBUG_MAP_UNMAP("vaddr = %p\n", vaddr);

#ifdef NKMTEST_DEBUG_MAP_UNMAP
    dump_pmap(get_current_pmap());
#endif

    DEBUG_MAP_UNMAP("write 1\n");
    int i;
    for (i = 0; i < BASE_PAGE_SIZE; i++) {
        memory[i] = i % INT8_MAX;
    }
    DEBUG_MAP_UNMAP("verify 1\n");
    for (i = 0; i < BASE_PAGE_SIZE; i++) {
        assert(memory[i] == i % INT8_MAX);
    }

    DEBUG_MAP_UNMAP("delete frame cap\n");
    err = cap_destroy(frame);
    if (err_is_fail(err)) {
        printf("cap_delete(frame): %s (%"PRIuERRV")\n", err_getstring(err), err);
        return 1;
    }

#ifdef NKMTEST_DEBUG_MAP_UNMAP
    // no mapping should remain here
    dump_pmap(get_current_pmap());
    err = debug_dump_hw_ptables();
    if (err_is_fail(err)) {
        printf("kernel dump ptables: %s (%"PRIuERRV")\n", err_getstring(err), err);
        return 1;
    }
#endif

    printf("%s: done\n", __FUNCTION__);
    return 0;
}

#ifdef STANDALONE
int main(void) {
    return map_unmap();
}
#endif
