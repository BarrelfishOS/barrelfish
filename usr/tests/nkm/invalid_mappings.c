/**
 * \file
 * \brief Test invalid vnode invocations.
 */

/*
 * Copyright (c) 2014, HP Labs.
 * Copyright (c) 2015, ETH Zurich.
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

#define FRAME_ACCESS_DEFAULT \
        (PTABLE_USER_SUPERVISOR | PTABLE_EXECUTE_DISABLE | PTABLE_READ_WRITE)

static enum objtype types[7] =
{
    ObjType_VNode_x86_64_pml4,
    ObjType_VNode_x86_64_pdpt,
    ObjType_VNode_x86_64_pdir,
    ObjType_VNode_x86_64_ptable,
    ObjType_Frame,
    ObjType_Frame,
    ObjType_Frame
};

#define W SYS_ERR_WRONG_MAPPING
#define O SYS_ERR_OK
#define S SYS_ERR_VM_FRAME_TOO_SMALL

static errval_t mapping_ok[4][7] =
{         /*pml4*/ /*pdpt*/ /*pdir*/ /*pt*/ /*frm*/ /*2mfrm*/ /*1gfrm*/
/*pml4*/{   W,       O,       W,       W,     W,      W,        W  },
/*pdpt*/{   W,       W,       O,       W,     S,      S,        O  },
/*pdir*/{   W,       W,       W,       O,     S,      O,        O  },
/*pt  */{   W,       W,       W,       W,     O,      O,        O  },
};

static int pass = 0, fail = 0;
static void check_result(errval_t err, int dest, int src)
{
    if (err_no(err) == mapping_ok[dest][src]) {
        printf("%d<-%d PASSED (%s)\n", dest, src, err_getstring(err));
        pass++;
    } else {
        printf("%d<-%d FAILED (expected: %s, was %s)\n",
                dest, src, err_getstring(mapping_ok[dest][src]),
                err_getstring(err));
        fail++;
    }
}

int invalid_mappings(void)
{
    // outline:
    // get pml4, pdpt, pdir, ptable, and frame
    // check all combinations to make sure that restrictions are implemented
    // correctly in kernel space
    // VALID:
    // map pdpt in pml4
    // map pdir in pdpt
    // map pt   in pdir
    // map frame in {pt, pdir, pdpt}
    // INVALID:
    // all other combinations

    errval_t err;
    struct capref caps[7];
    struct capref mapping;

    // allocate slot for mapping cap: can reuse
    err = slot_alloc(&mapping);
    if (err_is_fail(err)) {
        debug_printf("slot_alloc: %s (%ld)\n", err_getstring(err), err);
        return 1;
    }

    // allocate caps
    for (int i = 0; i < 5; i++) {
        // get 4k block
        struct capref mem;
        err = ram_alloc(&mem, BASE_PAGE_BITS);
        if (err_is_fail(err)) {
            debug_printf("ram_alloc: %s (%ld)\n", err_getstring(err), err);
            return 1;
        }

        // get slot for retype dest
        err = slot_alloc(&caps[i]);
        if (err_is_fail(err)) {
            debug_printf("slot_alloc: %s (%ld)\n", err_getstring(err), err);
            return 1;
        }
        // retype to selected type
        err = cap_retype(caps[i], mem, 0, types[i], BASE_PAGE_SIZE, 1);
        if (err_is_fail(err)) {
            debug_printf("cap_retype: %s (%ld)\n", err_getstring(err), err);
            return 1;
        }

        // cleanup source cap
        DEBUG_INVALID_MAPPINGS("delete ram cap\n");
        err = cap_destroy(mem);
        if (err_is_fail(err)) {
            debug_printf("cap_delete(mem): %s (%ld)\n", err_getstring(err), err);
            return 1;
        }
    }

    // Do gigabyte frame tests
    int last_source = 7;

    // cap 6: 2M frame
    size_t rb = 0;
    err = frame_alloc(&caps[5], X86_64_LARGE_PAGE_SIZE, &rb);
    if (err_is_fail(err) || rb != X86_64_LARGE_PAGE_SIZE) {
        debug_printf("frame_alloc: %s (%ld)\n", err_getstring(err), err);
        return 1;
    }
    // cap 7: 1G frame
    err = frame_alloc(&caps[6], X86_64_HUGE_PAGE_SIZE, &rb);
    if (err_is_fail(err) || rb != X86_64_HUGE_PAGE_SIZE) {
        debug_printf("Cannot allocate 1GB frame (%s)\n", err_getcode(err));
        last_source = 6;
    }

    paging_x86_64_flags_t attr = 0;
    // select dest (ignore frame, asserts)
    for (int i = 0; i < 4; i++) {
        // select source
        for (int j = 0; j < last_source; j++) {
            if (j >= 4) {
                // frame
                attr = FRAME_ACCESS_DEFAULT;
            } else {
                // ptable
                attr = PTABLE_ACCESS_DEFAULT;
            }
            // try mapping
            err = vnode_map(caps[i], caps[j], /*slot*/0, attr, /*off*/0,
                            /*count*/1, mapping);
            check_result(err, i, j);
            // unmap if mapping succeeded
            if (err_is_ok(err)) {
                err = vnode_unmap(caps[i], mapping);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "vnode_unmap");
                }
                assert(err_is_ok(err));
                // XXX: better API?
                err = cap_delete(mapping);
                assert(err_is_ok(err));
            }
        }
    }

    printf("All tests executed: %d PASSED, %d FAILED\n", pass, fail);

    return 0;
}

#ifdef STANDALONE
int main(void)
{
    return invalid_mappings();
}
#endif
