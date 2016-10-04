/**
 * \file
 * \brief Modify flags (protect) new kernel memory test
 */

/*
 * Copyright (c) 2013, 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h> // sys_debug_flush_cache()
#include <barrelfish/threads.h>
#include <barrelfish/except.h>
#include <stdio.h>
#include "debug.h"
#include "tests.h"

static void *vbase = NULL, *vend = NULL;
static struct memobj *memobj = NULL;
static struct vregion *vregion = NULL;

#define EX_STACK_SIZE 16384
static char ex_stack[EX_STACK_SIZE];

static void handler(enum exception_type type, int subtype, void *addr,
        arch_registers_state_t *regs, arch_registers_fpu_state_t *fpuregs)
{
    static int count = 0;
    ++count;
    DEBUG_MODIFY_FLAGS("got exception %d(%d) on %p [%d]\n", type, subtype, addr, count);
    errval_t err;
    assert(type == EXCEPT_PAGEFAULT);
#ifdef __x86__
    assert(subtype == PAGEFLT_WRITE);
#endif
    assert(addr >= vbase && addr < vend);
    DEBUG_MODIFY_FLAGS("got expected write pagefault on %p\n", addr);
    // unprotect 4k page
    genvaddr_t offset = (genvaddr_t)(lvaddr_t)addr - (genvaddr_t)(lvaddr_t)vbase;
    err = memobj->f.protect(memobj, vregion, offset,
            BASE_PAGE_SIZE, VREGION_FLAGS_READ_WRITE);
    assert(err_is_ok(err));
}

int modify_flags(void)
{
    struct capref frame;
    errval_t err;
    size_t retsize;
    err = frame_alloc(&frame, 16 * BASE_PAGE_SIZE, &retsize);
    assert(err_is_ok(err));
    // map read-write
    err = vspace_map_anon_attr(&vbase, &memobj, &vregion, retsize, &retsize,
            VREGION_FLAGS_READ_WRITE);
    assert(err_is_ok(err));
    err = memobj->f.fill(memobj, 0, frame, retsize);
    assert(err_is_ok(err));
    err = memobj->f.pagefault(memobj, vregion, 0, 0);
    assert(err_is_ok(err));
    assert(vbase);
    vend = (unsigned char *)vbase + retsize;
    unsigned char *base = vbase;
    DEBUG_MODIFY_FLAGS("filling region %p\n", base);
    for (int i = 0; i < retsize; i++) {
        base[i] = i % 256;
    }
    sys_debug_flush_cache();
    DEBUG_MODIFY_FLAGS("checking region %p\n", base);
    // check
    for (int i = 0; i < retsize; i++) {
        if (base[i] != i % 256) {
            debug_printf("failed at %d\n", i);
        }
        assert(base[i] == i % 256);
    }
    sys_debug_flush_cache();
    // change region to read only
    DEBUG_MODIFY_FLAGS("changing region %p perms to readonly\n", base);
    err = memobj->f.protect(memobj, vregion, 0, retsize, VREGION_FLAGS_READ);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "protect");
        return 1;
    }
    // check (reads should not fault)
    DEBUG_MODIFY_FLAGS("checking region %p\n", base);
    printf("%d\n", base[0]);
    for (int i = 0; i < retsize; i++) {
        assert(base[i] == i % 256);
    }

    // register exception handler for writes
    err = thread_set_exception_handler(handler, NULL, ex_stack,
            ex_stack+EX_STACK_SIZE, NULL, NULL);
    assert(err_is_ok(err));

    // this should fault
    for (int i = 0; i < retsize / BASE_PAGE_SIZE; i++) {
        DEBUG_MODIFY_FLAGS("provoke write pagefault on %p\n", base+i*BASE_PAGE_SIZE);
        base[i * BASE_PAGE_SIZE] = 0x42;
    }

    printf("%s: done\n", __FUNCTION__);
    return 0;
}

#ifdef STANDALONE
int main(void)
{
    return modify_flags();
}
#endif
