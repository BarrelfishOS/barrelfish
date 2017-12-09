/**
 * \file
 * \brief test that vnode_inherit cannot create unrevokable ptes
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>
#include <barrelfish/sys_debug.h>

// Defined in lib/barrelfish/target/x86_64/pmap_target.c
errval_t get_ptable(struct pmap_x86 *pmap, genvaddr_t base,
                    struct vnode **ptable);

#ifdef DELETE_FRAME
#define EX_STACK_SIZE 16384
static char ex_stack[EX_STACK_SIZE];
static char *ex_stack_top = ex_stack + EX_STACK_SIZE;

static void exhandler(enum exception_type type, int subtype, void *vaddr,
        arch_registers_state_t *regs, arch_registers_fpu_state_t *fpuregs)
{
    debug_printf("Got fault on %p\n", vaddr);
    debug_printf("Test completed: SUCCESS.\n");
    exit(0);
}
#endif

int main(int argc, char *argv[])
{
    errval_t err;

    struct pmap_x86 *x86 = (struct pmap_x86*)get_current_pmap();

    struct memobj *memobj = NULL;
    struct vregion *vregion = NULL;

    /* get frame cap */
    struct capref frame;
    size_t rb;
    err = frame_alloc(&frame, 4096, &rb);
    assert(err_is_ok(err));
    assert(rb == 4096);

    void *vbase;
    err = vspace_map_one_frame(&vbase, BASE_PAGE_SIZE, frame, &memobj, &vregion);
    assert(err_is_ok(err));

    genvaddr_t base = vregion_get_base_addr(vregion);
    debug_printf("our region is at %#"PRIxGENVADDR"\n", base);

    /* get struct vnode for ptable for our region */
    struct vnode *ptable = NULL;
    err = get_ptable(x86, base, &ptable);
    assert(err_is_ok(err));
    assert(ptable);

    /* allocate a empty ptable */
    genvaddr_t cloned_base;
    err = x86->p.f.determine_addr_raw(&x86->p, LARGE_PAGE_SIZE,
                                      LARGE_PAGE_SIZE, &cloned_base);
    assert(err_is_ok(err));
    debug_printf("cloned ptable is at %#"PRIxGENVADDR"\n", cloned_base);

    struct vnode *cloned = NULL;
    err = get_ptable(x86, cloned_base, &cloned);
    assert(err_is_ok(err));
    assert(cloned);

    /* clone ptable */
    debug_printf("calling vnode inherit\n");
    err = vnode_inherit(cloned->u.vnode.invokable, ptable->u.vnode.invokable,
                        0, PTABLE_SIZE, ptable->u.vnode.mcn, cloned->u.vnode.mcn);
    assert(err_is_ok(err));

    size_t ptentry = X86_64_PTABLE_BASE(base);
    genvaddr_t cloned_addr = cloned_base | (ptentry << BASE_PAGE_BITS);
    debug_printf("cloned region is at %#"PRIxGENVADDR"\n", cloned_addr);

    debug_printf("Writing to original region\n");
    uint8_t *b = vbase;
    for (int i = 0; i < BASE_PAGE_SIZE; i++) {
        b[i] = i % UINT8_MAX;
    }
    sys_debug_flush_cache();

    char *exitmsg = NULL;
    int retval = 0;

#ifdef DELETE_FRAME
    debug_printf("Deleting Frame cap, check should fault\n");
    exitmsg = "FAILURE";
    retval = 1;

    /* install exception handler, to gracefully handle expected faults */
    err = thread_set_exception_handler(exhandler, NULL, ex_stack, ex_stack_top,
                                       NULL, NULL);
    assert(err_is_ok(err));

    /* delete frame cap, should unmap region in both page tables */
    err = cap_destroy(frame);
    assert(err_is_ok(err));
#else
    exitmsg = "SUCCESS";
#endif

    debug_printf("Checking cloned region\n");
    b = (void *)cloned_addr;
    for (int i = 0; i < BASE_PAGE_SIZE; i++) {
        if (b[i] != (i % UINT8_MAX)) {
            debug_printf("Error at byte %d: %hhu, %u\n", i, b[i], i % UINT8_MAX);
        }
    }
    debug_printf("Cloned region checked successfully!\n");

#if !defined(DELETE_FRAME)

    sys_debug_flush_cache();
    debug_printf("Checking original region\n");
    b = vbase;
    for (int i = 0; i < BASE_PAGE_SIZE; i++) {
        if (b[i] != (i % UINT8_MAX)) {
            debug_printf("Error at byte %d: %hhu, %u\n", i, b[i], i % UINT8_MAX);
        }
    }
    debug_printf("Original region checked successfully\n");
#endif

    debug_printf("Test completed: %s.\n", exitmsg);

    return retval;
}
