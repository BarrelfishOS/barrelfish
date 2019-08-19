/**
 * \file
 * \brief Protect test
 */
/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>

#define EX_STACK_SIZE 16384
static char ex_stack[EX_STACK_SIZE];
static char *ex_stack_end = ex_stack + EX_STACK_SIZE;

#define ALL_PRIVILEGES (VREGION_FLAGS_READ | VREGION_FLAGS_WRITE | VREGION_FLAGS_EXECUTE)
#define NO_PRIVILEGES 0x0

#define PAGES 100

struct bf_mem {
    struct capref  frame;
    void           *vmem;
    struct memobj  *memobj;
    struct vregion *vregion;
};

struct bf_mem BFmem;
size_t pagesize;

static void
bf_alloc_pages(struct bf_mem *bfmem, size_t npages)
{
    errval_t err;
    size_t retfsize;
    const size_t nbytes = npages*BASE_PAGE_SIZE;
    // allocate a frame
    err = frame_alloc(&bfmem->frame, nbytes, &retfsize);
    if (err_is_fail(err)) {
        fprintf(stderr, "frame_alloc: %s\n", err_getstring(err));
        abort();
    }
    assert(retfsize >= nbytes);
    // map frame rw
    err = vspace_map_one_frame_attr(&bfmem->vmem, retfsize, bfmem->frame,
                                    VREGION_FLAGS_READ_WRITE,
                                    &bfmem->memobj,
                                    &bfmem->vregion);
    if (err_is_fail(err)) {
        fprintf(stderr, "vspace_map: %s\n", err_getstring(err));
        abort();
    }
    genvaddr_t mem = (genvaddr_t) bfmem->vmem;
    if (X86_64_PDIR_BASE(mem) != X86_64_PDIR_BASE(mem + retfsize - 1)) {
        debug_printf("WARN: mapping overlaps leaf pt!\n");
    }
}

__attribute__((unused))
static paging_x86_64_flags_t vregion_to_pmap_flag(vregion_flags_t vregion_flags)
{
    paging_x86_64_flags_t pmap_flags =
        PTABLE_USER_SUPERVISOR | PTABLE_EXECUTE_DISABLE;
    if (!(vregion_flags & VREGION_FLAGS_GUARD)) {
        if (vregion_flags & VREGION_FLAGS_WRITE) {
            pmap_flags |= PTABLE_READ_WRITE;
        }
        if (vregion_flags & VREGION_FLAGS_EXECUTE) {
            pmap_flags &= ~PTABLE_EXECUTE_DISABLE;
        }
        if (vregion_flags & VREGION_FLAGS_NOCACHE) {
            pmap_flags |= PTABLE_CACHE_DISABLED;
        }
    }
    return pmap_flags;
}

//#define DIRECT_INVOKE
static void
bf_protect(struct bf_mem *bfm, size_t off, size_t len,
           vs_prot_flags_t flags)
{
    //debug_printf("%s: off:%zd len:%zd flags:%u\n", __FUNCTION__, off, len, flags);
    errval_t err;
#if defined(DIRECT_INVOKE)
    err = invoke_frame_modify_flags(bfm->frame, off / pagesize, len / pagesize,
            vregion_to_pmap_flag(flags));
#else
    err = bfm->memobj->f.protect(bfm->memobj, bfm->vregion, off, len, flags);
#endif
    if (err_is_fail(err)) {
        fprintf(stderr, "vmpup: memobj.f.protect: %s\n", err_getstring(err));
        abort();
    }
}


static void
bf_handler(enum exception_type type, int subtype,
           void *vaddr,
           arch_registers_state_t *regs)
{
    assert(type == EXCEPT_PAGEFAULT);
    assert(subtype == PAGEFLT_WRITE);
    debug_printf("got exception %d(%d) on %p\n", type, subtype, vaddr);

    assert((uintptr_t)BFmem.vmem <= (uintptr_t)vaddr);
    uintptr_t off_unprotect = (uintptr_t)vaddr - (uintptr_t)BFmem.vmem;
    bf_protect(&BFmem, off_unprotect, pagesize, ALL_PRIVILEGES);
}


int main(int argc, char **argv)
{
    char *mem;
    pagesize = BASE_PAGE_SIZE;

    bf_alloc_pages(&BFmem, PAGES);
    mem = BFmem.vmem;
    thread_set_exception_handler(bf_handler, NULL, ex_stack, ex_stack_end, NULL, NULL);
    debug_printf("MEM=%p\n", mem);

    bf_protect(&BFmem, 0, PAGES*pagesize, NO_PRIVILEGES);

    for (size_t i = 0; i < PAGES; i++) {
        mem[i * pagesize] = 20;
    }
}