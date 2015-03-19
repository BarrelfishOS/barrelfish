/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>

extern errval_t vspace_add_vregion(struct vspace *vspace, struct vregion *region);
int main(int argc, char *argv[])
{
    debug_printf("Hello world (debug_printf)\n");
    printf("Hello world (normal printf)\n");
    for (int i = 0;i < argc; i ++) {
        printf("arg[%d] = %s\n", i, argv[i]);
    }

    // Check that we're in privileged mode
    uint16_t cs;
    __asm volatile("mov %%cs, %[reg]"
            : [reg] "=r" (cs));

    if((cs & 3) == 0) {
        printf("We're in privileged mode!\n");

        printf("Trying privileged operation...\n");
        uintptr_t cr0;
        __asm volatile("mov %%cr0, %[reg]"
                : [reg] "=r" (cr0));

        printf("Succeeded! CR0 is %" PRIxPTR "\n", cr0);
    } else {
        printf("NO privileged mode enabled\n");
        return EXIT_FAILURE;
    }

    printf("Installing our own page tables\n");
    struct pmap *p = get_current_pmap();
    struct memobj m;
    // full pml4 entry
    m.size = 512ULL * 1024 * 1024 * 1024;
    genvaddr_t base;
    errval_t err = p->f.determine_addr(p, &m, m.size, &base);
    assert(err_is_ok(err));
    printf("base: %"PRIxGENVADDR"\n", base);

    struct vregion ours;
    ours.base = base;
    ours.size = m.size;
    vspace_add_vregion(get_current_vspace(), &ours);

    struct capref pdpt_ram, pdpt_cap;
    err = ram_alloc(&pdpt_ram, BASE_PAGE_BITS);
    assert(err_is_ok(err));
    err = slot_alloc(&pdpt_cap);
    assert(err_is_ok(err));
    err = cap_retype(pdpt_cap, pdpt_ram, ObjType_VNode_x86_64_pdpt, BASE_PAGE_BITS);
    assert(err_is_ok(err));
    struct capref pml4 = (struct capref) {
        .cnode = cnode_page,
        .slot = 0
    };
    size_t pml4e = X86_64_PML4_BASE(base);
    printf("our pml4e is: %zu\n", pml4e);
    err = vnode_map(pml4, pdpt_cap, pml4e, PTABLE_ACCESS_DEFAULT, 0, 1);
    assert(err_is_ok(err));

    debug_dump_hw_ptables();
    printf("done!\n\n");

    // make stuff not crash
    while(true);
    return EXIT_SUCCESS;
}
