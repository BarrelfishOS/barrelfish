/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include "nestedpaging.h"

extern errval_t vspace_add_vregion(struct vspace *vspace, struct vregion *region);
errval_t install_user_managed_pdpt(genvaddr_t *base, void **ptable)
{
    errval_t err;
    assert(base);
    assert(ptable);
    printf("Installing our own page tables\n");
    struct pmap *p = get_current_pmap();
    struct memobj m;
    // full pml4 entry
    m.size = 512ULL * 1024 * 1024 * 1024;
    genvaddr_t base_;
    err = p->f.determine_addr(p, &m, m.size, &base_);
    if (err_is_fail(err)) {
        return err;
    }
    printf("base: %"PRIxGENVADDR"\n", base_);

    struct vregion ours;
    ours.base = base_;
    ours.size = m.size;
    vspace_add_vregion(get_current_vspace(), &ours);

    *base = base_;

    struct capref pdpt_ram, pdpt_cap;
    err = ram_alloc(&pdpt_ram, BASE_PAGE_BITS);
    if (err_is_fail(err)) {
        return err;
    }
    err = slot_alloc(&pdpt_cap);
    if (err_is_fail(err)) {
        return err;
    }
    err = cap_retype(pdpt_cap, pdpt_ram, 0, ObjType_VNode_x86_64_pdpt, BASE_PAGE_SIZE, 1);
    if (err_is_fail(err)) {
        return err;
    }
    err = cap_destroy(pdpt_ram);
    if (err_is_fail(err)) {
        return err;
    }
    struct capref pml4 = (struct capref) {
        .cnode = cnode_page,
        .slot = 0
    };
    size_t pml4e = X86_64_PML4_BASE(base_);
    printf("our pml4e is: %zu\n", pml4e);
    struct capref mapping;
    err = slot_alloc(&mapping);
    assert(err_is_ok(err));

    err = vnode_map(pml4, pdpt_cap, pml4e, PTABLE_ACCESS_DEFAULT, 0, 1, mapping);
    if (err_is_fail(err)) {
        return err;
    }
    struct capref pdpt_map;
    err = slot_alloc(&pdpt_map);
    if (err_is_fail(err)) {
        return err;
    }
    err = cap_copy(pdpt_map, pdpt_cap);
    if (err_is_fail(err)) {
        return err;
    }

    void *ptable_ = NULL;
    err = vspace_map_one_frame_attr(&ptable_, BASE_PAGE_SIZE, pdpt_map,
            VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }
    printf("pdpt mapped at %p\n", ptable_);

    *ptable = ptable_;

    return SYS_ERR_OK;
}
