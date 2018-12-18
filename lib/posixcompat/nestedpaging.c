/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/ram_alloc.h>
#include "nestedpaging.h"

// allocate real vnode
static errval_t allocate_vnode(enum objtype type, struct capref *vnode, void **mapped)
{
    errval_t err;
    struct capref vnode_ram, vnode_cap;
    err = ram_alloc(&vnode_ram, BASE_PAGE_BITS);
    if (err_is_fail(err)) {
        return err;
    }
    err = slot_alloc(&vnode_cap);
    if (err_is_fail(err)) {
        return err;
    }
    assert(type == ObjType_VNode_x86_64_pdpt ||
           type == ObjType_VNode_x86_64_pdir ||
           type == ObjType_VNode_x86_64_ptable);
    err = cap_retype(vnode_cap, vnode_ram, 0, type, 0, 1);
    if (err_is_fail(err)) {
        return err;
    }
    err = cap_destroy(vnode_ram);
    if (err_is_fail(err)) {
        return err;
    }
    *vnode = vnode_cap;

    struct capref vnode_map;
    err = slot_alloc(&vnode_map);
    if (err_is_fail(err)) {
        return err;
    }
    err = cap_copy(vnode_map, vnode_cap);
    if (err_is_fail(err)) {
        return err;
    }

    assert(mapped);
    err = vspace_map_one_frame_attr(mapped, BASE_PAGE_SIZE, vnode_map,
            VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

// allocate 8kB
// first 4kB: HW table
// second 4kB: SW table
static errval_t allocate_user_vnode(struct capref *vnode, void **mapped)
{
    errval_t err;
    struct capref vnode_cap;
    err = frame_alloc(&vnode_cap, 2*BASE_PAGE_BITS);
    if (err_is_fail(err)) {
        return err;
    }
    *vnode = vnode_cap;
    assert(mapped);
    err = vspace_map_one_frame_attr(mapped, BASE_PAGE_SIZE, vnode_cap,
            VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}


extern errval_t vspace_add_vregion(struct vspace *vspace, struct vregion *region);
errval_t install_user_managed_pdpt(struct pml4_entry *retentry)
{
    errval_t err;
    assert(retentry);
    printf("Installing our own page tables\n");
    struct pmap *p = get_current_pmap();
    struct memobj m;
    // full pml4 entry
    m.size = 512ULL * 1024 * 1024 * 1024;
    genvaddr_t base;
    err = p->f.determine_addr(p, &m, m.size, &base);
    if (err_is_fail(err)) {
        return err;
    }
    printf("base: %"PRIxGENVADDR"\n", base);

    struct vregion ours;
    ours.base = base;
    ours.size = m.size;
    vspace_add_vregion(get_current_vspace(), &ours);

    struct capref pdpt_cap;
    void *ptable;
    err = allocate_vnode(ObjType_VNode_x86_64_pdpt, &pdpt_cap, &ptable);
    if (err_is_fail(err)) {
        return err;
    }
    printf("pdpt mapped at %p\n", ptable);

    struct capref pml4 = (struct capref) {
        .cnode = cnode_page,
        .slot = 0
    };
    size_t pml4e = X86_64_PML4_BASE(base);
    printf("our pml4e is: %zu\n", pml4e);
    err = vnode_map(pml4, pdpt_cap, pml4e, PTABLE_ACCESS_DEFAULT, 0, 1);
    if (err_is_fail(err)) {
        return err;
    }

    // fill out retentry struct
    retentry->addr    = ptable;
    retentry->cap     = pdpt_cap;
    retentry->entry   = pml4e;

    struct capref swt;
    size_t retsize;
    err = frame_alloc(&swt, BASE_PAGE_SIZE, &retsize);
    if (err_is_fail(err)) {
        return err;
    }
    err = vspace_map_one_frame((void**)&retentry->swtable, BASE_PAGE_SIZE,
            swt, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

errval_t user_managed_map_frame(struct pml4_entry *pdpt,
                                genvaddr_t vaddr,
                                struct capref frame,
                                vregion_flags_t flags)
{
    assert(pdpt);
    errval_t err;

    size_t pdpte = X86_64_PDPT_BASE(vaddr);
    size_t pdire = X86_64_PDIR_BASE(vaddr);
    size_t pte   = X86_64_PTABLE_BASE(vaddr);

    printf("mapping at pdpte %zu, pdire %zu, pte %zu\n",
            pdpte, pdire, pte);

    struct frame_identity fi;
    err = frame_identify(frame, &fi);

    size_t fsize   = (1ULL << fi.bits);
    size_t npages  = fsize / BASE_PAGE_SIZE;
    size_t npdires = fsize / LARGE_PAGE_SIZE;
    size_t npdptes = fsize / HUGE_PAGE_SIZE;
    if (npdires == 0) { npdires = 1; }
    if (npdptes == 0) { npdptes = 1; }
    printf("mapping %zu pages, %zu pdires, %zu pdptes\n",
            npages, npdires, npdptes);

    paging_x86_64_flags_t pmap_flags = vregion_to_pmap(flags);

    assert(npdptes == 1);
    assert(npdires <= 512);

    struct frame_identity ti;

    union x86_64_pdir_entry *pdir = NULL;
    if (!pdpt->swtable[pdpte]) {
        // need to get pdir
        struct capref pdir_cap;
        void *vpdir;
        err = allocate_user_vnode(&pdir_cap, &vpdir);
        if (err_is_fail(err)) {
            return err;
        }
        err = frame_identify(pdir_cap, &ti);
        assert(err_is_ok(err));
        union x86_64_pdir_entry *pdpt_ptr = pdpt->addr;
        // map new pdir in hw pdpt
        paging_x86_64_map_table(&pdpt_ptr[pdpte], ti.base);
        // map new pdir in sw pdpt
        pdpt->swtable[pdpte] = vpdir;
    } else {
        assert(pdpt->swtable[pdpte]);
        pdir = pdpt->swtable[pdpte];
    }
    assert(pdir);
    union x86_64_pdir_entry *swpdir = pdir + PTABLE_SIZE;

    genpaddr_t offset = 0;
    for (int i = pdire; i < pdire + npdires; i++) {
        void *pt = NULL;
        if (i == 0 || i == pdire + npdires - 1) {
            // first/last pt
            if (!swpdir[i]) {
                // get ptable
                struct capref pt_cap;
                void *vpt;
                err = allocate_user_vnode(&pt_cap, &vpt);
                if (err_is_fail(err)) {
                    return err;
                }
                err = frame_identify(pt_cap, &ti);
                assert(err_is_ok(err));
                // map new ptable in hw pdir
                paging_x86_64_map_table(&pdir[i], ti.base);
                // map new ptable in sw pdir
                swpdir[i] = vpt;
            } else {
                assert(swpdir[i]);
                pt = swpdir[i];
            }
            assert(pt);

            size_t first_e  = i ? 0 : pte;
            assert(first_e < PTABLE_SIZE);
            size_t map_here = npages < PTABLE_SIZE ? npages : PTABLE_SIZE;
            for (int j = first_e; j < first_e + map_here; j++) {
                paging_x86_64_map(&pt[j], fi.base + offset, pmap_flags);
                offset += BASE_PAGE_SIZE;
                npages -= 1;
            }
        } else {
            // full leaf: map as 2M page
            assert(swpdir[i] == NULL);
            union x86_64_ptable_entry *pde = (union x86_64_ptable_entry *)swpdir[i];
            paging_x86_64_map_large(pde, fi.base + offset, pmap_flags);
            offset += LARGE_PAGE_SIZE;
            npages -= PTABLE_SIZE;
        }
    }

    return SYS_ERR_OK;
}
