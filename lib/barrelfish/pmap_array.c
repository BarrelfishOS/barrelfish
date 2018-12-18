/**
 * \file
 * \brief architecture-independent shadow page table traversal code for
 *        linked-list pmap implementation.
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/pmap_target.h>

#include <pmap_ds.h>
#include <pmap_priv.h>

/**
 * \brief Starting at a given root, return the vnode with entry equal to #entry
 */
struct vnode *pmap_find_vnode(struct vnode *root, uint16_t entry)
{
    assert(root != NULL);
    assert(root->v.is_vnode);
    assert(entry < PTABLE_ENTRIES);

    if (root->v.u.vnode.children) {
        return root->v.u.vnode.children[entry];
    } else {
        return NULL;
    }
}

bool pmap_inside_region(struct vnode *root, uint16_t entry, uint16_t npages)
{
    assert(root != NULL);
    assert(root->v.is_vnode);

    struct vnode *n = root->v.u.vnode.children[entry];

    // empty or ptable
    if (!n || n->v.is_vnode) {
        return false;
    }

    uint16_t end = n->v.entry + n->v.u.frame.pte_count;
    if (n->v.entry <= entry && entry + npages <= end) {
        return true;
    }

    return false;
}

void pmap_remove_vnode(struct vnode *root, struct vnode *item)
{
    assert(root->v.is_vnode);
    size_t pte_count = item->v.is_vnode ? 1 : item->v.u.frame.pte_count;
    // check that we don't overflow children buffer
    assert(item->v.entry + pte_count <= PTABLE_ENTRIES);
    for (int i = 0; i < pte_count; i++) {
        root->v.u.vnode.children[item->v.entry+i] = NULL;
    }
}

errval_t pmap_vnode_mgmt_init(struct pmap *pmap)
{
    struct pmap_vnode_mgmt *m = &pmap->m;
    /* special case init of own pmap vnode mgmt */
    if (get_current_pmap() == pmap) {
        /* use core state buffers */
        slab_init(&m->slab, sizeof(struct vnode), NULL);
        slab_grow(&m->slab, m->slab_buffer, INIT_SLAB_BUFFER_SIZE);

        /* Initialize slab allocator for child arrays */
        slab_init(&m->ptslab, PTSLAB_SLABSIZE, NULL);
        slab_grow(&m->ptslab, m->ptslab_buffer, INIT_PTSLAB_BUFFER_SIZE);
    } else {
        /*initialize slab allocator for vnodes */
        slab_init(&m->slab, sizeof(struct vnode), NULL);
        uint8_t *buf = malloc(INIT_SLAB_BUFFER_SIZE);
        if (!buf) {
            return LIB_ERR_MALLOC_FAIL;
        }
        slab_grow(&m->slab, buf, INIT_SLAB_BUFFER_SIZE);

        /* Initialize slab allocator for child arrays */
        slab_init(&m->ptslab, PTSLAB_SLABSIZE, NULL);
        buf = malloc(INIT_PTSLAB_BUFFER_SIZE);
        if (!buf) {
            return LIB_ERR_MALLOC_FAIL;
        }
        slab_grow(&m->ptslab, buf, INIT_PTSLAB_BUFFER_SIZE);
    }

    return SYS_ERR_OK;
}

void pmap_vnode_init(struct pmap *p, struct vnode *v)
{
    v->v.u.vnode.children = slab_alloc(&p->m.ptslab);
    assert(v->v.u.vnode.children);
    memset(v->v.u.vnode.children, 0, PTSLAB_SLABSIZE);
}

void pmap_vnode_insert_child(struct vnode *root, struct vnode *newvnode)
{
    size_t pte_count = newvnode->v.is_vnode ? 1 : newvnode->v.u.frame.pte_count;
    // check that we don't overflow children buffer
    assert(newvnode->v.entry + pte_count <= PTABLE_ENTRIES);
    for (int i = 0; i < pte_count; i++) {
        root->v.u.vnode.children[newvnode->v.entry+i] = newvnode;
    }
}

void pmap_vnode_free(struct pmap *pmap, struct vnode *n)
{
    slab_free(&pmap->m.ptslab, n->v.u.vnode.children);
    slab_free(&pmap->m.slab, n);
}

errval_t pmap_refill_slabs(struct pmap *pmap, size_t max_slabs)
{
    errval_t err;
    err = pmap_slab_refill(pmap, &pmap->m.slab, max_slabs);
    if (err_is_fail(err)) {
        return err;
    }
    err = pmap_slab_refill(pmap, &pmap->m.ptslab, max_slabs);
    if (err_is_fail(err)) {
        return err;
    }
    /* for pmap_array, after refilling ptslabs, we need to make sure that we
     * actually still have enough vnode slabs. */
    err = pmap_slab_refill(pmap, &pmap->m.slab, max_slabs);
    if (err_is_fail(err)) {
        return err;
    }
    return SYS_ERR_OK;
}
