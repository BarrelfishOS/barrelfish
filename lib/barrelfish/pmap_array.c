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

/**
 * \brief Starting at a given root, return the vnode with entry equal to #entry
 */
struct vnode *pmap_find_vnode(struct vnode *root, uint16_t entry)
{
    assert(root != NULL);
    assert(root->is_vnode);
    assert(entry < PTABLE_ENTRIES);

    if (root->u.vnode.children) {
        return root->u.vnode.children[entry];
    } else {
        return NULL;
    }
}

bool pmap_inside_region(struct vnode *root, uint16_t entry, uint16_t npages)
{
    assert(root != NULL);
    assert(root->is_vnode);

    struct vnode *n = root->u.vnode.children[entry];

    // empty or ptable
    if (!n || n->is_vnode) {
        return false;
    }

    uint16_t end = n->entry + n->u.frame.pte_count;
    if (n->entry <= entry && entry + npages <= end) {
        return true;
    }

    return false;
}

void pmap_remove_vnode(struct vnode *root, struct vnode *item)
{
    assert(root->is_vnode);
    size_t pte_count = item->is_vnode ? 1 : item->u.frame.pte_count;
    for (int i = 0; i < pte_count; i++) {
        root->u.vnode.children[item->entry+i] = NULL;
    }
}

struct pmap_vnode_mgmt mymgmt;
uint8_t slab_buffer[INIT_SLAB_BUFFER_SIZE];
uint8_t pt_slab_buffer[INIT_PTSLAB_BUFFER_SIZE];

errval_t pmap_vnode_mgmt_init(struct pmap *pmap)
{
    struct pmap_vnode_mgmt *m = NULL;
    /* special case init of own pmap vnode mgmt */
    if (get_current_pmap() == pmap) {
        /* x86 specific portion */
        slab_init(&mymgmt.slab, sizeof(struct vnode), NULL);
        slab_grow(&mymgmt.slab, slab_buffer, INIT_SLAB_BUFFER_SIZE);

        /* Initialize slab allocator for child arrays */
        slab_init(&mymgmt.ptslab, PTSLAB_SLABSIZE, NULL);
        slab_grow(&mymgmt.ptslab, pt_slab_buffer, INIT_PTSLAB_BUFFER_SIZE);

        /* use static vnode_mgmt struct */
        m = &mymgmt;
    } else {
        /* allocate vnode_mgmt struct */
        m = calloc(1, sizeof(*m));
        if (!m) {
            return LIB_ERR_MALLOC_FAIL;
        }

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

    m->refill_slabs = refill_vnode_slabs;
    m->refill_ptslab = refill_pt_slabs;

    /* assign vnode_mgmt to pmap */
    pmap->m = m;

    return SYS_ERR_OK;
}

void pmap_vnode_init(struct pmap *p, struct vnode *v)
{
    v->u.vnode.children = slab_alloc(&p->m->ptslab);
    assert(v->u.vnode.children);
    memset(v->u.vnode.children, 0, PTSLAB_SLABSIZE);
}

void pmap_vnode_insert_child(struct vnode *root, struct vnode *newvnode)
{
    root->u.vnode.children[newvnode->entry] = newvnode;
    newvnode->next = NULL;
}

void pmap_vnode_free(struct pmap *pmap, struct vnode *n)
{
    slab_free(&pmap->m->ptslab, n->u.vnode.children);
    slab_free(&pmap->m->slab, n);
}
