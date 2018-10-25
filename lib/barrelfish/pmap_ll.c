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
    struct vnode *n;

    for(n = root->v.u.vnode.children; n != NULL; n = n->v.meta.next) {
        if (!n->v.is_vnode) {
            // check whether entry is inside a large region
            uint16_t end = n->v.entry + n->v.u.frame.pte_count;
            if (n->v.entry <= entry && entry < end) {
                //if (n->v.entry < entry) {
                //    debug_printf("%d \\in [%d, %d]\n", entry, n->v.entry, end);
                //}
                return n;
            }
        }
        else if(n->v.entry == entry) {
            // return n if n is a vnode and the indices match
            return n;
        }
    }
    return NULL;
}

bool pmap_inside_region(struct vnode *root, uint16_t entry, uint16_t npages)
{
    assert(root != NULL);
    assert(root->v.is_vnode);

    struct vnode *n;

    for (n = root->v.u.vnode.children; n; n = n->v.meta.next) {
        if (!n->v.is_vnode) {
            uint16_t end = n->v.entry + n->v.u.frame.pte_count;
            if (n->v.entry <= entry && entry + npages <= end) {
                return true;
            }
        }
    }

    return false;
}

void pmap_remove_vnode(struct vnode *root, struct vnode *item)
{
    assert(root->v.is_vnode);
    struct vnode *walk = root->v.u.vnode.children;
    struct vnode *prev = NULL;
    while (walk) {
        if (walk == item) {
            if (prev) {
                prev->v.meta.next = walk->v.meta.next;
                return;
            } else {
                root->v.u.vnode.children = walk->v.meta.next;
                return;
            }
        }
        prev = walk;
        walk = walk->v.meta.next;
    }
    USER_PANIC("Should not get here");
}

struct pmap_vnode_mgmt mymgmt = { 0 };
static uint8_t slab_buffer[INIT_SLAB_BUFFER_SIZE];
errval_t pmap_vnode_mgmt_init(struct pmap *pmap)
{
    struct pmap_vnode_mgmt *m = NULL;
    if (get_current_pmap() == pmap) {
        m = &mymgmt;
        slab_init(&m->slab, sizeof(struct vnode), NULL);
        /* use static buffer for own pmap */
        slab_grow(&m->slab, slab_buffer, INIT_SLAB_BUFFER_SIZE);
    } else {
        m = calloc(1, sizeof(*m));
        if (!m) {
            return LIB_ERR_MALLOC_FAIL;
        }
        /* malloc initial buffer for other pmaps */
        uint8_t *buf = malloc(INIT_SLAB_BUFFER_SIZE);
        if (!buf) {
            return LIB_ERR_MALLOC_FAIL;
        }
        slab_init(&m->slab, sizeof(struct vnode), NULL);
        slab_grow(&m->slab, buf, INIT_SLAB_BUFFER_SIZE);
    }

    pmap->m = m;

    return SYS_ERR_OK;
}

void pmap_vnode_init(struct pmap *p, struct vnode *v)
{
    v->v.u.vnode.children = NULL;
    v->v.meta.next = NULL;
}

void pmap_vnode_insert_child(struct vnode *root, struct vnode *newvnode)
{
    newvnode->v.meta.next = root->v.u.vnode.children;
    root->v.u.vnode.children = newvnode;
}

void pmap_vnode_free(struct pmap *pmap, struct vnode *n)
{
    slab_free(&pmap->m->slab, n);
}

errval_t pmap_refill_slabs(struct pmap *pmap, size_t max_slabs)
{
    return pmap_slab_refill(pmap, &pmap->m->slab, max_slabs);
}
