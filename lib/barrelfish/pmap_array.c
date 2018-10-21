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

/**
 * \brief Starting at a given root, return the vnode with entry equal to #entry
 */
struct vnode *pmap_find_vnode(struct vnode *root, uint16_t entry)
{
    assert(root != NULL);
    assert(root->is_vnode);
    assert(entry < PTABLE_SIZE);

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
