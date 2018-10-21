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
    struct vnode *n;

    for(n = root->u.vnode.children; n != NULL; n = n->next) {
        if (!n->is_vnode) {
            // check whether entry is inside a large region
            uint16_t end = n->entry + n->u.frame.pte_count;
            if (n->entry <= entry && entry < end) {
                //if (n->entry < entry) {
                //    debug_printf("%d \\in [%d, %d]\n", entry, n->entry, end);
                //}
                return n;
            }
        }
        else if(n->entry == entry) {
            // return n if n is a vnode and the indices match
            return n;
        }
    }
    return NULL;
}

bool pmap_inside_region(struct vnode *root, uint16_t entry, uint16_t npages)
{
    assert(root != NULL);
    assert(root->is_vnode);

    struct vnode *n;

    for (n = root->u.vnode.children; n; n = n->next) {
        if (!n->is_vnode) {
            uint16_t end = n->entry + n->u.frame.pte_count;
            if (n->entry <= entry && entry + npages <= end) {
                return true;
            }
        }
    }

    return false;
}

void pmap_remove_vnode(struct vnode *root, struct vnode *item)
{
    assert(root->is_vnode);
    struct vnode *walk = root->u.vnode.children;
    struct vnode *prev = NULL;
    while (walk) {
        if (walk == item) {
            if (prev) {
                prev->next = walk->next;
                return;
            } else {
                root->u.vnode.children = walk->next;
                return;
            }
        }
        prev = walk;
        walk = walk->next;
    }
    USER_PANIC("Should not get here");
}
