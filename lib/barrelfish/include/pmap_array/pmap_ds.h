/**
 * \file
 * \brief pmap datastructure header for array pmap. This file is
 * included by selecting the right include dir in lib/barrelfish/Hakefile
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBF_INCLUDE_PMAP_DS_H
#define LIBBF_INCLUDE_PMAP_DS_H

#define INIT_SLAB_BUFFER_SIZE SLAB_STATIC_SIZE(INIT_SLAB_COUNT, sizeof(struct vnode))
#define PTSLAB_SLABSIZE (sizeof(struct vnode *)*PTABLE_ENTRIES)
#define INIT_PTSLAB_BUFFER_SIZE SLAB_STATIC_SIZE(INIT_SLAB_COUNT, PTSLAB_SLABSIZE)

errval_t refill_vnode_slabs(struct pmap *pmap, size_t count);
errval_t refill_pt_slabs(struct pmap *pmap, size_t count);

/**
 * \brief find next non-null child starting from index i in children array of
 * `root`.
 * \returns the index of the next non-null child starting from i.
 */
static inline int pmap_next_child(struct vnode *root, int i, struct vnode **n)
{
    assert(n);
    struct vnode *tmp;
    do {
        // check child i
        tmp = root->u.vnode.children[i];
        i++;
    } while (!tmp && i < PTABLE_ENTRIES);
    *n = tmp;
    return i;
}

/**
 * \brief a macro that provides a datastructure-independent way of iterating
 * through the non-null children of the vnode `root`.
 * Internally this uses `pmap_next_child` to skip children which do not exist.
 *
 * Note: this macro requires both root and iter to be 'struct vnode *'.
 */
#define pmap_foreach_child(root, iter) \
    for (int i = pmap_next_child(root, 0, &iter); i < PTABLE_ENTRIES; i = pmap_next_child(root, i, &iter))

struct pmap_vnode_mgmt {
    struct slab_allocator slab;     ///< Slab allocator for the shadow page table entries
    struct slab_allocator ptslab;   ///< Slab allocator for the page table children arrays
    errval_t (*refill_slabs)(struct pmap *, size_t count); ///< Function to refill slabs
    errval_t (*refill_ptslab)(struct pmap *, size_t count); ///< Function to refill slabs
    struct vregion vregion;         ///< Vregion used to reserve virtual address for metadata
    genvaddr_t vregion_offset;      ///< Offset into amount of reserved virtual address used
};

#endif // LIBBF_INCLUDE_PMAP_DS_H
