/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_PMAP_PRIV_H
#define LIBBARRELFISH_PMAP_PRIV_H

/**
 * \brief internal mapping function. This assumes that enough slabs are
 * available for metadata.
 */
errval_t do_map(struct pmap *pmap, genvaddr_t vaddr,
                struct capref frame, size_t offset, size_t size,
                vregion_flags_t flags, size_t *retoff, size_t *retsize);

/**
 * \brief return the number of slabs required for mapping a region of size
 * `bytes` with small pages.
 */
size_t max_slabs_required(size_t bytes);

errval_t pmap_slab_refill(struct pmap *pmap, struct slab_allocator *slab,
                          size_t max_slabs_for_mapping);

errval_t pmap_vnode_mgmt_current_init(struct pmap *pmap);

static inline void
set_mapping_cap(struct pmap *pmap, struct vnode *vnode,
                struct vnode *root, uint16_t entry)
{
    assert(root->v.is_vnode);
    assert(entry < PTABLE_ENTRIES);
#ifdef GLOBAL_MCN
    vnode->v.mapping.cnode = root->u.vnode.mcnode[entry / L2_CNODE_SLOTS];
    vnode->v.mapping.slot  = entry % L2_CNODE_SLOTS;
    assert(!cnoderef_is_null(vnode->v.mapping.cnode));
    assert(!capref_is_null(vnode->v.mapping));
#else
    /* just use the slot allocator associated with the pmap. */
    errval_t err;
    err = pmap->slot_alloc->alloc(pmap->slot_alloc, &vnode->v.mapping);
    // TODO: error propagation
    assert(err_is_ok(err));
#endif
}

#endif // LIBBARRELFISH_PMAP_PRIV_H
