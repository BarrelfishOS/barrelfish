/**
 * \file
 * \brief Generic pmap definitions
 */

/*
 * Copyright (c) 2009-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_PMAP_H
#define LIBBARRELFISH_PMAP_H

struct pmap_dump_info;
struct pmap;
struct pmap_mapping_info;
struct pmap_res_info;
struct pmap_funcs {
    errval_t (*determine_addr)(struct pmap *pmap, struct memobj *memobj,
                               size_t alignment, genvaddr_t *vaddr);
    errval_t (*determine_addr_raw)(struct pmap *pmap, size_t size,
                                   size_t alignment, genvaddr_t *vaddr);
    errval_t (*map)(struct pmap* pmap, genvaddr_t vaddr, struct capref frame,
                    size_t offset, size_t size, vregion_flags_t flags,
                    size_t *retoffset, size_t *retsize);
    errval_t (*unmap)(struct pmap* pmap, genvaddr_t vaddr, size_t size,
                      size_t *retsize);
    errval_t (*modify_flags)(struct pmap* pmap, genvaddr_t vaddr, size_t size,
                             vregion_flags_t flags, size_t *retsize);
    errval_t (*lookup)(struct pmap *pmap, genvaddr_t vaddr, struct pmap_mapping_info *info);
    /* create the page-tables for a range and mark them as pinned */
    errval_t (*create_pts_pinned)(struct pmap *pmap, genvaddr_t vaddr, size_t bytes,
                                  vregion_flags_t flags);
    /* get the leaf pt of a virtual address and returns the mapping of it
     * XXX possibly it's better to return the cap here? */
    errval_t (*get_leaf_pt)(struct pmap *pmap, genvaddr_t vaddr, lvaddr_t *va);
    errval_t (*serialise)(struct pmap *pmap, void *buf, size_t buflen);
    errval_t (*deserialise)(struct pmap *pmap, void *buf, size_t buflen);

    errval_t (*dump)(struct pmap *pmap, struct pmap_dump_info *buf, size_t buflen,
		     size_t *items_written);
    errval_t (*measure_res)(struct pmap *pmap, struct pmap_res_info *buf);
};

struct pmap {
    struct pmap_funcs f;
    struct vspace *vspace;      ///< The vspace this pmap is associated with
    struct slot_allocator *slot_alloc; ///< (Optional) slot allocator for vnodes
};

struct pmap_mapping_info {
    genvaddr_t vaddr;        ///< The page-aligned virtual address
    size_t size;             ///< Size of the backing page
    struct capref cap;       ///< Capability to the frame mapped here
    genvaddr_t offset;       ///< Offset into the frame
    vregion_flags_t flags;   ///< Mapping flags
    struct capref mapping;   ///< Mapping cap
};

struct pmap_res_info {
    size_t vnode_used;       ///< Amount of memory in use for vnodes in bytes
    size_t vnode_free;       ///< Amount of memory allocated for vnodes in bytes
    size_t slots_used;       ///< Number of capability slots in use by pmap
};

/*
 * API for walking pmap -- implementation independent
 */
/**
 * \brief Pmap traversal: return the vnode with entry equal to #entry in vnode `root`.
 */
struct vnode *pmap_find_vnode(struct vnode *root, uint16_t entry);
/**
 * \brief Pmap traversal: check if [entry, entry+npages) is inside a single mapping in `root`
 */
bool pmap_inside_region(struct vnode *root, uint16_t entry, uint16_t npages);
/**
 * \brief Pmap traversal: remove vnode `item` from vnode `root`
 */
void pmap_remove_vnode(struct vnode *root, struct vnode *item);

#endif // LIBBARRELFISH_PMAP_H
