/**
 * \file
 * \brief Pmap definition common for the x86 archs, but private to libbarrelfish
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * Copyright (c) 2014, HP Labs.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_BARRELFISH_PMAP_X86_H
#define TARGET_X86_BARRELFISH_PMAP_X86_H

struct pmap;

errval_t pmap_x86_serialise(struct pmap *pmap, void *buf, size_t buflen);
errval_t pmap_x86_deserialise(struct pmap *pmap, void *buf, size_t buflen);
errval_t pmap_x86_determine_addr(struct pmap *pmap, struct memobj *memobj,
                                 size_t alignment, genvaddr_t *vaddr);

/**
 * \brief check whether vnode `root` has children in [entry .. entry+len).
 * \return * true iff `root` has children in [entry .. entry+len) and
 *           only_pages false
 *         * true iff `root` has valid page mappings in [entry .. entry+len)
 *           and only_pages true
 */
bool has_vnode(struct vnode *root, uint32_t entry, size_t len,
               bool only_pages);
/**
 * \return vnode at `entry` in `root`. NULL if no vnode there.
 */
struct vnode *find_vnode(struct vnode *root, uint16_t entry);

/**
 * \return true iff [entry..entry+npages) inside a child of `root`.
 */
bool inside_region(struct vnode *root, uint32_t entry, uint32_t npages);

/**
 * \brief remove vnode `item` from list of children of `root`.
 */
void remove_vnode(struct vnode *root, struct vnode *item);

/**
 * \brief allocate vnode as child of `root` with type `type`. Allocates the
 * struct vnode with `pmap`'s slab allocator.
 * \arg entry the entry at which the new vnode is inserted
 * \arg retvnode pointer to the new vnode.
 */
errval_t alloc_vnode(struct pmap_x86 *pmap, struct vnode *root,
                     enum objtype type, uint32_t entry,
                     struct vnode **retvnode);

/**
 * \brief remove vnodes with no leafs in [entry .. entry+len), destroy their
 * associated capabilities and free their slabs.
 */
void remove_empty_vnodes(struct pmap_x86 *pmap, struct vnode *root,
                         uint32_t entry, size_t len);

#endif // TARGET_X86_BARRELFISH_PMAP_X86_H
