/**
 * \file
 * \brief public declarations for pmap datastructure stuff.
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_PMAP_DS_H
#define BARRELFISH_PMAP_DS_H

#if defined(PMAP_LL)

typedef struct vnode pmap_ds_child_t;

struct pmap_ds_meta {
    struct vnode *next; ///< Pointer to next vnode in linked list
};

struct pmap_vnode_mgmt {
    struct slab_allocator slab;     ///< Slab allocator for the shadow page table entries
    struct vregion vregion;         ///< Vregion used to reserve virtual address for metadata
    genvaddr_t vregion_offset;      ///< Offset into amount of reserved virtual address used
};

#elif defined(PMAP_ARRAY)

typedef struct vnode* pmap_ds_child_t;

struct pmap_ds_meta {
    // no metadata for datastructure
};

struct pmap_vnode_mgmt {
    struct slab_allocator slab;     ///< Slab allocator for the shadow page table entries
    struct slab_allocator ptslab;   ///< Slab allocator for the page table children arrays
    struct vregion vregion;         ///< Vregion used to reserve virtual address for metadata
    genvaddr_t vregion_offset;      ///< Offset into amount of reserved virtual address used
};

#else
#error Unknown Pmap datastructure.
#endif

#endif // BARRELFISH_PMAP_DS_H
