/**
 * \file
 * \brief Pmap definition common for the arm archs
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_ARM_BARRELFISH_PMAP_H
#define TARGET_ARM_BARRELFISH_PMAP_H

#include <barrelfish/pmap.h>

/// Node in the meta-data, corresponds to an actual VNode object
struct vnode {
    uint16_t      entry;       ///< Page table entry of this VNode
    bool          is_vnode;    ///< Is this a page table or a page mapping
    struct vnode  *next;       ///< Next entry in list of siblings
    struct capref mapping;     ///< Mapping cap for this vnode
    union {
        struct {
            struct capref cap;           ///< Capability of this VNode
            struct capref invokable;     ///< Invokable copy of Capability of this VNode
            struct vnode  *children;     ///< Children of this VNode
        } vnode; // for non-leaf node
        struct {
            struct capref   cap;         ///< Capability of this VNode
            genvaddr_t      offset;      ///< Offset within mapped frame cap
            vregion_flags_t flags;       ///< Flags for mapping
            uint16_t        pte_count;   ///< number of page table entries consumed by this mapping
        } frame; // for leaf node (maps page(s)/section(s))
    } u;
};

struct pmap_arm {
    struct pmap p;
    struct vregion vregion;     ///< Vregion used to reserve virtual address for metadata
    genvaddr_t vregion_offset;  ///< Offset into amount of reserved virtual address used
    struct vnode root;          ///< Root of the vnode tree
    struct slab_allocator slab;     ///< Slab allocator for the vnode lists
    uint8_t slab_buffer[512];   ///< Initial buffer to back the allocator
};

#endif // TARGET_ARM_BARRELFISH_PMAP_H
