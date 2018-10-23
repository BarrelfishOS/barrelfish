/**
 * \file
 * \brief Pmap definition common for the AARCH64 archs
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_AARCH64_BARRELFISH_PMAP_H
#define TARGET_AARCH64_BARRELFISH_PMAP_H

#include <barrelfish/pmap.h>
#include <barrelfish_kpi/paging_arch.h>

#define MCN_COUNT DIVIDE_ROUND_UP(PTABLE_ENTRIES, L2_CNODE_SLOTS)

/// Node in the meta-data, corresponds to an actual VNode object
struct vnode {
    uint16_t      entry;       ///< Page table entry of this VNode
    bool          is_vnode;    ///< Is this a page table or a page mapping
    struct vnode  *next;       ///< Next entry in list of siblings
    struct capref mapping;     ///< the mapping for this vnode
    union {
        struct {
            struct capref cap;         ///< Capability of this VNode
            struct capref invokable;    ///< Copy of VNode cap that is invokable
            struct capref mcn[MCN_COUNT]; ///< CNodes to store mappings (caprefs)
            struct cnoderef mcnode[MCN_COUNT]; ///< CNodeRefs of mapping cnodes
#if defined(PMAP_LL)
            struct vnode  *children;   ///< Children of this VNode
#elif defined(PMAP_ARRAY)
            struct vnode **children;
#else
#error invalid pmap datastructure
#endif
        } vnode; // for non-leaf node
        struct {
            struct capref cap;         ///< Capability of this VNode
            genvaddr_t    offset;      ///< Offset within mapped frame cap
            vregion_flags_t flags;     ///< Flags for mapping
            size_t        pte_count;   ///< number of mapped PTEs in this mapping
        } frame; // for leaf node (maps page(s))
    } u;
};

struct pmap_aarch64 {
    struct pmap p;
    struct vnode root;          ///< Root of the vnode tree
    genvaddr_t min_mappable_va; ///< Minimum mappable virtual address
    genvaddr_t max_mappable_va; ///< Maximum mappable virtual address
};

#endif // TARGET_AARCH64_BARRELFISH_PMAP_H
