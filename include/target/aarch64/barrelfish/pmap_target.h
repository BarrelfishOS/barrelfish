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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_AARCH64_BARRELFISH_PMAP_H
#define TARGET_AARCH64_BARRELFISH_PMAP_H

#include <barrelfish/pmap.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish/pmap_ds.h>

#define MCN_COUNT DIVIDE_ROUND_UP(PTABLE_ENTRIES, L2_CNODE_SLOTS)

/// Node in the meta-data, corresponds to an actual VNode object
struct vnode {
    struct vnode_public v;    ///< Public part of struct vnode
    union {
        struct {
            struct capref mcn[MCN_COUNT]; ///< CNodes to store mappings (caprefs)
            struct cnoderef mcnode[MCN_COUNT]; ///< CNodeRefs of mapping cnodes
        } vnode; // for non-leaf node
        struct {
            // no custom fields for frame
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
