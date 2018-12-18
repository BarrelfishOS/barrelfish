/**
 * \file
 * \brief Pmap definition common for the x86 archs
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_BARRELFISH_PMAP_H
#define TARGET_X86_BARRELFISH_PMAP_H

#include <barrelfish/pmap.h>
#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/paging_arch.h> // for PTABLE_ENTRIES
#include <barrelfish/pmap_ds.h>

#define MCN_COUNT DIVIDE_ROUND_UP(PTABLE_ENTRIES, L2_CNODE_SLOTS)

/// Node in the meta-data, corresponds to an actual VNode object
struct vnode { // NB: misnomer :)
    struct vnode_public v;   ///< public part of vnode
    bool          is_pinned; ///< is this a pinned vnode (do not reclaim automatically)
    bool          is_cloned; ///< For copy-on-write: indicate whether we
                             //   still have to clone this vnode
    struct vnode  *orig;     ///< vnode from which this one is cloned, for copy-on-write
    union {
        struct {
            lvaddr_t base;             ///< Virtual address start of page (upper level bits)
            struct capref mcn[MCN_COUNT]; ///< CNodes to store mappings (caprefs)
            struct cnoderef mcnode[MCN_COUNT]; ///< CNodeRefs of mapping cnodes
            lvaddr_t virt_base;        ///< vaddr of mapped RO page table in user-space
            struct capref page_table_frame;
        } vnode; // for non-leaf node (maps another vnode)
        struct {
            lvaddr_t vaddr;            ///< The virtual address this frame has
            uint16_t cloned_count;     ///< counter for #times a page of this range was cloned
        } frame; // for leaf node (maps an actual page)
    } u;
};

STATIC_ASSERT(sizeof(struct vnode) <= VNODE_SLAB_SIZE, "vnode slab size estimate big enough");

struct pmap_x86 {
    struct pmap p;
    struct vnode root;          ///< Root of the vnode tree
    genvaddr_t min_mappable_va; ///< Minimum mappable virtual address
    genvaddr_t max_mappable_va; ///< Maximum mappable virtual address
    size_t used_cap_slots;      ///< Current count of capability slots allocated by pmap code
};

#endif // TARGET_X86_BARRELFISH_PMAP_H
