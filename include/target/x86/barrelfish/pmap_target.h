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
#include <barrelfish_kpi/paging_arch.h> // for PTABLE_SIZE

#define MCN_COUNT DIVIDE_ROUND_UP(PTABLE_SIZE, L2_CNODE_SLOTS)

/// Node in the meta-data, corresponds to an actual VNode object
struct vnode { // NB: misnomer :)
    uint16_t      entry;       ///< Page table entry of this VNode
    bool          is_vnode;    ///< Is this a vnode, or a (leaf) page mapping
    bool          is_cloned;   ///< For copy-on-write: indicate whether we
                               //   still have to clone this vnode
    bool          is_pinned;   ///< is this a pinned vnode (do not reclaim automatically)
    enum objtype  type;        ///< Type of cap in the vnode
    struct vnode  *next;       ///< Next entry in list of siblings
    struct capref mapping;     ///< mapping cap associated with this node (stored in parent's mapping cnode)
    union {
        struct {
            lvaddr_t base;             ///< Virtual address start of page (upper level bits)
            struct capref cap;         ///< VNode cap
            struct capref invokable;    ///< Copy of VNode cap that is invokable
            struct capref mcn[MCN_COUNT]; ///< CNodes to store mappings (caprefs)
            struct cnoderef mcnode[MCN_COUNT]; ///< CNodeRefs of mapping cnodes
#ifdef PMAP_LL
            struct vnode *children;
#elif defined(PMAP_ARRAY)
            struct vnode  **children;   ///< Children of this VNode, allocated from second slab allocator
#else
#error Invalid pmap datastructure
#endif
            lvaddr_t virt_base;        ///< vaddr of mapped RO page table in user-space
            struct capref page_table_frame;
        } vnode; // for non-leaf node (maps another vnode)
        struct {
            struct capref cap;         ///< Frame cap
            genvaddr_t    offset;      ///< Offset within mapped frame cap
            vregion_flags_t flags;     ///< Flags for mapping
            size_t        pte_count;   ///< number of mapped PTEs in this mapping
            lvaddr_t vaddr;            ///< The virtual address this frame has
            uint16_t cloned_count;     ///< counter for #times a page of this range was cloned
        } frame; // for leaf node (maps an actual page)
    } u;
};

#define INIT_SLAB_COUNT 32
#define INIT_SLAB_BUFFER_SIZE SLAB_STATIC_SIZE(INIT_SLAB_COUNT, sizeof(struct vnode))
#ifdef PMAP_ARRAY
#define PTSLAB_SLABSIZE (sizeof(struct vnode *)*PTABLE_SIZE)
#define INIT_PTSLAB_BUFFER_SIZE SLAB_STATIC_SIZE(INIT_SLAB_COUNT, PTSLAB_SLABSIZE)
#endif

struct pmap_x86 {
    struct pmap p;
    struct vregion vregion;     ///< Vregion used to reserve virtual address for metadata
    genvaddr_t vregion_offset;  ///< Offset into amount of reserved virtual address used
    struct vnode root;          ///< Root of the vnode tree
    errval_t (*refill_slabs)(struct pmap_x86 *, size_t count); ///< Function to refill slabs
    errval_t (*refill_ptslab)(struct pmap_x86 *, size_t count); ///< Function to refill slabs
    struct slab_allocator slab;     ///< Slab allocator for the shadow page table entries
#ifdef PMAP_ARRAY
    struct slab_allocator ptslab;     ///< Slab allocator for the page table children arrays
#endif
    genvaddr_t min_mappable_va; ///< Minimum mappable virtual address
    genvaddr_t max_mappable_va; ///< Maximum mappable virtual address
    size_t used_cap_slots;      ///< Current count of capability slots allocated by pmap code
    uint8_t slab_buffer[INIT_SLAB_BUFFER_SIZE];      ///< Initial buffer to back the allocator
#ifdef PMAP_ARRAY
    uint8_t pt_slab_buffer[INIT_PTSLAB_BUFFER_SIZE];   ///< Pointer to initial buffer to back the pt allocator (static for own pmap, malloced for other pmaps)
#endif
};

#endif // TARGET_X86_BARRELFISH_PMAP_H
