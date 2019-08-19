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

#include <barrelfish_kpi/paging_arch.h> // for PTABLE_ENTRIES

#include <sys/cdefs.h>

__BEGIN_DECLS

// amount of slabs which are provided in core_data struct for own pmap
#define INIT_SLAB_COUNT 32

// 128 bytes is a rough estimate of sizeof(struct vnode)
#define VNODE_SLAB_SIZE 256
#define INIT_SLAB_BUFFER_SIZE SLAB_STATIC_SIZE(INIT_SLAB_COUNT, VNODE_SLAB_SIZE)

#if defined(PMAP_LL) || defined (__ARM_ARCH_7A__)

typedef struct vnode pmap_ds_child_t;

struct pmap_ds_meta {
    struct vnode *next; ///< Pointer to next vnode in linked list
};

struct pmap_vnode_mgmt {
    struct slab_allocator slab;     ///< Slab allocator for the shadow page table entries
    struct vregion vregion;         ///< Vregion used to reserve virtual address for metadata
    genvaddr_t vregion_offset;      ///< Offset into amount of reserved virtual address used
    uint8_t slab_buffer[INIT_SLAB_BUFFER_SIZE];
};

#elif defined(PMAP_ARRAY)

#define PTSLAB_SLABSIZE (sizeof(void *)*PTABLE_ENTRIES)
#define INIT_PTSLAB_BUFFER_SIZE SLAB_STATIC_SIZE(INIT_SLAB_COUNT, PTSLAB_SLABSIZE)

typedef struct vnode* pmap_ds_child_t;

struct pmap_ds_meta {
    // no metadata for datastructure
};

struct pmap_vnode_mgmt {
    struct slab_allocator slab;     ///< Slab allocator for the shadow page table entries
    struct slab_allocator ptslab;   ///< Slab allocator for the page table children arrays
    struct vregion vregion;         ///< Vregion used to reserve virtual address for metadata
    genvaddr_t vregion_offset;      ///< Offset into amount of reserved virtual address used
    uint8_t slab_buffer[INIT_SLAB_BUFFER_SIZE];
    uint8_t ptslab_buffer[INIT_PTSLAB_BUFFER_SIZE];
};

#else
#error Unknown Pmap datastructure.
#endif

__END_DECLS

#endif // BARRELFISH_PMAP_DS_H
