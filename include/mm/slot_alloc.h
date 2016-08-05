/**
 * \file
 * \brief Slot management for memory allocator
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MM_SLOT_ALLOC_H
#define MM_SLOT_ALLOC_H

#include <sys/cdefs.h>
#include <errors/errno.h>
#include <barrelfish/caddr.h>

__BEGIN_DECLS

/// Generic interface to slot allocator function
typedef errval_t (*slot_alloc_t)(void *inst, uint64_t nslots, struct capref *ret);
typedef errval_t (*slot_refill_t)(void *inst);

/// Implementations of above interface
errval_t slot_alloc_prealloc(void *inst, uint64_t nslots, struct capref *ret);
errval_t slot_alloc_basecn(void *inst, uint64_t nslots, struct capref *ret);
errval_t slot_alloc_dynamic(void *inst, uint64_t nslots, struct capref *ret);
errval_t slot_refill_dynamic(void *inst);

struct mm; // forward declaration

/// Instance data for pre-allocating slot allocator for 2 level cspace
struct slot_prealloc {
    uint8_t maxslotbits;            ///< Maximum number of slots per allocation

    /// Metadata for next place from which to allocate slots
    struct {
        struct capref cap;        ///< Next cap to allocate
        uint64_t free;              ///< Number of free slots including cap
    } meta[2] __attribute__ ((aligned(4)));

    /// Which entry in meta array we are currently allocating from
    uint8_t current;

    /// RAM allocator to allocate space for new cnodes
    struct mm *mm;
};

/// Initialiser for the pre-allocating implementation
errval_t slot_prealloc_init(struct slot_prealloc *slot_alloc,
                            uint8_t maxslotbits, struct capref initial_cnode,
                            uint64_t initial_space, struct mm *ram_mm);

/// Refill function for the pre-allocating implementation
errval_t slot_prealloc_refill(struct slot_prealloc *inst);

/// Instance data for simple base-cnode allocator
struct slot_alloc_basecn {
    struct capref cap;          ///< Next cap to allocate
    uint64_t free;              ///< Number of free slots including cap
};

/// Initialiser for the single-cnode implementation
errval_t slot_alloc_basecn_init(struct slot_alloc_basecn *slot_alloc);

__END_DECLS

#endif // MM_SLOT_ALLOC_H
