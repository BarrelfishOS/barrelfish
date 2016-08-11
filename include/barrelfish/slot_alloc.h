/**
 * \file
 * \brief Slot allocator
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SLOT_ALLOC_H
#define SLOT_ALLOC_H

#include <sys/cdefs.h>

#include <barrelfish/threads.h>
#include <barrelfish/slab.h>
#include <barrelfish/vspace_common.h>

__BEGIN_DECLS

struct slot_allocator {
    errval_t (*alloc)(struct slot_allocator *ca, struct capref *cap);
    errval_t (*free)(struct slot_allocator *ca, struct capref cap);
    struct thread_mutex mutex;     ///< Mutex for thread safety
    cslot_t nslots;                ///< Slots to grow allocator by
    cslot_t space;                 ///< Space left in the allocator
};

/// Meta data for single_slot_allocator
struct cnode_meta {
    cslot_t slot;
    cslot_t space;
    struct cnode_meta *next;
};

struct single_slot_allocator {
    struct slot_allocator a;    ///< Public data
    struct capref cap;          ///< Cap of the cnode the allocator is tracking
    struct cnoderef cnode;      ///< Cnode the allocator is tracking
    struct cnode_meta *head;    ///< Linked list of free slots
    struct slab_allocator slab;     ///< Slab for backing the list
};

struct slot_allocator_list {
    struct single_slot_allocator a;
    struct slot_allocator_list *next;
};

struct multi_slot_allocator {
    struct slot_allocator a;      ///< Public data

    struct slot_allocator_list *head; ///< List of single slot allocators
    struct slot_allocator_list *reserve; ///< One single allocator in reserve

    struct slab_allocator slab;      ///< Slab backing the slot_allocator_list

    struct vspace_mmu_aware mmu_state;
};

struct range_slot_allocator {
    struct capref cnode_cap;     ///< capref for the L1 cnode
    struct cnoderef cnode;       ///< cnoderef for the cnode to allocate from
    struct cnode_meta *meta;     ///< Linked list of meta data
    struct slab_allocator slab;      ///< Slab allocation
    struct thread_mutex mutex;   ///< Mutex for thread safety (used when is_head == true)
    struct range_slot_allocator *next; ///< Next slot allocator
    bool is_head; ///< Is this instance head of a chain
};

// single_slot_alloc_init_raw() requires a specific buflen
#define SINGLE_SLOT_ALLOC_BUFLEN(nslots) \
    (SLAB_STATIC_SIZE(nslots / 2, sizeof(struct cnode_meta)))

errval_t single_slot_alloc_init(struct single_slot_allocator *ret,
                                cslot_t nslots, cslot_t *retslots);
errval_t single_slot_alloc_init_raw(struct single_slot_allocator *ret,
                                    struct capref cap, struct cnoderef cnode,
                                    cslot_t nslots, void *buf, size_t buflen);

cslot_t single_slot_alloc_freecount(struct single_slot_allocator *s);
errval_t single_slot_alloc_resize(struct single_slot_allocator *this,
                                  cslot_t newslotcount);

errval_t two_level_slot_alloc_init(struct multi_slot_allocator *ret);
errval_t two_level_slot_alloc_init_raw(struct multi_slot_allocator *ret,
                                       struct capref initial_cap,
                                       struct cnoderef initial_cnode,
                                       struct capref reserve_cap,
                                       struct cnoderef reserve_cnode,
                                       void *head_buf, void *reserve_buf, size_t bufsize);

errval_t slot_alloc_init(void);
struct slot_allocator *get_default_slot_allocator(void);
errval_t slot_alloc(struct capref *ret);

/// Root slot allocator functions
errval_t slot_alloc_root(struct capref *ret);
typedef errval_t (*cn_ram_alloc_func_t)(void *st, uint8_t reqbits, struct capref *ret);
errval_t root_slot_allocator_refill(cn_ram_alloc_func_t myalloc, void *allocst);

errval_t slot_free(struct capref ret);

errval_t range_slot_alloc(struct range_slot_allocator *alloc, cslot_t nslots,
                          struct capref *ret);
errval_t range_slot_free(struct range_slot_allocator *alloc, struct capref cap,
                         cslot_t nslots);
errval_t range_slot_alloc_init(struct range_slot_allocator *ret,
                               cslot_t nslots, cslot_t *retslots);
size_t range_slot_alloc_freecount(struct range_slot_allocator *alloc);
errval_t range_slot_alloc_refill(struct range_slot_allocator *alloc, cslot_t slots);

__END_DECLS

#endif // SLOT_ALLOC_H
