/**
 * \file
 * \brief Slot management for the memory allocator.
 */

/*
 * Copyright (c) 2007, 2008, 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <mm/mm.h>
#include <mm/slot_alloc.h>
#include <stdio.h>

static errval_t rootcn_alloc(void *st, uint8_t reqbits, struct capref *ret)
{
    return mm_alloc(st, reqbits, ret, NULL);
}

/// Allocate a new cnode if needed
errval_t slot_prealloc_refill(struct slot_prealloc *this)
{
    uint8_t refill = !this->current;
    errval_t err;

    if (this->meta[refill].free == L2_CNODE_SLOTS) {
        return SYS_ERR_OK; // Nop
    }

    // Allocate a ram cap
    struct capref ram_cap;
    err = mm_alloc(this->mm, L2_CNODE_BITS + OBJBITS_CTE, &ram_cap, NULL);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_SLOT_MM_ALLOC);
    }

    // Retype to and build the next cnode
    struct capref cnode_cap;
    err = slot_alloc_root(&cnode_cap);
    if (err_no(err) == LIB_ERR_SLOT_ALLOC_NO_SPACE) {
        // resize root slot allocator (and rootcn)
        err = root_slot_allocator_refill(rootcn_alloc, this->mm);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_ROOTSA_RESIZE);
        }
        // retry slot_alloc_root
        err = slot_alloc_root(&cnode_cap);
    }
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    err = cnode_create_from_mem(cnode_cap, ram_cap, ObjType_L2CNode,
            &this->meta[refill].cap.cnode, L2_CNODE_SLOTS);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE);
    }

    // Set the metadata
    this->meta[refill].cap.slot  = 0;
    this->meta[refill].free      = L2_CNODE_SLOTS;

    return SYS_ERR_OK;
}

errval_t slot_alloc_prealloc(void *inst, uint64_t nslots, struct capref *ret)
{
    struct slot_prealloc *this = inst;
    assert(nslots <= (1UL << this->maxslotbits));

    /* Check if enough space */
    if (this->meta[this->current].free < nslots) {
        // Allocate from next cnode
        this->current = !this->current;
    }

    if (this->meta[this->current].free < nslots) {
        return MM_ERR_SLOT_NOSLOTS;
    }

    /* Return next slot and update */
    *ret = this->meta[this->current].cap;
    this->meta[this->current].cap.slot += nslots;
    this->meta[this->current].free -= nslots;

    return SYS_ERR_OK;
}

/**
 * \brief Initialise preallocating slot allocator instance
 *
 * \param this Pointer to area for instance data
 * \param maxslotbits Maximum size of each allocation (in bits)
 * \param initial_cnode First cap in an empty cnode to start allocating from
 * \param initial_space Number of slots free in initial cnode
 * \param ram_mm Memory allocator to use for RAM caps when creating new CNodes
 */
errval_t slot_prealloc_init(struct slot_prealloc *this, uint8_t maxslotbits,
                            struct capref initial_cnode, uint64_t initial_space,
                            struct mm *ram_mm)
{
    this->maxslotbits = maxslotbits;
    this->mm = ram_mm;

    assert(initial_space == L2_CNODE_SLOTS);
    if (initial_space != L2_CNODE_SLOTS) {
        debug_printf("Initial CNode for 2 level preallocating slot allocator needs to be 16kB");
        return LIB_ERR_SLOT_ALLOC_INIT;
    }

    this->current = 0;
    this->meta[0].cap       = initial_cnode;
    this->meta[0].free      = initial_space;
    this->meta[1].free      = 0;

    return SYS_ERR_OK;
}

errval_t slot_alloc_basecn_init(struct slot_alloc_basecn *this)
{
    // Use ROOTCN_SLOT_SLOT_ALLOC0 as CNode fore basecn allocator
    this->cap.cnode.croot = CPTR_ROOTCN;
    this->cap.cnode.cnode = ROOTCN_SLOT_ADDR(ROOTCN_SLOT_SLOT_ALLOC0);
    this->cap.cnode.level = CNODE_TYPE_OTHER;
    this->cap.slot = 0;
    this->free = L2_CNODE_SLOTS;

    return SYS_ERR_OK;
}

errval_t slot_alloc_basecn(void *inst, uint64_t nslots, struct capref *ret)
{
    struct slot_alloc_basecn *this = inst;
    errval_t err;

    if (nslots > this->free) {
        /* XXX: Special case for init, need to get memory from basecn */
        struct capref ram;
        err = ram_alloc(&ram, L2_CNODE_BITS + OBJBITS_CTE);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "ram_alloc in slot_alloc_basecn cannot allocate L2 "
                           "CNode-sized ram cap");
            return err_push(err, LIB_ERR_RAM_ALLOC);
        }

        /* to conform with 2 level cspace: put new cnode into rootcn */
        struct capref cnode;
        err = slot_alloc_root(&cnode);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "allocating root cnode slot");
            return err_push(err, LIB_ERR_SLOT_ALLOC);
        }

        err = cnode_create_from_mem(cnode, ram, ObjType_L2CNode,
                                    &this->cap.cnode, L2_CNODE_SLOTS);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_CNODE_CREATE);
        }

        this->cap.slot = 0;
        this->free = L2_CNODE_SLOTS;
    }

    assert(nslots <= this->free);
    *ret = this->cap;
    this->cap.slot += nslots;
    this->free -= nslots;

    return SYS_ERR_OK;
}

/// Requires an instance of range_slot_allocator
errval_t slot_alloc_dynamic(void *alloc, uint64_t nslots, struct capref *ret)
{
    return range_slot_alloc(alloc, nslots, ret);
}

errval_t slot_refill_dynamic(void *alloc)
{
    return range_slot_alloc_refill(alloc, L2_CNODE_SLOTS);
}
