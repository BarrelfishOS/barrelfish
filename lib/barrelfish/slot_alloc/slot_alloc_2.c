/**
 * \file
 * \brief Slot allocator for two-level CSpace
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/core_state.h>
#include <barrelfish/caddr.h>
#include "internal.h"


/**
 * \brief Initialize the slot_allocator for two-level CSpace
 *
 * Initializes the default and root slot_allocator.
 */
errval_t slot_alloc_init_2(void)
{
    errval_t err;

    struct slot_alloc_state *state = get_slot_alloc_state();

    /* Default allocator */
    // While initializing, other domains will call into it. Be careful
    struct capref cap;
    struct cnoderef cnode;
    struct multi_slot_allocator *def = &state->defca;

    // Generic
    thread_mutex_init(&def->a.mutex);

    def->a.alloc = two_level_alloc;
    def->a.free  = two_level_free;
    def->a.space = SLOT_ALLOC_CNODE_SLOTS;
    def->a.nslots = SLOT_ALLOC_CNODE_SLOTS;

    def->top = NULL;
    def->head = &state->head;
    def->head->next = NULL;
    def->reserve = &state->reserve;
    def->reserve->next = NULL;

#if 0
    // Top: not used for 2level cspace, put CNode into standard list
    cap.cnode = cnode_root;
    cap.slot  = ROOTCN_SLOT_SLOT_ALLOC0;
    //cnode = build_cnoderef(cap, SLOT_ALLOC_CNODE_BITS);
    cnode.croot = CPTR_ROOTCN;
    cnode.root_level = CSPACE_LEVEL_L2;
    cnode.cnode = ROOTCN_SLOT_ADDR(ROOTCN_SLOT_SLOT_ALLOC0);
    cnode.node_level = CSPACE_LEVEL_L2;
    err = single_slot_alloc_init_raw((struct single_slot_allocator*)def->head->next,
                                     cap, cnode,
                                     SLOT_ALLOC_CNODE_SLOTS, state->top_buf,
                                     sizeof(state->top_buf));
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT_RAW);
    }
#endif

    // Head
    cap.cnode = cnode_root;
    cap.slot  = ROOTCN_SLOT_SLOT_ALLOC1;
    cnode = build_cnoderef(cap, CNODE_TYPE_OTHER);
    /*
    cnode.croot = CPTR_ROOTCN;
    cnode.cnode = ROOTCN_SLOT_ADDR(ROOTCN_SLOT_SLOT_ALLOC1);
    cnode.level = CSPACE_LEVEL_L2;
    */
    err = single_slot_alloc_init_raw(&def->head->a, cap, cnode,
                                     SLOT_ALLOC_CNODE_SLOTS, state->head_buf,
                                     sizeof(state->head_buf));
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT_RAW);
    }

    // Reserve
    cap.cnode = cnode_root;
    cap.slot  = ROOTCN_SLOT_SLOT_ALLOC2;
    cnode = build_cnoderef(cap, CNODE_TYPE_OTHER);
    /*
    cnode.croot = CPTR_ROOTCN;
    cnode.root_level = CSPACE_LEVEL_L2;
    cnode.cnode = ROOTCN_SLOT_ADDR(ROOTCN_SLOT_SLOT_ALLOC2);
    cnode.node_level = CSPACE_LEVEL_L2;
    */
    err = single_slot_alloc_init_raw(&def->reserve->a, cap, cnode,
                                     SLOT_ALLOC_CNODE_SLOTS, state->reserve_buf,
                                     sizeof(state->reserve_buf));
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT_RAW);
    }

    // Slab
    size_t allocation_unit = sizeof(struct slot_allocator_list) +
                             SINGLE_SLOT_ALLOC_BUFLEN(SLOT_ALLOC_CNODE_SLOTS);
    slab_init(&def->slab, allocation_unit, NULL);

    // Vspace mgmt
    // Warning: necessary to do this in the end as during initialization,
    // libraries can call into slot_alloc.
    err = vspace_mmu_aware_init(&def->mmu_state,
                                allocation_unit * SLOT_ALLOC_CNODE_SLOTS * 2);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MMU_AWARE_INIT);
    }

    /* Root allocator */
    err = single_slot_alloc_init_raw(&state->rootca, cap_root, cnode_root,
                                     DEFAULT_CNODE_SLOTS, state->root_buf,
                                     sizeof(state->root_buf));
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SINGLE_SLOT_ALLOC_INIT_RAW);
    }
    state->rootca.a.space     = DEFAULT_CNODE_SLOTS - ROOTCN_FREE_SLOTS;
    state->rootca.head->space = DEFAULT_CNODE_SLOTS - ROOTCN_FREE_SLOTS;
    state->rootca.head->slot  = ROOTCN_FREE_SLOTS;

    return SYS_ERR_OK;
}
