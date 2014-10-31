/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <errors/errno.h>
#include <barrelfish/types.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/distcaps.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <mdb/mdb.h>
#include <mdb/mdb_tree.h>

static void
mdb_set_cte_relations(struct cte *cte, uint8_t relations, uint8_t mask)
{
    if (mask & RRELS_COPY_BIT) {
        cte->mdbnode.remote_copies = (relations & RRELS_COPY_BIT) != 0;
    }
    if (mask & RRELS_ANCS_BIT) {
        cte->mdbnode.remote_ancs = (relations & RRELS_ANCS_BIT) != 0;
    }
    if (mask & RRELS_DESC_BIT) {
        cte->mdbnode.remote_descs = (relations & RRELS_DESC_BIT) != 0;
    }
}

void mdb_set_relations(struct cte *cte, uint8_t relations, uint8_t mask)
{
    assert(cte != NULL);

    bool old_copies = cte->mdbnode.remote_copies;
    bool old_ancs = cte->mdbnode.remote_ancs;
    bool old_descs = cte->mdbnode.remote_descs;

    mdb_set_cte_relations(cte, relations, mask);

    if (cte->mdbnode.remote_copies == old_copies
        && cte->mdbnode.remote_ancs == old_ancs
        && cte->mdbnode.remote_descs == old_descs)
    {
        // nothing changed
        return;
    }

    // find all copies and set attributes on them
    for (struct cte *next = mdb_successor(cte);
         next && is_copy(&next->cap, &cte->cap);
         next = mdb_successor(next))
    {
#ifndef NDEBUG
        if (!(next->mdbnode.remote_copies == old_copies
              && next->mdbnode.remote_ancs == old_ancs
              && next->mdbnode.remote_descs == old_descs))
        {
            printf("WARNING: rrels out of sync! %p:%d%d%d vs %p:%d%d%d\n",
                   cte, old_copies, old_ancs, old_descs,
                   next, next->mdbnode.remote_copies,
                   next->mdbnode.remote_ancs, next->mdbnode.remote_descs);
        }
#endif
        mdb_set_cte_relations(next, relations, mask);
    }

    for (struct cte *prev = mdb_predecessor(cte);
         prev && is_copy(&prev->cap, &cte->cap);
         prev = mdb_predecessor(prev))
    {
#ifndef NDEBUG
        if (!(prev->mdbnode.remote_copies == old_copies
              && prev->mdbnode.remote_ancs == old_ancs
              && prev->mdbnode.remote_descs == old_descs))
        {
            printf("WARNING: rrels out of sync! %p:%d%d%d vs %p:%d%d%d\n",
                   cte, old_copies, old_ancs, old_descs,
                   prev, prev->mdbnode.remote_copies,
                   prev->mdbnode.remote_ancs, prev->mdbnode.remote_descs);
        }
#endif
        mdb_set_cte_relations(prev, relations, mask);
    }
}

/// Check if #cte has any descendants
bool has_descendants(struct cte *cte)
{
    assert(cte != NULL);

    struct cte *next = mdb_find_greater(&cte->cap, false);
    return next
        && get_type_root(next->cap.type) == get_type_root(cte->cap.type)
        && get_address(&next->cap) < get_address(&cte->cap) + get_size(&cte->cap);
}

bool has_ancestors(struct cte *cte) {
    return mdb_find_ancestor(cte) != NULL;
}

struct cte *mdb_find_ancestor(struct cte *cte)
{
    assert(cte != NULL);
    struct cte *result = NULL;

#if 0
    // XXX: this check should have its own predicate
    if (!get_address(&cte->cap) && !get_size(&cte->cap)) {
        return false;
    }
#endif

    result = mdb_find_less(&cte->cap, false);
    if (result
        && get_type_root(result->cap.type) == get_type_root(cte->cap.type)
        && get_address(&result->cap) + get_size(&result->cap)
           >= get_address(&cte->cap) + get_size(&cte->cap))
    {
        return result;
    }

    // cte is preceded in the ordering by a non-ancestor. This imples one of
    // two situations:
    //  1) cte has no ancestors
    //  2) cte has ancestors but also has siblings earlier in the
    //     ordering, thus the ancestor cannot have the same base
    //     address as cte.
    // If we query for the zero-length memory region at cte's start
    // address, we will not get cte itself back as the end of our query
    // is at cte's start address.
    // Similarly, we cannot get a sibling of cte that ends where cte
    // starts, as the beginning of our query is not in that sibling's
    // region.
    // Thus we must get its ancestor if present, or no cap at all.
    int find_result;
    result = NULL;
    mdb_find_range(get_type_root(cte->cap.type),
                   get_address(&cte->cap), 0,
                   MDB_RANGE_FOUND_SURROUNDING,
                   &result, &find_result);
    if (find_result != MDB_RANGE_NOT_FOUND) {
        assert(find_result == MDB_RANGE_FOUND_SURROUNDING);
        assert(result);
        assert(get_address(&result->cap) <= get_address(&cte->cap));
        assert(get_address(&result->cap) + get_size(&result->cap)
               >= get_address(&cte->cap) + get_size(&cte->cap));
    }
    else {
        assert(!result);
    }

    return result;
}

/// Checks if #cte has any copies
bool has_copies(struct cte *cte)
{
    assert(cte != NULL);

    struct cte *next = mdb_successor(cte);
    if (next && is_copy(&next->cap, &cte->cap)) {
        return true;
    }

    struct cte *prev = mdb_predecessor(cte);
    if (prev && is_copy(&prev->cap, &cte->cap)) {
        return true;
    }

    return false;
}

/**
 * \brief Returns a copy of the #cap
 */
errval_t mdb_get_copy(struct capability *cap, struct capability **ret)
{
    assert(cap != NULL);
    assert(ret != NULL);

    struct cte *cte = mdb_find_equal(cap);
    if (cte) {
        *ret = &cte->cap;
        return SYS_ERR_OK;
    }
    else {
        return SYS_ERR_NO_LOCAL_COPIES;
    }
}

bool mdb_is_sane(void)
{
    if (mdb_check_invariants() != 0) {
        return false;
    }

    // TODO: check following conditon on each element of mdb
    //if ((lvaddr_t)walk < BASE_PAGE_SIZE || walk->cap.type == ObjType_Null) {
    //    return false;
    //}

    return true;
}

/**
 * Place #dest_start in the mapping database in the appropriate location.
 *
 * Look for its relations: copies and descendants and place in accordance.
 * If no relations found, place at the top and set map_head to point to it.
 */
void set_init_mapping(struct cte *dest_start, size_t num)
{
    for (size_t i = 0; i < num; i++) {
        mdb_insert(&dest_start[i]);
    }
}

/// Remove one cap from the mapping database
void remove_mapping(struct cte *cte)
{
    mdb_remove(cte);
}
