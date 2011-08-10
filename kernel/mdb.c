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
#include <kernel.h>
#include <string.h>
#include <barrelfish_kpi/syscalls.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <dispatch.h>
#include <paging_kernel_arch.h>
#include <mdb.h>

void set_cap_remote(struct cte *cte, bool is_remote) 
{
    assert(cte != NULL);
    assert(cte->mdbnode.next != NULL);
    assert(cte->mdbnode.prev != NULL);

    cte->mdbnode.remote_relations = is_remote;   // set is_remote on this cte

    struct cte *next = cte->mdbnode.next;
    struct cte *prev = cte->mdbnode.prev;

    // find all relations and set is_remote on them
    // XXX: We shouldn't need to do the ancestor check forwards and backwards,
    // but I'll stay consistent with the rest of the code in this file
    while(next != cte) {
        if (is_copy(&next->cap, &cte->cap) || is_ancestor(&next->cap, &cte->cap)
            || is_ancestor(&cte->cap, &next->cap)) {
            next->mdbnode.remote_relations = is_remote;
        } else {
            break;
        }
        next = next->mdbnode.next;
    }

    while(prev != cte) {
        if (is_copy(&prev->cap, &cte->cap) || is_ancestor(&prev->cap, &cte->cap)
            || is_ancestor(&cte->cap, &prev->cap)) {
            prev->mdbnode.remote_relations = is_remote;
        } else {
            break;
        }
        prev = prev->mdbnode.prev;
    }
}

bool is_cap_remote(struct cte *cte) 
{
    return cte->mdbnode.remote_relations;
}

/// Check if #cte has any descendants
/// XXX TODO - check for descendents on remote cores
bool has_descendants(struct cte *cte)
{
    assert(cte != NULL);
    assert(cte->mdbnode.next != NULL);
    assert(cte->mdbnode.prev != NULL);

    struct cte *next = cte->mdbnode.next;
    struct cte *prev = cte->mdbnode.prev;

    while(next != cte) {
        if (is_ancestor(&next->cap, &cte->cap)) {
            return true;
        }
        if (!is_copy(&next->cap, &cte->cap)) {
            break;
        }
        next = next->mdbnode.next;
    }

    // SP: Why is walking backwards needed when looking for descendants?
    while(prev != cte) {
        if (is_ancestor(&prev->cap, &cte->cap)) {
            return true;
        }
        if (!is_copy(&prev->cap, &cte->cap)) {
            break;
        }
        prev = prev->mdbnode.prev;
    }

    return false;
}

/// Check if #cte has any ancestors
/// XXX TODO - check for descendents on remote cores
bool has_ancestors(struct cte *cte)
{
    assert(cte != NULL);
    assert(cte->mdbnode.next != NULL);
    assert(cte->mdbnode.prev != NULL);

    struct cte *next = cte->mdbnode.next;
    struct cte *prev = cte->mdbnode.prev;

    // XXX: Is walking forward needed here? (doing it because we walk
    // backwards in has_descendants())
    while(next != cte) {
        if (is_ancestor(&cte->cap, &next->cap)) {
            return true;
        }
        if (!is_copy(&next->cap, &cte->cap)) {
            break;
        }
        next = next->mdbnode.next;
    }

    while(prev != cte) {
        if (is_ancestor(&cte->cap, &prev->cap)) {
            return true;
        }
        if (!is_copy(&prev->cap, &cte->cap)) {
            break;
        }
        prev = prev->mdbnode.prev;
    }

    return false;
}

/// Checks if #cte has any copies
bool has_copies(struct cte *cte)
{
    assert(cte != NULL);
    assert(cte->mdbnode.next != NULL);
    assert(cte->mdbnode.prev != NULL);

    struct cte *next = cte->mdbnode.next;
    struct cte *prev = cte->mdbnode.prev;

    while(next != cte) {
        if (is_copy(&next->cap, &cte->cap)) {
            return true;
        }
        if (!is_ancestor(&next->cap, &cte->cap)) {
            break;
        }
        next = next->mdbnode.next;
    }

    while(prev != cte) {
        if (is_copy(&prev->cap, &cte->cap)) {
            return true;
        }
        if (!is_ancestor(&prev->cap, &cte->cap)) {
            break;
        }
        prev = prev->mdbnode.prev;
    }

    return false;
}

/**
 * Insert #dest_start after #src in the mapping database.
 *
 * Can handle insertion of multiple capabilities by setting #num.
 * Will also set MDB properties, like remote_relations, accordingly.
 */
void insert_after(struct cte *dest_start, struct cte *src, size_t num)
{
    assert(dest_start != NULL);
    assert(src != NULL);
    assert(src->mdbnode.next != NULL);
    assert(src->mdbnode.prev != NULL);

    src->mdbnode.next->mdbnode.prev = &dest_start[num - 1];
    dest_start[num - 1].mdbnode.next    = src->mdbnode.next;

    src->mdbnode.next               = &dest_start[0];
    dest_start[0].mdbnode.prev      = src;

    dest_start[num - 1].mdbnode.remote_relations = src->mdbnode.remote_relations;
    dest_start[0].mdbnode.remote_relations = src->mdbnode.remote_relations;

    for(size_t i = 0; i < (num - 1); i++) {
        dest_start[i + 1].mdbnode.prev = &dest_start[i];
        dest_start[i].mdbnode.next     = &dest_start[i + 1];
        dest_start[i].mdbnode.remote_relations = src->mdbnode.remote_relations;
    }
}

/**
 * Insert #dest_start before #src in the mapping database.
 *
 * Can handle insertion of multiple capabilities by setting #num.
 */
static void insert_before(struct cte *dest_start, struct cte *src, size_t num)
{
    assert(dest_start != NULL);
    assert(src != NULL);

    src->mdbnode.prev->mdbnode.next = &dest_start[0];
    dest_start[0].mdbnode.prev = src->mdbnode.prev;

    src->mdbnode.prev = &dest_start[num -1];
    dest_start[num - 1].mdbnode.next = src;

    dest_start[0].mdbnode.remote_relations = src->mdbnode.remote_relations;
    dest_start[num - 1].mdbnode.remote_relations = src->mdbnode.remote_relations;

    for(size_t i = 0; i < (num - 1); i++) {
        dest_start[i + 1].mdbnode.prev = &dest_start[i];
        dest_start[i].mdbnode.next     = &dest_start[i + 1];
        dest_start[i].mdbnode.remote_relations = src->mdbnode.remote_relations;
    }
}

/**
 * The head of the mapping database on the core. Should always point to a cap
 * that has no ancestors.
 */
static struct cte *map_head = NULL;

/**
 * \brief Returns a copy of the #cap
 */
errval_t mdb_get_copy(struct capability *cap, struct capability **ret)
{
    assert(cap != NULL);
    assert(ret != NULL);
    assert(map_head != NULL);

    struct cte *walk = map_head;
    do {
        if(is_copy(&walk->cap, cap)) {
            *ret = &walk->cap;
            return SYS_ERR_OK;
        }
        walk = walk->mdbnode.next;
    } while(walk != map_head);

    return SYS_ERR_NO_LOCAL_COPIES;
}

bool mdb_is_sane(void)
{
    assert(map_head != NULL);

    struct cte *walk = map_head;
    do {
        if ((lvaddr_t)walk < BASE_PAGE_SIZE || walk->cap.type == ObjType_Null) {
            return false;
        }
        walk = walk->mdbnode.next;
    } while (walk != map_head);
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
    /* Initialize the head if first entry */
    if (map_head == NULL) {
        // Set map_head
        map_head = dest_start;
        // Add all the capabilities in sequence
        dest_start->mdbnode.prev = &dest_start[num - 1];
        dest_start[num - 1].mdbnode.next = dest_start;
        for(size_t i = 0; i < (num - 1); i++) {
            dest_start[i].mdbnode.next = &dest_start[(i + 1) % num];
            dest_start[(i + 1) % num].mdbnode.prev = &dest_start[i];
        }
        return;
    }

    struct cte *walk = map_head;
    struct cte *ancestor = NULL;

    /* Loop to find the appropriate place to insert */
    do {
        if (is_copy(&dest_start->cap, &walk->cap)) {
            /* A copy if found, insert after it */
            assert(num == 1);
            insert_after(dest_start, walk, num);
            return;
        } else if (is_ancestor(&dest_start->cap, &walk->cap)) {
            /* Keep looking for the closest ancestor */
            ancestor = walk;
        } else if (ancestor != NULL) {
            /* Insert after closest ancestor */
            insert_after(dest_start, ancestor, num);
            return;
        }
        // Increment
        walk = walk->mdbnode.next;
    } while (walk != map_head);

    /* No relations found, insert before map_head and update map_head */
    insert_before(dest_start, map_head, num);
    map_head = dest_start;
}

/// Remove one cap from the mapping database
void remove_mapping(struct cte *cte)
{
    /* Paramter checking */
    assert(map_head != NULL);
    assert(cte      != NULL);
    assert(cte->mdbnode.prev != NULL);
    assert(cte->mdbnode.next != NULL);

    cte->mdbnode.remote_relations = false;   // insert as non-remote

    /* The only cap in the database, empty it */
    if (cte->mdbnode.next == cte) {
        assert(map_head == cte);
        assert(cte->mdbnode.prev == cte);
        map_head = NULL;
    }

    /* Update map_head if the same as cte */
    if (map_head == cte) {
        /* The previous cap maybe a copy or else pick the next */
        struct cte* head_prev = map_head->mdbnode.prev;
        if (is_copy(&head_prev->cap, &map_head->cap)) {
            map_head = head_prev;
        } else {
            map_head = map_head->mdbnode.next;
        }
    }

    /* Link up the prev and next caps */
    cte->mdbnode.prev->mdbnode.next = cte->mdbnode.next;
    cte->mdbnode.next->mdbnode.prev = cte->mdbnode.prev;

    /* Nullify the mdb on the cte */
    cte->mdbnode.prev = cte->mdbnode.next = NULL;
}

/// Recursively remove the cnode and the caps it contains from the mapping db.
void mdb_remove_recursively(struct cte *cte)
{
    // Parameter checking
    assert(cte != NULL);

    // Return if cte already removed
    if ((cte->mdbnode.prev == NULL) && (cte->mdbnode.prev == NULL)) {
        return;
    }
    assert(cte->mdbnode.prev != NULL);
    assert(cte->mdbnode.next != NULL);

    // Remove this cte
    remove_mapping(cte);

    // Remove specific fields from the dcb
    if (cte->cap.type == ObjType_Dispatcher) {
        mdb_remove_recursively(&cte->cap.u.dispatcher.dcb->cspace);
        remove_mapping(&cte->cap.u.dispatcher.dcb->disp_cte);
        return;
    }

    // Do not recurse if not type CNode
    if (cte->cap.type != ObjType_CNode) {
        return;
    }

    // Number of slots in the cnode
    uint64_t max_slots = 1UL << cte->cap.u.cnode.bits;

    // Remove each cap stored in the cnode
    for(uint64_t slot_no = 0; slot_no < max_slots; slot_no++) {
        struct cte *cte_in_cnode = caps_locate_slot(cte->cap.u.cnode.cnode,
                                                    slot_no);
        if (cte_in_cnode->cap.type == ObjType_Null) {
            continue;
        }
        mdb_remove_recursively(cte_in_cnode);
    }
}
