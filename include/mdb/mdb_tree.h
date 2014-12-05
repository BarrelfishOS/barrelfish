/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBMDB_MDB_TREE_H
#define LIBMDB_MDB_TREE_H

#include <sys/cdefs.h>

#include <errors/errno.h>
#include <barrelfish/types.h>
#include <mdb/types.h>

__BEGIN_DECLS

struct capability;
struct cte;

#define mdb_invariants(f) \
    /* All checked invariants hold*/\
    f(MDB_INVARIANT_OK) \
    /* A node with level > 0 must have both children*/\
    f(MDB_INVARIANT_BOTHCHILDREN) \
    /* The level of a node's left child must be lt the node's level*/\
    f(MDB_INVARIANT_LEFT_LEVEL_LESS) \
    /* The level of a node's right child must be leq the node's level*/\
    f(MDB_INVARIANT_RIGHT_LEVEL_LEQ) \
    /* The level of a node's right grandchildren must bt lt the node's level*/\
    f(MDB_INVARIANT_RIGHTRIGHT_LEVEL_LESS) \
    f(MDB_INVARIANT_RIGHTLEFT_LEVEL_LESS) \
    /* The node's "end" value must be the maximum of the subtree's ends*/\
    f(MDB_INVARIANT_END_IS_MAX) \
    /* The left child of a node must be earlier in the ordering*/\
    f(MDB_INVARIANT_LEFT_SMALLER) \
    /* The right child of a node must be later in the ordering*/\
    f(MDB_INVARIANT_RIGHT_GREATER)

#define f_enum(x) x,
enum mdb_invariant {
    mdb_invariants(f_enum)
};
#undef f_enum

#define f_str(x) #x,
static const char *mdb_invariant_str[] = { mdb_invariants(f_str) };
#undef f_str

#undef mdb_invariants

static inline const char *mdb_invariant_to_str(enum mdb_invariant i)
{
    return mdb_invariant_str[i];
}

enum {
    // No cap was found covering the specified region
    MDB_RANGE_NOT_FOUND = 0,
    // A cap was found covering at least the entire specified region
    MDB_RANGE_FOUND_SURROUNDING = 1,
    // A cap was found that is inside the specified region
    MDB_RANGE_FOUND_INNER = 2,
    // A cap was found that overlaps with one of the specified region's ends
    MDB_RANGE_FOUND_PARTIAL = 3,
};

#if IN_KERNEL
// kcb defined else-where
struct kcb;
#else
// create mini kcb that can be used to set root node in user space mdb
struct kcb {
    lvaddr_t mdb_root;
};
#endif

// Restore mdb state by passing in the address of the root node
// requires: mdb not initialized
// ensures: mdb_check_invariants() && mdb_is_sane()
errval_t mdb_init(struct kcb *k);

// Print the specified subtree
void mdb_dump(struct cte *cte, int indent);
// Print the complete tree
void mdb_dump_all_the_things(void);
// Check that the invariants hold. The return value indicates the first issue
// that was encountered.
int mdb_check_invariants(void);

// Insert a cap into the tree. An error (MDB_DUPLICATE_ENTRY) is returned iff
// the cap is already present in the tree.
errval_t mdb_insert(struct cte *new_node);
// Remove a cap from the tree. An error (MDB_ENTRY_NOTFOUND) is returned iff
// the cap is not present in the tree.
errval_t mdb_remove(struct cte *node);

struct cte *mdb_predecessor(struct cte *current);
struct cte *mdb_successor(struct cte *current);

// Find a cap in the tree that compares equal to the given cap. Returns NULL if
// no such cap is found.
struct cte *mdb_find_equal(struct capability *cap);
// Find the greatest cap in the tree that is earlier in the ordering. Returns
// NULL if no matching cap is found.
// @param equal_ok If true, will return a copy of the passed cap if it is
//        present.
struct cte *mdb_find_less(struct capability *cap, bool equal_ok);
// Find the smallest cap in the tree that is later in the ordering. Returns
// NULL if no matching cap is found.
// @param equal_ok If true, will return a copy of the passed cap if it is
//        present.
struct cte *mdb_find_greater(struct capability *cap, bool equal_ok);

// Find a cap in the given range.
// @param root indicates which type tree root to search through (usually
//        `get_type_root(ObjType_PhysAddr)`).
// @param max_result Indicates the maximum outcome (not found, surrounding...)
//        for which a cap should be returned. If an result is found that is
//        greater than max_result, this function returns immediately with the
//        given result and NULL cte. The semantics/uses for individual
//        max_result values are thus:
//        - MDB_RANGE_NOT_FOUND: A simple test whether the given region is
//          covered by a cap.
//        - MDB_RANGE_FOUND_SURROUNDING: Can be used to check if a region can
//          be retyped.
//        - MDB_RANGE_FOUND_INNER: Useful for iterating through *immediate*
//          descendants.
errval_t mdb_find_range(mdb_root_t root, genpaddr_t address, gensize_t size,
                        int max_result, struct cte **ret_node, int *result);

errval_t mdb_find_cap_for_address(genpaddr_t address, struct cte **ret_node);

bool mdb_reachable(struct cte *cte);

__END_DECLS

#endif // LIBMDB_MDB_TREE_H
