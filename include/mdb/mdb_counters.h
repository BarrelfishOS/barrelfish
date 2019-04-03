/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBMDB_MDB_COUNTERS_H
#define LIBMDB_MDB_COUNTERS_H

#include <stdint.h>

// from mdb.c
extern uint64_t mdb_has_descendants_count;
extern uint64_t mdb_find_ancestor_count;
extern uint64_t mdb_has_copies_count;

// from mdb_tree.c
extern uint64_t mdb_insert_count;
extern uint64_t mdb_remove_count;
extern uint64_t mdb_find_equal_count;
extern uint64_t mdb_find_less_count;
extern uint64_t mdb_find_greater_count;
extern uint64_t mdb_predecessor_count;
extern uint64_t mdb_successor_count;
extern uint64_t mdb_find_range_count;
extern uint64_t mdb_find_cap_for_address_count;

#endif // LIBMDB_MDB_COUNTERS_H
