/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBMDB_TYPES_H
#define LIBMDB_TYPES_H

struct cte;

typedef uint8_t mdb_root_t;
typedef uint8_t mdb_level_t;

/**
 * \brief A mapping database node.
 */
struct mdbnode {
    struct cte *left, *right;
    genpaddr_t end;
    mdb_root_t end_root;
    mdb_level_t level;
    bool remote_copies:1, remote_ancs:1, remote_descs:1;
    bool locked:1, in_delete:1;
    coreid_t owner;
};

#ifndef IN_KERNEL
// XXX: When compiling for userland, the cte definition in
// kernel/include/capabilities.h is not available, so we define a substitute
// cte type here.
#include <barrelfish/barrelfish.h>
struct cte {
    struct capability cap;
    struct mdbnode mdbnode;
    char padding[(1<<OBJBITS_CTE)-sizeof(struct capability)-sizeof(struct mdbnode)];
};
#endif

#endif // LIBMDB_TYPES_H
