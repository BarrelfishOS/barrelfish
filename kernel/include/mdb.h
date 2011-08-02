/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MDB_H
#define MDB_H

void set_init_mapping(struct cte *dest_start, size_t num);
bool has_descendants(struct cte *cte);
bool has_ancestors(struct cte *cte);
bool has_copies(struct cte *cte);
void insert_after(struct cte *dest_start, struct cte *src, size_t num);
void remove_mapping(struct cte *cte);
errval_t mdb_get_copy(struct capability *cap, struct capability **ret);
bool mdb_is_sane(void);
void set_cap_remote(struct cte *cte, bool is_remote);
bool is_cap_remote(struct cte *cte);

#endif
