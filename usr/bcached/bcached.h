/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BCACHED_H
#define BCACHED_H

#include <barrelfish/barrelfish.h>
#include <barrelfish/bulk_transfer.h>

struct bcache_state {
    struct bulk_transfer bt;
};

extern struct capref cache_memory;
extern size_t cache_size, block_size;
extern void *cache_pool;

errval_t start_service(void);
bool cache_lookup(char *key, size_t key_len, uintptr_t *index,
                  uintptr_t *length);
uintptr_t cache_allocate(char *key, size_t key_len);
void cache_update(uintptr_t index, uintptr_t length);

void print_stats(void);

#endif
