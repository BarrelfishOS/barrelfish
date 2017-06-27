/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <collections/hash_table.h>
#include <vfs/vfs.h>

#include "ps.h"

#define HASH_INDEX_BUCKETS 6151
static collections_hash_table* ps_table = NULL;

static struct ps_entry *entries[MAX_DOMAINS];

errval_t ps_allocate(struct ps_entry *entry, domainid_t *domainid)
{
    for(domainid_t i = 1; i < MAX_DOMAINS; i++) {
        if(entries[i] == NULL) {
            entries[i] = entry;
            *domainid = i;
            entry->domain_id = i;
            return SYS_ERR_OK;
        }
    }

    return SPAWN_ERR_DOMAIN_ALLOCATE;
}

void ps_remove(domainid_t domain_id)
{
    assert(domain_id < MAX_DOMAINS);
    entries[domain_id] = NULL;
}

bool ps_exists(domainid_t domain_id)
{
    assert(domain_id < MAX_DOMAINS);
    return entries[domain_id] != NULL ? true : false;
}

struct ps_entry *ps_get(domainid_t domain_id)
{
    if(domain_id >= MAX_DOMAINS) {
        return NULL;
    }

    return entries[domain_id];
}

errval_t ps_hash_domain(struct ps_entry *entry, struct capref domain_cap)
{
    entry->domain_cap = domain_cap;

    if (ps_table == NULL) {
        collections_hash_create_with_buckets(&ps_table, HASH_INDEX_BUCKETS,
                                             NULL);
        if (ps_table == NULL) {
            return SPAWN_ERR_CREATE_DOMAIN_TABLE;
        }
    }

    uint64_t key;
    errval_t err = domain_cap_hash(entry->domain_cap, &key);
    if (err_is_fail(err)) {
        return err;
    }

    collections_hash_insert(ps_table, key, entry);

    return SYS_ERR_OK;
}

errval_t ps_get_domain(struct capref domain_cap, struct ps_entry **ret_entry,
                       uint64_t *ret_hash_key)
{
    assert(ret_entry != NULL);

    uint64_t key;
    errval_t err = domain_cap_hash(domain_cap, &key);
    if (err_is_fail(err)) {
        return err;
    }

    void *table_entry = collections_hash_find(ps_table, key);
    if (table_entry == NULL) {
        return SPAWN_ERR_DOMAIN_TABLE_FIND;
    }
    *ret_entry = (struct ps_entry*) table_entry;

    if (ret_hash_key != NULL) {
        *ret_hash_key = key;
    }

    return SYS_ERR_OK;
}

errval_t ps_release_domain(struct capref domain_cap,
                           struct ps_entry **ret_entry)
{
    assert(ret_entry != NULL);

    uint64_t key;
    errval_t err = ps_get_domain(domain_cap, ret_entry, &key);
    if (err_is_fail(err)) {
        return err;
    }

    collections_hash_delete(ps_table, key);

    return SYS_ERR_OK;
}
