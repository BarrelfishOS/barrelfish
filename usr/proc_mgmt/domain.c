/*
 * \brief Domain internals for the process manager.
 *
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <collections/hash_table.h>
#include <if/spawn_defs.h>

#include "domain.h"
#include "spawnd_state.h"

#define HASH_INDEX_BUCKETS 6151
static collections_hash_table* domain_table = NULL;

errval_t domain_new(struct capref domain_cap, struct domain_entry **ret_entry)
{
    assert(ret_entry != NULL);

    struct domain_entry *entry = (struct domain_entry*) malloc(
            sizeof(struct domain_entry));
    if (entry == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    entry->domain_cap = domain_cap;
    entry->status = DOMAIN_STATUS_NIL;
    memset(entry->spawnds, 0, sizeof(entry->spawnds));
    entry->num_spawnds_running = 0;
    entry->waiters = NULL;

    if (domain_table == NULL) {
        collections_hash_create_with_buckets(&domain_table, HASH_INDEX_BUCKETS,
                                             NULL);
        if (domain_table == NULL) {
            return PROC_MGMT_ERR_CREATE_DOMAIN_TABLE;
        }
    }

    uint64_t key;
    errval_t err = domain_cap_hash(entry->domain_cap, &key);
    if (err_is_fail(err)) {
        return err;
    }

    collections_hash_insert(domain_table, key, entry);

    *ret_entry = entry;

    return SYS_ERR_OK;
}

errval_t domain_get_by_cap(struct capref domain_cap,
                           struct domain_entry **ret_entry)
{
    assert(ret_entry != NULL);

    uint64_t key;
    errval_t err = domain_cap_hash(domain_cap, &key);
    if (err_is_fail(err)) {
        return err;
    }

    void *table_entry = collections_hash_find(domain_table, key);
    if (table_entry == NULL) {
        return PROC_MGMT_ERR_DOMAIN_TABLE_FIND;
    }
    *ret_entry = (struct domain_entry*) table_entry;

    return SYS_ERR_OK;
}

void domain_run_on_core(struct domain_entry *entry, coreid_t core_id)
{
    assert(entry != NULL);
    assert(core_id < MAX_COREID);
    assert(entry->status == DOMAIN_STATUS_NIL ||
           entry->status == DOMAIN_STATUS_RUNNING);

    entry->status = DOMAIN_STATUS_RUNNING;

    entry->spawnds[core_id] = spawnd_state_get(core_id);
    ++entry->num_spawnds_running;
}

errval_t domain_spawn(struct capref domain_cap, coreid_t core_id)
{
    struct domain_entry *entry = NULL;
    errval_t err = domain_new(domain_cap, &entry);
    if (err_is_fail(err)) {
        if (entry != NULL) {
            free(entry);
        }
        return err;
    }

    domain_run_on_core(entry, core_id);

    return SYS_ERR_OK;
}

errval_t domain_can_span(struct capref domain_cap, coreid_t core_id)
{
    struct domain_entry *entry = NULL;
    errval_t err = domain_get_by_cap(domain_cap, &entry);
    if (err_is_fail(err)) {
        return err;
    }

    assert(entry != NULL);
    if (entry->status != DOMAIN_STATUS_RUNNING) {
        return PROC_MGMT_ERR_DOMAIN_NOT_RUNNING;
    }

    if (entry->spawnds[core_id] != NULL) {
        // TODO(razvan): Maybe we want to allow the same domain to span multiple
        // dispatchers onto the same core?
        return PROC_MGMT_ERR_ALREADY_SPANNED;
    }

    return SYS_ERR_OK;
}

errval_t domain_span(struct capref domain_cap, coreid_t core_id)
{
    struct domain_entry *entry = NULL;
    errval_t err = domain_get_by_cap(domain_cap, &entry);
    if (err_is_fail(err)) {
        return err;
    }
    assert(entry != NULL);

    domain_run_on_core(entry, core_id);

    return SYS_ERR_OK;
}
