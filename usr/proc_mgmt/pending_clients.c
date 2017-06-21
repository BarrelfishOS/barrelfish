/*
 * \brief Client handling internals for the process manager.
 *
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <collections/hash_table.h>

#include "domain.h"
#include "pending_clients.h"

static collections_hash_table* pending_clients_table = NULL;

errval_t pending_clients_add(struct capref domain_cap,
                             struct proc_mgmt_binding *b, enum ClientType type,
                             coreid_t core_id)
{
    if (pending_clients_table == NULL) {
        collections_hash_create_with_buckets(&pending_clients_table,
                                             HASH_INDEX_BUCKETS, NULL);
        if (pending_clients_table == NULL) {
            return PROC_MGMT_ERR_CREATE_CLIENTS_TABLE;
        }
    }

    uint64_t key;
    errval_t err = domain_cap_hash(domain_cap, &key);
    if (err_is_fail(err)) {
        return err;
    }

    struct pending_client *client = (struct pending_client*) malloc(
            sizeof(struct pending_client));
    client->b = b;
    client->core_id = core_id;
    client->type = type;
    collections_hash_insert(pending_clients_table, key, client);

    return SYS_ERR_OK;
}

errval_t pending_clients_release(struct capref domain_cap,
                                        struct pending_client **ret_cl)
{
    uint64_t key;
    errval_t err = domain_cap_hash(domain_cap, &key);
    if (err_is_fail(err)) {
        return err;
    }

    void *table_entry = collections_hash_find(pending_clients_table, key);
    if (table_entry == NULL) {
        return PROC_MGMT_ERR_CLIENTS_TABLE_FIND;
    }
    if (ret_cl != NULL) {
        *ret_cl = (struct pending_client*) table_entry;
    }

    collections_hash_delete(pending_clients_table, key);

    return SYS_ERR_OK;
}
