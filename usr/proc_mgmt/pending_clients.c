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

static collections_hash_table *spawn_table = NULL;
static collections_hash_table *spawn_with_caps_table = NULL;
static collections_hash_table *span_table = NULL;
static collections_hash_table *kill_table = NULL;
static collections_hash_table *exit_table = NULL;
static collections_hash_table *cleanup_table = NULL;

errval_t pending_clients_add(struct capref domain_cap,
                             struct proc_mgmt_binding *b, enum ClientType type,
                             coreid_t core_id)
{
    collections_hash_table **table;
    switch (type) {
        case ClientType_Spawn:
            table = &spawn_table;
            break;
        case ClientType_SpawnWithCaps:
            table = &spawn_with_caps_table;
            break;
        case ClientType_Span:
            table = &span_table;
            break;
        case ClientType_Kill:
            table = &kill_table;
            break;
        case ClientType_Exit:
            table = &exit_table;
            break;
        case ClientType_Cleanup:
            table = &cleanup_table;
            break;
        default:
            USER_PANIC("Unhandled client type %d\n", type);
    }

    if (*table == NULL) {
        collections_hash_create_with_buckets(table, HASH_INDEX_BUCKETS, NULL);
        if (*table == NULL) {
            return PROC_MGMT_ERR_CREATE_CLIENTS_TABLE;
        }
    }

    uint64_t key;
    errval_t err = domain_cap_hash(domain_cap, &key);
    if (err_is_fail(err)) {
        return err;
    }

    struct pending_client *cl = (struct pending_client*) malloc(
            sizeof(struct pending_client));
    cl->b = b;
    cl->domain_cap = domain_cap;
    cl->core_id = core_id;
    cl->type = type;
    cl->next = NULL;

    if (type == ClientType_Kill) {
        // Special case: multiple clients might have issued a kill for some
        // domain. Need to chain them together.
        void *entry = collections_hash_find(*table, key);
        if (entry != NULL) {
            struct pending_client* old = (struct pending_client*) entry;
            collections_hash_delete(*table, key);
            cl->next = old;
        }
    }
    
    collections_hash_insert(*table, key, cl);

    return SYS_ERR_OK;
}

errval_t pending_clients_release(struct capref domain_cap, enum ClientType type,
                                 struct pending_client **ret_cl)
{
    uint64_t key;
    errval_t err = domain_cap_hash(domain_cap, &key);
    if (err_is_fail(err)) {
        return err;
    }

    collections_hash_table **table;
    switch (type) {
        case ClientType_Spawn:
            table = &spawn_table;
            break;
        case ClientType_SpawnWithCaps:
            table = &spawn_with_caps_table;
            break;
        case ClientType_Span:
            table = &span_table;
            break;
        case ClientType_Kill:
            table = &kill_table;
            break;
        case ClientType_Exit:
            table = &exit_table;
            break;
        case ClientType_Cleanup:
            table = &cleanup_table;
            break;
        default:
            USER_PANIC("Unhandled client type %d\n", type);
    }

    void *entry = collections_hash_find(*table, key);
    if (entry == NULL) {
        return PROC_MGMT_ERR_CLIENTS_TABLE_FIND;
    }
    struct pending_client *cl = (struct pending_client*) entry;
    if (ret_cl != NULL) {
        *ret_cl = cl;
    } else {
        free(cl);
    }

    collections_hash_delete(*table, key);

    return SYS_ERR_OK;
}

errval_t pending_clients_release_one(struct capref domain_cap,
                                     enum ClientType type,
                                     struct proc_mgmt_binding *b,
                                     struct pending_client **ret_cl)
{
    uint64_t key;
    errval_t err = domain_cap_hash(domain_cap, &key);
    if (err_is_fail(err)) {
        return err;
    }

    collections_hash_table **table;
    switch (type) {
        case ClientType_Spawn:
            table = &spawn_table;
            break;
        case ClientType_SpawnWithCaps:
            table = &spawn_with_caps_table;
            break;
        case ClientType_Span:
            table = &span_table;
            break;
        case ClientType_Kill:
            table = &kill_table;
            break;
        case ClientType_Exit:
            table = &exit_table;
            break;
        case ClientType_Cleanup:
            table = &cleanup_table;
            break;
        default:
            USER_PANIC("Unhandled client type %d\n", type);
    }

    void *entry = collections_hash_find(*table, key);
    if (entry == NULL) {
        return PROC_MGMT_ERR_CLIENTS_TABLE_FIND;
    }
    struct pending_client *cl = (struct pending_client*) entry;
    if (cl->b == b) {
        struct pending_client *tmp = cl;
        cl = cl->next;
        if (ret_cl != NULL) {
            *ret_cl = tmp;
        } else {
            free(tmp);
        }
    } else {
        while (cl->next != NULL) {
            if (cl->next->b == b) {
                struct pending_client *tmp = cl->next;
                cl->next = cl->next->next;
                if (ret_cl != NULL) {
                    *ret_cl = tmp;
                } else {
                    free(tmp);
                }
                break;
            }
            cl = cl->next;
        }
    }

    collections_hash_delete(*table, key);
    if (cl != NULL) {
        collections_hash_insert(*table, key, cl);
    }

    return SYS_ERR_OK;
}
