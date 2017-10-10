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
#include <collections/list.h>
#include <if/spawn_defs.h>

#include "domain.h"
#include "spawnd_state.h"

#define HASH_INDEX_BUCKETS 6151
static collections_hash_table* domain_table = NULL;

#define DOMAIN_CAP_REFILL_COUNT L2_CNODE_SLOTS//1
static struct domain_cap_node *domain_cap_list = NULL;
static uint32_t free_domain_caps = 0;
static domainid_t domain_alloc = 0;

inline bool domain_should_refill_caps(void) {
    return free_domain_caps == 0;
}

/**
 * \brief Allocates a new L2 cnode and fills it with domain capabilities.
 */
errval_t domain_prealloc_caps(void)
{
    struct capref new_cnode_cap;
    struct cnoderef new_cnode;
    errval_t err = cnode_create_l2(&new_cnode_cap, &new_cnode);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cnode_create_l2");
        return err_push(err, PROC_MGMT_ERR_CREATE_DOMAIN_CAP);
    }

    struct capref cap_iter = {
        .cnode = new_cnode,
        .slot = 0
    };
    err = cap_retype(cap_iter, cap_procmng, 0, ObjType_Domain, 0,
                     DOMAIN_CAP_REFILL_COUNT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cap_retype");
        return err_push(err, PROC_MGMT_ERR_CREATE_DOMAIN_CAP);
    }

    for (cap_iter.slot = 0; cap_iter.slot < DOMAIN_CAP_REFILL_COUNT; ++cap_iter.slot) {
        struct domain_cap_node *node = (struct domain_cap_node*) malloc(
                sizeof(struct domain_cap_node));
        node->domain_cap = cap_iter;

        err = domain_cap_hash(node->domain_cap, &node->hash);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "domain_cap_hash");
            return err_push(err, PROC_MGMT_ERR_CREATE_DOMAIN_CAP);
        }

        node->next = domain_cap_list;
        domain_cap_list = node;
        ++free_domain_caps;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Returns the next node in the list of available domain caps.
 */
struct domain_cap_node *next_cap_node(void)
{
    assert(domain_cap_list != NULL);
    assert(free_domain_caps > 0);
    
    struct domain_cap_node *tmp = domain_cap_list;
    domain_cap_list = domain_cap_list->next;
    --free_domain_caps;
    
    return tmp;
}

/**
 * \brief Creates and returns a new domain entry.
 *
 * \param cap_node  preallocated domain cap node.
 * \param ret_entry returned domain entry, must be passed in non-NULL.
 */
errval_t domain_new(struct domain_cap_node *cap_node, const char* argbuf, 
                    size_t argbytes, struct domain_entry **ret_entry)
{
    assert(ret_entry != NULL);

    struct domain_entry *entry = (struct domain_entry*) malloc(
            sizeof(struct domain_entry));
    if (entry == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    entry->cap_node = cap_node;
    entry->status = DOMAIN_STATUS_NIL;
    memset(entry->spawnds, 0, sizeof(entry->spawnds));
    entry->num_spawnds_running = 0;
    entry->num_spawnds_resources = 0;
    entry->waiters = NULL;

    entry->domainid = domain_alloc;
    domain_alloc++;

    entry->argbuf = memdup(argbuf, argbytes);
    entry->argbytes = argbytes;

    if (domain_table == NULL) {
        collections_hash_create_with_buckets(&domain_table, HASH_INDEX_BUCKETS,
                                             NULL);
        if (domain_table == NULL) {
            return PROC_MGMT_ERR_CREATE_DOMAIN_TABLE;
        }
    }

    collections_hash_insert(domain_table, cap_node->hash, entry);

    *ret_entry = entry;

    return SYS_ERR_OK;
}

/**
 * \brief Returns the domain entry associated with the given domain cap.
 *
 * \param domain_cap identifying cap for which to look up the domain entry.
 * \param returned domain entry, must be passed in non-NULL.
 */
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

errval_t domain_get_by_id(domainid_t domain_id,
                          struct domain_entry **ret_entry)
{
    // XXX slow since we traverse the whole hash map 
    collections_hash_traverse_start(domain_table);

    uint64_t key;
    void* ele = collections_hash_traverse_next(domain_table, &key);
    
    // get all domain ids and store in list
    // XXX traverse whole hash table since it seems to not
    // reset the internal lists when resetting the traversal of the hash table
    while (ele != NULL) {
        struct domain_entry *entry = (struct domain_entry*) ele;
    
        if (entry->domainid == domain_id) {
            *ret_entry = entry;
        }

        ele = collections_hash_traverse_next(domain_table, &key);
    }

    collections_hash_traverse_end(domain_table);
    if (ret_entry == NULL) {
        return PROC_MGMT_ERR_DOMAIN_TABLE_FIND;
    } else {
        return SYS_ERR_OK;
    }
}

/**
 * \brief Adds a new core to the list of cores where the given domain runs.
 *
 * \param entry   domain entry to add a new core for.
 * \param core_id new core running a dispatcher for the domain.
 */
void domain_run_on_core(struct domain_entry *entry, coreid_t core_id)
{
    assert(entry != NULL);
    assert(core_id < MAX_COREID);
    assert(entry->status == DOMAIN_STATUS_NIL ||
           entry->status == DOMAIN_STATUS_RUNNING);

    entry->status = DOMAIN_STATUS_RUNNING;

    entry->spawnds[core_id] = spawnd_state_get(core_id);
    ++entry->num_spawnds_running;
    ++entry->num_spawnds_resources;
}


/**
 * \brief Creates a new domain entry for the given cap node and core.
 *
 * \param cap_node preallocated capability node for the new domain.
 * \param core_id  core that runs the new domain.
 */
errval_t domain_spawn(struct domain_cap_node *cap_node, coreid_t core_id,
                      const char* argbuf, size_t argbytes)
{
    struct domain_entry *entry = NULL;
    errval_t err = domain_new(cap_node, argbuf, argbytes, &entry);
    if (err_is_fail(err)) {
        if (entry != NULL) {
            free(entry);
        }
        return err;
    }

    domain_run_on_core(entry, core_id);

    return SYS_ERR_OK;
}

/**
 * \brief Marks that the domain identified by the given cap spans a new core.
 *
 * \param domain_cap identifying capability for the spanning domain.
 * \param core_id    new core which the domain spans.
 */
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

void domain_get_all_ids(domainid_t** domains, size_t* len)
{
    if (domain_table == NULL) {
        *len = 0;
        return;
    }

    collections_hash_traverse_start(domain_table);
    
    collections_listnode* start;
    collections_list_create(&start, NULL);

    uint64_t key;
    void* ele = collections_hash_traverse_next(domain_table, &key);
 
    // get all domain ids and store in list
    while (ele != NULL) {
        struct domain_entry *entry = (struct domain_entry*) ele;
    
        if (entry->status != DOMAIN_STATUS_CLEANED) {
            collections_list_insert(start, &entry->domainid);
        }

        ele = collections_hash_traverse_next(domain_table, &key);
    }

    domainid_t* doms = (domainid_t*) calloc(1, sizeof(domainid_t)*
                                             collections_list_size(start));

    *len = collections_list_size(start);
    // copy domain ids
    for (int i = 0; i < collections_list_size(start); i++) {
        doms[i] = *((domainid_t*) collections_list_get_ith_item(start, i));
    }

    collections_list_release(start);        
    *domains = doms;
    collections_hash_traverse_end(domain_table);
}
