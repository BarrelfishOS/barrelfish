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

#define DOMAIN_CAP_REFILL_COUNT 128
static struct domain_cap_node *domain_cap_list = NULL;
static uint32_t free_domain_caps = 0;

#define PROC_MGMT_BENCH 1
#define PROC_MGMT_BENCH_MIN_RUNS 100

#ifdef PROC_MGMT_BENCH
#include <bench/bench.h>

static inline cycles_t calculate_time(cycles_t tsc_start, cycles_t tsc_end)
{
    cycles_t result;
    if (tsc_end < tsc_start) {
        result = (LONG_MAX - tsc_start) + tsc_end - bench_tscoverhead();
    } else {
        result = (tsc_end - tsc_start - bench_tscoverhead());
    }
    return result;
}

static bench_ctl_t *hash_ctl;
static uint64_t tscperus;
#endif

inline bool domain_should_refill_caps(void) {
    return free_domain_caps == 0;
}

errval_t domain_prealloc_caps(void)
{
    for (size_t i = 0; i < DOMAIN_CAP_REFILL_COUNT; ++i) {
        struct domain_cap_node *node = (struct domain_cap_node*) malloc(
                sizeof(struct domain_cap_node));
        errval_t err = slot_alloc(&node->domain_cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "slot_alloc domain_cap");
            return err_push(err, PROC_MGMT_ERR_CREATE_DOMAIN_CAP);
        }

        err = cap_retype(node->domain_cap, cap_procmng, 0, ObjType_Domain, 0, 1);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "cap_retype domain_cap");
            return err_push(err, PROC_MGMT_ERR_CREATE_DOMAIN_CAP);
        }

        err = domain_cap_hash(node->domain_cap, &node->hash);
        if (err_is_fail(err)) {
            return err;
        }

        node->next = domain_cap_list;
        domain_cap_list = node;
        ++free_domain_caps;
    }

    return SYS_ERR_OK;
}

struct domain_cap_node *next_cap_node(void)
{
    assert(domain_cap_list != NULL);
    assert(free_domain_caps > 0);
    
    struct domain_cap_node *tmp = domain_cap_list;
    domain_cap_list = domain_cap_list->next;
    --free_domain_caps;
    
    return tmp;
}

errval_t domain_new(struct domain_cap_node *cap_node,
                    struct domain_entry **ret_entry)
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

    if (domain_table == NULL) {
#ifdef PROC_MGMT_BENCH
        bench_init();

        hash_ctl = calloc(1, sizeof(*hash_ctl));
        hash_ctl->mode = BENCH_MODE_FIXEDRUNS;
        hash_ctl->result_dimensions = 1;
        hash_ctl->min_runs = PROC_MGMT_BENCH_MIN_RUNS;
        hash_ctl->data = calloc(hash_ctl->min_runs * hash_ctl->result_dimensions,
                               sizeof(*hash_ctl->data));

        errval_t err = sys_debug_get_tsc_per_ms(&tscperus);
        assert(err_is_ok(err));
        tscperus /= 1000;
#endif
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
    ++entry->num_spawnds_resources;
}

errval_t domain_spawn(struct domain_cap_node *cap_node, coreid_t core_id)
{
    struct domain_entry *entry = NULL;
    errval_t err = domain_new(cap_node, &entry);
    if (err_is_fail(err)) {
        if (entry != NULL) {
            free(entry);
        }
        return err;
    }

    domain_run_on_core(entry, core_id);

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
