/**
 * \file
 * \brief Definitions for external C predicates used in Prolog code of
 * the dist2 server implementation.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <eclipse.h>
#include <barrelfish/barrelfish.h>
#include <include/skb_server.h>
#include <collections/hash_table.h>

#include <dist2_server/debug.h>
#include <dist2_server/service.h>


#include "predicates.h"
#include "skiplist.h"
#include "fnv.h"

#define HASH_INDEX_BUCKETS 6151
static hash_table* record_index = NULL;

static char* mystrdup(char* data)
{

    char *p = malloc(strlen(data) + 1);
    if (p == NULL) {
        return NULL;
    }

    strcpy(p, data);
    return p;
}

int p_save_index(void)
{
    char* key = NULL;
    char* value = NULL;
    ec_get_string(ec_arg(1), &key);
    ec_get_string(ec_arg(2), &value);

    if(record_index == NULL) {
        hash_create_with_buckets(&record_index, HASH_INDEX_BUCKETS, NULL);
    }

    uint64_t hash_key = fnv_64a_str(key, FNV1A_64_INIT);
    struct skip_list* sl = (struct skip_list*) hash_find(record_index, hash_key);
    if (sl == NULL) {
        errval_t err = skip_create_list(&sl);
        if (err_is_fail(err)) {
            return PFAIL;
        }
        hash_insert(record_index, hash_key, sl);
    }
    // else: List already in hash table

    // Add our value into the index
    skip_insert(sl, mystrdup(value));

    skip_print_list(sl);

    return PSUCCEED;
}

int p_remove_index(void)
{
    char* key = NULL;
    char* value = NULL;
    ec_get_string(ec_arg(1), &key);
    ec_get_string(ec_arg(2), &value);
    assert(record_index != NULL);

    uint64_t hash_key = fnv_64a_str(key, FNV1A_64_INIT);
    struct skip_list* sl = hash_find(record_index, hash_key);
    assert(sl != NULL);

    char* elem = skip_delete(sl, mystrdup(value));
    assert(elem != NULL);
    free(elem);

    skip_print_list(sl);

    return PSUCCEED;
}

int p_index_intersect(void) /* p_index_intersect(-[Attributes], -Current, +Next) */
{
    static struct skip_list** sets = NULL;
    static char* next = NULL;

    int res;
    char* key;
    size_t i = 0;

    res = ec_get_string(ec_arg(0), &next);
    if (res != PSUCCEED) {
        free(sets);
        pword list, cur, rest;

        size_t elems = 0;
        for (list = ec_arg(1); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
            elems++;
        }
        sets = malloc(sizeof(struct skip_list*) * elems);

        for (list = ec_arg(1); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
            res = ec_get_string(cur, &key);
            if (res != PSUCCEED) {
                return res;
            }

            uint64_t hash_key = fnv_64a_str(key, FNV1A_64_INIT);
            struct skip_list* sl = hash_find(record_index, hash_key);
            if (sl == NULL) {
                return PFAIL;
            }

            sets[i] = sl;
            i++;
        }
        next = NULL;
    }

    next = skip_intersect(sets, i, next);
    dident item = ec_did(next, 0);
    return ec_unify_arg(3, ec_atom(item));
}

int p_notify_client(void) /* p_notify_client(+String, ReplyState) */
{
    DIST2_DEBUG("p_notify_client\n");

    struct dist_reply_state* drs;
    char* str = NULL;

    ec_get_string(ec_arg(1), &str);
    ec_get_long(ec_arg(2), (long int*) &drs); // TODO conversion to pointer?
    assert(strlen(str)+1 < BUFFER_SIZE); // TODO

    strcpy(drs->query_state.stdout.buffer, str);
    debug_printf("p_notify_client: %s\n", drs->query_state.stdout.buffer);

    drs->error = SYS_ERR_OK;
    drs->reply(drs->binding, drs);

    return PSUCCEED;
}


int p_trigger_watch(void) /* p_trigger_watch(+String, +Mode, +Recipient, +WatchId, -Retract) */
{
    int res;

    // Get arguments
    char* record = NULL;
    res = ec_get_string(ec_arg(1), &record);
    if (res != PSUCCEED) {
        return res;
    }
    assert(strlen(record)+1 < BUFFER_SIZE); // TODO

    long int mode = 0;
    res = ec_get_long(ec_arg(2), &mode);
    if (res != PSUCCEED) {
        return res;
    }

    struct dist_reply_state* drs = NULL;
    res = ec_get_long(ec_arg(3), (long int*) &drs);
    if (res != PSUCCEED) {
        return res;
    }
    assert(drs != NULL);
    DIST2_DEBUG("drs is: %p\n", drs);

    long int watch_id = 0;
    res = ec_get_long(ec_arg(4), &watch_id);
    if (res != PSUCCEED) {
        return res;
    }

    strcpy(drs->query_state.stdout.buffer, record);
    DIST2_DEBUG("p_trigger_watch: %s\n", drs->query_state.stdout.buffer);
    DIST2_DEBUG("drs->binding: %p\n", drs->binding);
    DIST2_DEBUG("drs->reply: %p\n", drs->reply);

    drs->error = SYS_ERR_OK;
    drs->reply(drs->binding, drs);

    long int retract = true;
    return ec_unify_arg(5, ec_long(retract));
}


