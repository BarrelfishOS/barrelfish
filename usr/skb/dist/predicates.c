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
static hash_table* subscriber_index = NULL;

static char* mystrdup(char* data)
{
    char *p = malloc(strlen(data)+1);
    if (p == NULL) {
        return NULL;
    }

    strcpy(p, data);
    return p;
}

static inline void init_index(void) {
    if(record_index == NULL) {
        hash_create_with_buckets(&record_index, HASH_INDEX_BUCKETS, NULL);
    }

    if(subscriber_index == NULL) {
        hash_create_with_buckets(&subscriber_index, HASH_INDEX_BUCKETS, NULL);
    }
}

static inline hash_table* choose_index(char* index) {
    hash_table* ht = NULL;

    if (strcmp(index, "rh") == 0) {
        ht = record_index;
    }
    else if (strcmp(index, "sh") == 0) {
        ht = subscriber_index;
    }
    else {
        DIST2_DEBUG("Invalid index name!\n");
        assert(!"Fix your prolog code.\n");
    }

    return ht;
}

int p_save_index(void)
{
    DIST2_DEBUG("p_save_index\n");
    init_index();

    // Select hash-table
    char* index_type = NULL;
    ec_get_string(ec_arg(1), &index_type);
    hash_table* ht = choose_index(index_type);

    // Retrieve Index from hash-table
    char* key = NULL;
    ec_get_string(ec_arg(2), &key);
    uint64_t hash_key = fnv_64a_str(key, FNV1A_64_INIT);
    struct skip_list* sl = (struct skip_list*) hash_find(ht, hash_key);
    if (sl == NULL) {
        errval_t err = skip_create_list(&sl);
        if (err_is_fail(err)) {
            return PFAIL;
        }
        hash_insert(ht, hash_key, sl);
    }
    // else: List already in hash-table

    // Add value into index
    char* value = NULL;
    int res = ec_get_string(ec_arg(3), &value);
    assert(res == PSUCCEED);

    skip_insert(sl, mystrdup(value));
    DIST2_DEBUG("insert %s into index[%s]=", value, key);
    //skip_print_list(sl);

    return PSUCCEED;
}

int p_remove_index(void)
{
    int res;
    char* key = NULL;
    char* value = NULL;
    char* index_type = NULL;

    init_index();


    res = ec_get_string(ec_arg(1), &index_type);
    if (res != PSUCCEED) {
        return res;
    }
    hash_table* ht = choose_index(index_type);

    res = ec_get_string(ec_arg(2), &key);
    if (res != PSUCCEED) {
        return res;
    }
    uint64_t hash_key = fnv_64a_str(key, FNV1A_64_INIT);
    struct skip_list* sl = hash_find(ht, hash_key);
    assert(sl != NULL);

    res = ec_get_string(ec_arg(3), &value);
    if (res != PSUCCEED) {
        return res;
    }

    char* elem = skip_delete(sl, value);
    assert(elem != NULL);
    free(elem);

    DIST2_DEBUG("remove %s from index[%s]=", value, key);
    //skip_print_list(sl);
    return PSUCCEED;
}

int p_index_intersect(void) /* p_index_intersect(type, -[Attributes], -Current, +Next) */
{
    DIST2_DEBUG("p_index_intersect\n");
    static struct skip_list** sets = NULL;
    static char* next = NULL;

    int res;
    char* key;
    size_t i = 0;

    init_index();

    char* index_type = NULL;
    res = ec_get_string(ec_arg(1), &index_type);
    if (res != PSUCCEED) {
        return res;
    }
    hash_table* ht = choose_index(index_type);

    res = ec_get_string(ec_arg(3), &next);
    if (res != PSUCCEED) {
        DIST2_DEBUG("state is not a string, find skip lists\n");
        free(sets);
        pword list, cur, rest;

        size_t elems = 0;
        for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
            elems++;
        }
        sets = malloc(sizeof(struct skip_list*) * elems);

        for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
            res = ec_get_string(cur, &key);
            if (res != PSUCCEED) {
                return res;
            }

            uint64_t hash_key = fnv_64a_str(key, FNV1A_64_INIT);
            struct skip_list* sl = hash_find(ht, hash_key);
            if (sl == NULL) {
                return PFAIL;
            }
            DIST2_DEBUG("skip_intersect found skip list for key: %s\n", key);
            //skip_print_list(sl);

            sets[i] = sl;
            i++;
        }
        next = NULL;
    }

    next = skip_intersect(sets, i, next);
    DIST2_DEBUG("skip_intersect found next: %s\n", next);
    if(next != NULL) {
        dident item = ec_did(next, 0);
        return ec_unify_arg(4, ec_atom(item));
    }

    return PFAIL;
}

int p_index_union(void) /* p_index_union(type, -[Attributes], -Current, +Next) */
{
    DIST2_DEBUG("p_index_union\n");
    static hash_table* union_ht = NULL;
    static char* next = NULL;

    int res;
    char* key;

    init_index();

    char* index_type = NULL;
    res = ec_get_string(ec_arg(1), &index_type);
    if (res != PSUCCEED) {
        return res;
    }
    hash_table* ht = choose_index(index_type);

    res = ec_get_string(ec_arg(3), &next);
    if (res != PSUCCEED) {
        DIST2_DEBUG("state is not a string, find skip lists\n");
        if (union_ht != NULL) {
            hash_release(union_ht);
            union_ht = NULL;
        }
        hash_create_with_buckets(&union_ht, HASH_INDEX_BUCKETS, NULL);

        pword list, cur, rest;
        for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
            res = ec_get_string(cur, &key);
            if (res != PSUCCEED) {
                return res;
            }

            uint64_t hash_key = fnv_64a_str(key, FNV1A_64_INIT);
            struct skip_list* sl = hash_find(ht, hash_key);

            // Insert all entries in union hash table
            if (sl != NULL) {
                DIST2_DEBUG("p_index_union found skip list for key: %s\n", key);
                //skip_print_list(sl);

                struct skip_node* sentry = sl->header->forward[0];
                while(sentry != NULL) {
                    uint64_t hash_key = fnv_64a_str(sentry->element, FNV1A_64_INIT);
                    if(hash_find(union_ht, hash_key) == NULL) {
                        DIST2_DEBUG("p_index_union insert: %s\n", sentry->element);
                        hash_insert(union_ht, hash_key, sentry->element);
                    }
                    sentry = sentry->forward[0];
                }
            }

        }
        next = NULL;
        hash_traverse_start(union_ht);
    }

    uint64_t hash_key;
    next = hash_traverse_next(union_ht, &hash_key);
    DIST2_DEBUG("skip_union found next: %s\n", next);
    if(next != NULL) {
        dident item = ec_did(next, 0);
        return ec_unify_arg(4, ec_atom(item));
    }
    else {
        hash_traverse_end(union_ht);
        return PFAIL;
    }
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
    if (drs->binding != NULL) {
        drs->reply(drs->binding, drs);
    }
    else {
        // Ignore sending trigger messages
        DIST2_DEBUG("No event binding found for watch_id: %lu", watch_id);
    }

    long int retract = true;
    return ec_unify_arg(5, ec_long(retract));
}
