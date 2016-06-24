/**
 * \file
 * \brief Stubs of Octopus implementation
 *
 * Warning: This implementation provides only the very basic functionality
 * needed by the name service API in libbarrelfish!
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

#include <pcre.h>

#include <barrelfish/barrelfish.h>
#include <if/octopus_defs.h>

#include <octopus_server/debug.h>
#include <octopus_server/query.h>
#include <octopus/parser/ast.h>
#include <octopus/definitions.h>
#include <octopus/getset.h> // for SET_SEQUENTIAL define

#define MAX_RECORDS 1024
#define RECORD_NAME(ast) ((ast)->u.on.name->u.in.str)
#define RECORD_NAME_REGEX(ast) ((ast)->u.on.name->u.cnsn.value->u.sn.str)

// cnsn

struct wait_queue;
struct wait_queue {
    struct oct_reply_state* ors;
    struct wait_queue* next;
};

struct record {
    char* name;
    struct ast_object* record;
    struct wait_queue* waiting_parties;
};

static struct record record_storage[MAX_RECORDS] = { { NULL, NULL, NULL } };

static void transform_to_string(struct ast_object* ast, char* str)
{
    assert(ast != NULL);
    assert(str != NULL);
    size_t idx = 0;

    idx += sprintf(str + idx, "%s { ", RECORD_NAME(ast));

    struct ast_object* attr = ast->u.on.attrs;
    while (attr != NULL) {
        assert(attr->type == nodeType_Attribute);

        struct ast_object* pair = attr->u.an.attr;
        assert(pair != NULL);
        assert(pair->type == nodeType_Pair);

        struct ast_object* left = pair->u.pn.left;
        struct ast_object* right = pair->u.pn.right;
        ;assert(left != NULL);
        assert(right != NULL);

        assert(left->type == nodeType_Ident);
        idx += sprintf(str + idx, "%s: ", left->u.in.str);

        switch(right->type) {
            case nodeType_String:
                idx += sprintf(str + idx, "%s", right->u.sn.str);
                break;
            case nodeType_Constant:
                idx += sprintf(str + idx, "%"PRId64"", right->u.cn.value);
                break;
            case nodeType_Ident:
                idx += sprintf(str + idx, "%s ", right->u.in.str);
                break;
            default:
                USER_PANIC("Unsupported node type: %u\n", right->type);
        }

        attr = attr->u.an.next;
        if (attr != NULL) {
            idx += sprintf(str + idx, ", ");
        }
    }

    idx += sprintf(str + idx, " }");
    str[idx + 1] = '\0';

    OCT_DEBUG("transform to string: %s\n", str);
}

static void copy_ast(struct ast_object** copy, struct ast_object* ast)
{
    if (ast == NULL) {
        return;
    }

    *copy = malloc(sizeof(struct ast_object));
    memcpy(*copy, ast, sizeof(struct ast_object));

    switch (ast->type) {
    case nodeType_Object:
        copy_ast(&(*copy)->u.on.name, ast->u.on.name);
        copy_ast(&(*copy)->u.on.attrs, ast->u.on.attrs);
        break;

    case nodeType_Attribute:
        copy_ast(&(*copy)->u.an.attr, ast->u.an.attr);
        copy_ast(&(*copy)->u.an.next, ast->u.an.next);
        break;

    case nodeType_Pair:
        copy_ast(&(*copy)->u.pn.left, ast->u.pn.left);
        copy_ast(&(*copy)->u.pn.right, ast->u.pn.right);
        break;

    case nodeType_Ident:
        (*copy)->u.in.str = strdup(ast->u.in.str);
        break;

    case nodeType_String:
        (*copy)->u.sn.str = strdup(ast->u.sn.str);
        break;

    case nodeType_Constant:
        // Nothing to copy
        break;

    default:
        OCT_DEBUG("node is: %d\n", ast->type);
        assert(!"Unsupported Node!");
        break;
    }
}

errval_t get_record(struct ast_object* ast, struct oct_query_state* sqs)
{
    OCT_DEBUG("get record %s\n", RECORD_NAME(ast));
    assert(ast != NULL);
    assert(sqs != NULL);

    for (size_t i = 0; i < MAX_RECORDS; i++) {
        struct record* entry = &record_storage[i];
        if (entry->name == NULL) {
            continue;
        }

        if (strcmp(RECORD_NAME(ast), entry->name) == 0 && entry->record != NULL) {
            transform_to_string(entry->record, sqs->std_out.buffer);
            return SYS_ERR_OK;
        }
    }

    return OCT_ERR_NO_RECORD;
}

static void wakeup_clients(struct record* entry)
{
    struct wait_queue* cur = entry->waiting_parties;

    // Walk through list, wake up all clients
    while (cur != NULL) {
        assert(cur->ors != NULL);
        assert(cur->ors->reply != NULL);
        assert(cur->ors->binding != NULL);
        transform_to_string(entry->record,
                cur->ors->query_state.std_out.buffer);
        OCT_DEBUG(
                "wakeup %p for %s\n", cur->ors->binding, cur->ors->query_state.std_out.buffer);
        cur->ors->reply(cur->ors->binding, cur->ors);

        struct wait_queue* next = cur->next;
        free(cur);
        cur = next;
    }

    entry->waiting_parties = NULL;
}

errval_t set_record(struct ast_object* ast, uint64_t mode,
        struct oct_query_state* sqs)
{
    assert(ast != NULL);
    assert(sqs != NULL);
    assert(mode == 0);

    for (size_t i = 0; i < MAX_RECORDS; i++) {
        struct record* entry = &record_storage[i];
        if (entry->name == NULL) {
            continue;
        }

        OCT_DEBUG("found record: %s\n", entry->name);
        if (strcmp(RECORD_NAME(ast), entry->name) == 0) {
            assert(entry->record == NULL);
            copy_ast(&entry->record, ast);

            wakeup_clients(entry);
            return SYS_ERR_OK;
        }
    }

    for (size_t i = 0; i < MAX_RECORDS; i++) {
        struct record* entry = &record_storage[i];
        if (entry->name == NULL) {
            entry->name = strdup(RECORD_NAME(ast));
            copy_ast(&entry->record, ast);

            assert(entry->waiting_parties == NULL);
            return SYS_ERR_OK;
        }
    }

    assert(!"No more storage space!");
    return OCT_ERR_NO_RECORD;
}

errval_t del_record(struct ast_object* ast, struct oct_query_state* sqs)
{
    assert(ast != NULL);
    assert(sqs != NULL);

    for (size_t i = 0; i < MAX_RECORDS; i++) {
        struct record* entry = &record_storage[i];
        if (entry->name == NULL) {
            continue;
        }

        if (strcmp(RECORD_NAME(ast), entry->name) == 0) {
            assert(entry->record != NULL);

            free(entry->name);
            entry->name = NULL;
            free_ast(entry->record);
            entry->record = NULL;

            // Free the waiting list, this should be NULL anyways I guess
            struct wait_queue* cur = entry->waiting_parties;
            while (cur != NULL) {
                struct wait_queue* next = cur->next;
                free(cur);
                cur = next;
            }
            entry->waiting_parties = NULL;

            return SYS_ERR_OK;
        }
    }

    return OCT_ERR_NO_RECORD;
}

/**
 * TODO: this can only regex match on record name at the moment.
 */
errval_t get_record_names(struct ast_object* ast, struct oct_query_state* sqs)
{
    const char* errptr = NULL;
    int erroffset = 0;
    unsigned char tableptr;

    char** names = calloc(sizeof(char*), MAX_RECORDS);
    size_t names_cur = 0;

    OCT_DEBUG("%s:%s:%d: About to pcre_compile: %s\n", 
           __FILE__, __FUNCTION__, __LINE__, RECORD_NAME_REGEX(ast));
    pcre* reg = pcre_compile(RECORD_NAME_REGEX(ast), 0, &errptr, &erroffset, &tableptr);
    if (reg == NULL) {
        OCT_DEBUG("Failed to compile the regex errptr = %s erroroffset = %d", 
                  errptr, erroffset);
        return OCT_ERR_PARSER_FAIL;
    }

    for (size_t i = 0; i < MAX_RECORDS; i++) {        
        struct record* entry = &record_storage[i];
        if (entry->name == NULL) {
            continue;
        }

        int results[1];
        int rc = pcre_exec(reg, NULL, entry->name, strlen(entry->name),
                           0, 0, results, 1);
        if (rc == 0) {
            OCT_DEBUG("%s:%s:%d: We have a match\n", 
                      __FILE__, __FUNCTION__, __LINE__);
            names[names_cur++] = entry->name;
        }
        else if (rc < 0) {
            OCT_DEBUG("%s:%s:%d: pcre_exec failed matching %s with rc = %d.\n", 
                      __FILE__, __FUNCTION__, __LINE__, entry->name, rc);
        }
    }
    if (names_cur == 0) {
        return OCT_ERR_NO_RECORD;
    }

    // Form a comma separated string to send back over flounder
    names_cur = 0;
    int max_bytes = MAX_QUERY_LENGTH;
    sqs->std_out.buffer[0] = '\0';

    while(names[names_cur] != NULL) {
        strncat(sqs->std_out.buffer, names[names_cur], max_bytes);
        max_bytes -= strlen(names[names_cur]);
        names_cur++;
        if (names[names_cur] != NULL) {
            strncat(sqs->std_out.buffer, ",", max_bytes--);
        }
        assert(max_bytes > 0);
    }

    free(names);
    return SYS_ERR_OK;
}

errval_t set_watch(struct octopus_binding* b, struct ast_object* ast,
        uint64_t mode, struct oct_reply_state* drs, uint64_t* wid)
{
    OCT_DEBUG("set_watch %s\n", RECORD_NAME(ast));

    assert(ast != NULL);
    assert(mode == OCT_ON_SET);

    struct record* entry = NULL;
    for (size_t i = 0; i < MAX_RECORDS; i++) {
        entry = &record_storage[i];
        if (entry->name == NULL) {
            continue;
        }

        if (strcmp(RECORD_NAME(ast), entry->name) == 0) {
            goto insert;
        }
    }
    for (size_t i = 0; i < MAX_RECORDS; i++) {
        entry = &record_storage[i];
        if (entry->name == NULL) {
            entry->name = strdup(RECORD_NAME(ast));
            entry->record = NULL;
            entry->waiting_parties = NULL;
            goto insert;
        }
    }assert(!"Out of record space.");

    insert: if (strcmp(RECORD_NAME(ast), entry->name) == 0) {
        assert(entry->record == NULL);
        struct wait_queue* wq = malloc(sizeof(struct wait_queue));
        wq->next = NULL;
        wq->ors = drs;

        // Insert wq into waiting_parties of entry
        struct wait_queue** cur = &entry->waiting_parties;
        for (; *cur != NULL; cur = &(*cur)->next) {
            // Walk to the end of the list
        }
        *cur = wq;
    }

    *wid = 1;
    return SYS_ERR_OK;
}

errval_t del_watch(struct octopus_binding* b, octopus_trigger_id_t id,
        struct oct_query_state* dqs)
{
    assert(!"NYI");
    return OCT_ERR_INVALID_ID;
}

struct octopus_binding* get_event_binding(struct octopus_binding* b)
{
    assert(!"NYI");
    return b;
}

errval_t add_subscription(struct octopus_binding* b, struct ast_object* ast,
        uint64_t trigger_fn, uint64_t state, struct oct_reply_state* drs)
{
    assert(!"NYI");
    return OCT_ERR_NO_SUBSCRIPTION;
}

errval_t del_subscription(struct octopus_binding* b, uint64_t id,
        struct oct_query_state* sqs)
{
    assert(!"NYI");
    return OCT_ERR_NO_SUBSCRIPTION;
}

errval_t find_subscribers(struct ast_object* ast, struct oct_query_state* sqs)
{
    assert(!"NYI");
    return OCT_ERR_NO_SUBSCRIBERS;
}

errval_t set_binding(octopus_binding_type_t type, uint64_t id, void* binding)
{
    return SYS_ERR_OK;
}
