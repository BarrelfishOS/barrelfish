/**
 * \file
 * \brief Contains handler functions for server-side dist2 interface RPC call.
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h> // read list
#include <if/dist2_defs.h>

#include <dist2_server/service.h>
#include <dist2_server/query.h>
#include <dist2_server/debug.h>

#include <dist2/parser/ast.h>
#include <dist2/getset.h>
#include <dist2/definitions.h>

#include <bench/bench.h>

#include "queue.h"

static uint64_t current_id = 1;

static inline errval_t check_query_length(char* query) {
    if (strlen(query) >= MAX_QUERY_LENGTH) {
        return DIST2_ERR_QUERY_SIZE;
    }

    return SYS_ERR_OK;
}

errval_t new_dist_reply_state(struct dist_reply_state** drt,
        dist_reply_handler_fn reply_handler)
{
    assert(*drt == NULL);
    *drt = malloc(sizeof(struct dist_reply_state));
    if (*drt == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    //memset(*drt, 0, sizeof(struct dist_reply_state));
    (*drt)->query_state.stdout.buffer[0] = '\0';
    (*drt)->query_state.stdout.length = 0;
    (*drt)->query_state.stderr.buffer[0] = '\0';
    (*drt)->query_state.stderr.length = 0;
    (*drt)->binding = 0;
    (*drt)->return_record = false;
    (*drt)->error = 0;
    (*drt)->watch_id = 0;
    (*drt)->server_id = 0;
    (*drt)->reply = reply_handler;
    (*drt)->next = NULL;

    return SYS_ERR_OK;
}

static void free_dist_reply_state(void* arg)
{
    if (arg != NULL) {
        struct dist_reply_state* drt = (struct dist_reply_state*) arg;
        // In case we have to free things in dist_reply_state, free here...

        free(drt);
    } else {
        assert(!"free_reply_state with NULL argument?");
    }
}

static void trigger_send_handler(struct dist2_binding* b,
        struct dist_reply_state* drs)
{
    errval_t err;
    err = b->tx_vtbl.trigger(b, MKCONT(free_dist_reply_state, drs),
            drs->trigger.trigger, drs->trigger.st,
            drs->query_state.stdout.buffer);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            dist_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

static inline void install_trigger(struct dist2_binding* binding,
        struct ast_object* ast, dist2_trigger_t trigger, errval_t error)
{
    errval_t err;

    if (trigger.m > 0 && trigger.in_case == err_no(error)) {
        struct dist_reply_state* trigger_reply = NULL;
        err = new_dist_reply_state(&trigger_reply, trigger_send_handler);
        assert(err_is_ok(err));

        trigger_reply->trigger = trigger;
        trigger_reply->binding = get_event_binding(binding);
        if (trigger_reply->binding == NULL) {
            // In case no event binding is set, try to notify
            // over regular binding
            trigger_reply->binding = binding;
        }

        err = set_watch(ast, trigger.m, trigger_reply);
        assert(err_is_ok(err));
    }

}

static inline void arrival_rate(void)
{
    static cycles_t measure_time = 10000;
    static uint64_t arrivals = 0;
    static cycles_t start = 0;
    arrivals++;
    if ( (arrivals % 100) == 0 && bench_tsc_to_ms(bench_tsc() - start) > measure_time) {
        printf("Get Rate per sec: %lu\n", arrivals / (measure_time / 1000));
        start = bench_tsc();
        arrivals = 0;
    }
}

static void get_reply(struct dist2_binding* b, struct dist_reply_state* srt)
{
    errval_t err;
    char* reply = err_is_ok(srt->error) ?
            srt->query_state.stdout.buffer : NULL;
    err = b->tx_vtbl.get_response(b, MKCONT(free_dist_reply_state, srt),
            reply, srt->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            dist_rpc_enqueue_reply(b, srt);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void get_handler(struct dist2_binding *b, char *query, dist2_trigger_t trigger)
{
    errval_t err = SYS_ERR_OK;

    struct dist_reply_state* srt = NULL;
    struct ast_object* ast = NULL;
    err = new_dist_reply_state(&srt, get_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        err = get_record(ast, &srt->query_state);
        install_trigger(b, ast, trigger, err);
    }

out:
    srt->error = err;
    srt->reply(b, srt);

    arrival_rate();

    free_ast(ast);
    free(query);
}

static void get_names_reply(struct dist2_binding* b,
        struct dist_reply_state* srt)
{
    errval_t err;
    char* reply = err_is_ok(srt->error) ?
            srt->query_state.stdout.buffer : NULL;
    err = b->tx_vtbl.get_names_response(b, MKCONT(free_dist_reply_state, srt),
            reply, srt->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            dist_rpc_enqueue_reply(b, srt);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void get_names_handler(struct dist2_binding *b, char *query, dist2_trigger_t t)
{
    DIST2_DEBUG(" get_names_handler: %s\n", query);

    errval_t err = SYS_ERR_OK;

    struct dist_reply_state* srt = NULL;
    struct ast_object* ast = NULL;

    err = new_dist_reply_state(&srt, get_names_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        err = get_record_names(ast, &srt->query_state);
        install_trigger(b, ast, t, err);
    }

out:
    srt->error = err;
    srt->reply(b, srt);

    free_ast(ast);
    free(query);
}

static void set_reply(struct dist2_binding* b, struct dist_reply_state* srs)
{
    char* record = err_is_ok(srs->error) && srs->return_record ?
            srs->query_state.stdout.buffer : NULL;

    errval_t err;
    err = b->tx_vtbl.set_response(b, MKCONT(free_dist_reply_state, srs), record,
            srs->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            dist_rpc_enqueue_reply(b, srs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void set_handler(struct dist2_binding *b, char *query, uint64_t mode,
        dist2_trigger_t trigger, bool get)
{
    DIST2_DEBUG(" set_handler: %s\n", query);
    errval_t err = SYS_ERR_OK;

    struct dist_reply_state* srs = NULL;
    struct ast_object* ast = NULL;

    err = new_dist_reply_state(&srs, set_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        if (ast->u.on.name->type == nodeType_Ident) {
            err = set_record(ast, mode, &srs->query_state);
            install_trigger(b, ast, trigger, err);
        }
        else {
            // Since we don't have any ACLs atm. we do not
            // allow name to be a regex/variable, because
            // we it's not guaranteed which records get
            // modified in this case.
            err = DIST2_ERR_NO_RECORD_NAME;
        }
    }

out:
    srs->error = err;
    srs->return_record = get;
    srs->reply(b, srs);

    free_ast(ast);
    free(query);
}

static void del_reply(struct dist2_binding* b, struct dist_reply_state* srs)
{
    errval_t err;
    err = b->tx_vtbl.del_response(b, MKCONT(free_dist_reply_state, srs),
            srs->error);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            dist_rpc_enqueue_reply(b, srs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void del_handler(struct dist2_binding* b, char* query, dist2_trigger_t trigger)
{
    DIST2_DEBUG(" del_handler: %s\n", query);
    errval_t err = SYS_ERR_OK;

    struct dist_reply_state* srs = NULL;
    struct ast_object* ast = NULL;

    err = new_dist_reply_state(&srs, del_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        if (ast->u.on.name->type == nodeType_Ident) {
            err = del_record(ast, &srs->query_state);
            install_trigger(b, ast, trigger, err);
        }
        else {
            // Since we don't have any ACLs atm. we do not
            // allow name to be a regex/variable
            // (see set_handler).
            err = DIST2_ERR_NO_RECORD_NAME;
        }
    }

out:
    srs->error = err;
    srs->reply(b, srs);

    free_ast(ast);
    free(query);
}

static void exists_reply(struct dist2_binding* b, struct dist_reply_state* drs)
{
    errval_t err;
    err = b->tx_vtbl.exists_response(b, MKCONT(free_dist_reply_state, drs),
            drs->error);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            dist_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void exists_handler(struct dist2_binding* b, char* query,
        dist2_trigger_t trigger)
{
    errval_t err = SYS_ERR_OK;

    struct dist_reply_state* drs = NULL;
    struct ast_object* ast = NULL;

    err = new_dist_reply_state(&drs, exists_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        err = get_record(ast, &drs->query_state);
        install_trigger(b, ast, trigger, err);
    }

out:
    drs->error = err;
    drs->reply(b, drs);

    free_ast(ast);
    free(query);
}

static void wait_for_reply(struct dist2_binding* b, struct dist_reply_state* drs)
{
    errval_t err;
    err = b->tx_vtbl.wait_for_response(b, MKCONT(free_dist_reply_state, drs),
            drs->query_state.stdout.buffer, drs->error);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            dist_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

// XXX: For compatibility reasons with nameserver API
void wait_for_handler(struct dist2_binding* b, char* query) {
    errval_t err = SYS_ERR_OK;
    errval_t set_watch_err = SYS_ERR_OK;

    struct dist_reply_state* drs = NULL;
    struct ast_object* ast = NULL;

    err = new_dist_reply_state(&drs, wait_for_reply);
    drs->binding = b;
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        err = get_record(ast, &drs->query_state);
        if (err_no(err) == DIST2_ERR_NO_RECORD) {
            set_watch_err = set_watch(ast, DIST_ON_SET, drs);
        }
    }

out:
    if (err_no(err) != DIST2_ERR_NO_RECORD || err_is_fail(set_watch_err)) {
        drs->error = err;
        if (err_is_fail(set_watch_err)) {
            // implies err = DIST2_ERR_NO_RECORD
            drs->error = set_watch_err;
        }
        drs->reply(b, drs);
    }

    free_ast(ast);
    free(query);
}

static void subscribe_reply(struct dist2_binding* b,
        struct dist_reply_state* srs)
{
    errval_t err;
    err = b->tx_vtbl.subscribe_response(b, MKCONT(free_dist_reply_state, srs),
            srs->server_id, srs->error);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            dist_rpc_enqueue_reply(b, srs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void subscribe_handler(struct dist2_binding *b, char* query, uint64_t client_id)
{
    DIST2_DEBUG("subscribe: query = %s\n", query);
    errval_t err = SYS_ERR_OK;

    struct dist_reply_state* drs = NULL;
    struct ast_object* ast = NULL;

    err = new_dist_reply_state(&drs, subscribe_reply);
    assert(err_is_ok(err));

    err = check_query_length(query);
    if (err_is_fail(err)) {
        goto out;
    }

    err = generate_ast(query, &ast);
    if (err_is_ok(err)) {
        err = add_subscription(b, ast, client_id, drs);
    }

out:
    drs->error = err;
    drs->reply(b, drs);

    free_ast(ast);
    free(query);
}

static void unsubscribe_reply(struct dist2_binding* b,
        struct dist_reply_state* srs)
{
    errval_t err;
    err = b->tx_vtbl.unsubscribe_response(b, MKCONT(free_dist_reply_state, srs),
            srs->query_state.exec_res);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            dist_rpc_enqueue_reply(b, srs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void unsubscribe_handler(struct dist2_binding *b, uint64_t id)
{
    errval_t err = SYS_ERR_OK;

    DIST2_DEBUG("unsubscribe: id = %lu\n", id);

    struct dist_reply_state* srs = NULL;
    err = new_dist_reply_state(&srs, unsubscribe_reply);
    assert(err_is_ok(err));

    err = del_subscription(b, id, &srs->query_state);

    srs->error = err;
    srs->reply(b, srs);
}

static void send_subscribed_message(struct dist2_binding* b, uint64_t id,
        char* object)
{
    errval_t err;
    err = b->tx_vtbl.subscribed_message(b, NOP_CONT, id, object);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            // TODO
            //return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }

}

static void publish_reply(struct dist2_binding* b, struct dist_reply_state* srs)
{
    errval_t err;
    err = b->tx_vtbl.publish_response(b, MKCONT(free_dist_reply_state, srs),
            srs->query_state.exec_res);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            dist_rpc_enqueue_reply(b, srs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void publish_handler(struct dist2_binding *b, char* record)
{
    DIST2_DEBUG("publish_handler query: %s\n", record);
    errval_t err = SYS_ERR_OK;

    struct dist_reply_state* srs = NULL;
    err = new_dist_reply_state(&srs, publish_reply);
    assert(err_is_ok(err));

    err = check_query_length(record);
    if (err_is_fail(err)) {
        srs->error = err;
        srs->reply(b, srs);
        free(record);
        return;
    }

    struct ast_object* ast = NULL;
    err = generate_ast(record, &ast);
    // Reply to client
    srs->error = err;
    srs->reply(b, srs);
    if (err_is_ok(err)) {
        err = find_subscribers(ast, &srs->query_state);
        if (err_is_ok(err)) {
            struct dist2_binding* recipient = NULL;
            uint64_t id = 0;

            // TODO remove skb list parser dependency
            struct list_parser_status status;
            skb_read_list_init_offset(&status, srs->query_state.stdout.buffer, 0);

            // Send to all subscribers
            while (skb_read_list(&status, "subscriber(%lu, %lu)",
                    (uintptr_t*) &recipient, &id)) {
                DIST2_DEBUG("publish msg to: recipient:%p id:%lu\n", recipient, id);
                send_subscribed_message(recipient, id, record); // TODO no send queue
            }
        }
    }

    //free(object); TODO: used by send_subscribed_object
    free_ast(ast);
}

void get_identifier(struct dist2_binding* b)
{
    errval_t err = b->tx_vtbl.get_identifier_response(b, NOP_CONT,
            current_id++);
    assert(err_is_ok(err));
}

static void identify_binding_reply(struct dist2_binding* b,
        struct dist_reply_state* drs)
{
    errval_t err;
    // TODO send drs->error back to client!
    err = b->tx_vtbl.identify_response(b, MKCONT(free_dist_reply_state, drs));
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            dist_rpc_enqueue_reply(b, drs);
            return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }

}

void identify_binding(struct dist2_binding* b, uint64_t id,
        dist2_binding_type_t type)
{
    assert(id <= current_id);

    struct dist_reply_state* drs = NULL;
    errval_t err = new_dist_reply_state(&drs, identify_binding_reply);
    assert(err_is_ok(err));

    DIST2_DEBUG("set binding: id=%lu type=%d\n", id, type);
    drs->error = set_binding(type, id, b);
    drs->reply(b, drs);
}

