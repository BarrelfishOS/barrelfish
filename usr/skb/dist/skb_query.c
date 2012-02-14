/**
 * \file
 * \brief Implementation of the interface as described in <dist2_server/query.h>.
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
//#include <eclipse.h>

#include <barrelfish/barrelfish.h>
#include <include/skb_debug.h>

#include <if/dist2_defs.h>

#include <include/skb_server.h>

#include <dist2_server/debug.h>
#include <dist2_server/query.h>
#include <dist2/parser/ast.h>
#include <dist2/getset.h> // for SET_SEQUENTIAL define
#include "code_generator.h"
#include "bitfield.h"

#include <bench/bench.h>

#define STDOUT_QID 1
#define STDERR_QID 2

static struct bitfield* subscriber_ids = NULL;
static struct bitfield* trigger_ids = NULL;

STATIC_ASSERT(sizeof(long int) >= sizeof(uintptr_t),
        "Storage for pointers in SKB must be big enough");

static errval_t transform_ec_error(int res)
{
    errval_t err = SYS_ERR_OK;

    switch (res) {
    case PSUCCEED:
        err = SYS_ERR_OK;
        break;

    case PFLUSHIO:
        err = SKB_ERR_IO_OUTPUT;
        break;

    case PFAIL:
        err = SKB_ERR_GOAL_FAILURE;
        break;

    case PTHROW:
        err = SKB_ERR_EXECUTION;
        break;

    default:
        DIST2_DEBUG("Unexpected Eclipse error: %d", res);
        assert(!"Should not happen");
        break;
    }

    return err;
}

static void read_eclipse_queue(int qid, struct skb_writer* w)
{
    int res = 0;

    do {
        res = ec_queue_read(qid, w->buffer + w->length, BUFFER_SIZE - res);
        w->length += res;
    } while ((res != 0) && (w->length < BUFFER_SIZE));

    w->buffer[w->length] = '\0';
}

static errval_t run_eclipse(struct dist_query_state* st)
{
    assert(st != NULL);
    long int qid;

    // Contains Queue ID in case of PFLUSHIO
    ec_ref retval = ec_ref_create_newvar();
    //ec_post_goal(ec_term(ec_did("flush", 1), ec_long(1)));
    //ec_post_goal(ec_term(ec_did("flush", 1), ec_long(2)));
    //ec_post_goal(ec_term(ec_did("garbage_collect", 0)));
    while ((st->exec_res = ec_resume1(retval)) == PFLUSHIO) {
        ec_get_long(ec_ref_get(retval), &qid);

        switch (qid) {
        case STDOUT_QID:
            read_eclipse_queue(STDOUT_QID, &st->stdout);
            break;

        case STDERR_QID:
            read_eclipse_queue(STDERR_QID, &st->stderr);
            break;
        }
    }

    // atm. ignore all subsequent results
    if (st->exec_res == PSUCCEED) {
        ec_cut_to_chp(retval);
        ec_resume1(retval);
    }

    ec_ref_destroy(retval);

    errval_t err = transform_ec_error(st->exec_res);
    if (err_no(err) == SKB_ERR_EXECUTION) {
        err = err_push(err, DIST2_ERR_ENGINE_FAIL);
    }

    return err;
}

static void debug_skb_output(struct dist_query_state* st)
{
    DIST2_DEBUG(
            " output: %s error: %s error_code: %d\n", st->stdout.buffer, st->stderr.buffer, st->exec_res);
}

errval_t get_record(struct ast_object* ast, struct dist_query_state* sqs)
{
    assert(ast != NULL);
    assert(sqs != NULL);

    struct skb_ec_terms sr;
    errval_t err = transform_record(ast, &sr);
    if (err_is_ok(err)) {
        // Calling get_object(Name, Attrs, Constraints, Y), print_object(Y).
        dident get_object = ec_did("get_first_object", 4);
        dident print_object = ec_did("print_object", 1);

        pword print_var = ec_newvar();
        pword get_object_term = ec_term(get_object, sr.name, sr.attribute_list,
                sr.constraint_list, print_var);
        pword print_term = ec_term(print_object, print_var);

        ec_post_goal(get_object_term);
        ec_post_goal(print_term);

        err = run_eclipse(sqs);
        if (err_no(err) == SKB_ERR_GOAL_FAILURE) {
            err = err_push(err, DIST2_ERR_NO_RECORD);
        }

        DIST2_DEBUG(" get_record:\n");
        debug_skb_output(sqs);
    }

    return err;
}

errval_t get_record_names(struct ast_object* ast, struct dist_query_state* dqs)
{
    assert(ast != NULL);
    assert(dqs != NULL);

    struct skb_ec_terms sr;
    errval_t err = transform_record(ast, &sr);
    if (err_is_ok(err)) {
        // Calling findall(X, get_object(X, Attrs, Constraints, _), L),
        // prune_instances(L, PL), print_names(PL).
        dident prune_instances = ec_did("prune_instances", 2);
        dident findall = ec_did("findall", 3);
        dident get_object = ec_did("get_object", 4);
        dident print_names = ec_did("print_names", 1);

        pword var_l = ec_newvar();
        pword var_pl = ec_newvar();
        pword var_rec = ec_newvar();

        pword get_object_term = ec_term(get_object, sr.name, sr.attribute_list,
                sr.constraint_list, var_rec);
        pword findall_term = ec_term(findall, var_rec, get_object_term, var_l);
        pword prune_results = ec_term(prune_instances, var_l, var_pl);
        pword print_names_term = ec_term(print_names, var_pl);

        ec_post_goal(findall_term);
        ec_post_goal(prune_results);
        ec_post_goal(print_names_term);

        err = run_eclipse(dqs);
        if (err_is_ok(err) && dqs->stdout.buffer[0] == '\0') {
            err = DIST2_ERR_NO_RECORD;
        }
        else if (err_no(err) == SKB_ERR_GOAL_FAILURE) {
            assert(!"findall failed - should not happen!");
            // see http://eclipseclp.org/doc/bips/kernel/allsols/findall-3.html
        }

        DIST2_DEBUG(" get_record_names:\n");
        debug_skb_output(dqs);
    }

    return err;
}

errval_t set_record(struct ast_object* ast, uint64_t mode,
        struct dist_query_state* sqs)
{
    assert(ast != NULL);
    assert(sqs != NULL);

    struct skb_ec_terms sr;
    errval_t err = transform_record(ast, &sr);
    if (err_is_ok(err)) {
        // Calling add_object(Name, Attributes)
        dident add_object;
        if (mode & SET_SEQUENTIAL) {
            add_object = ec_did("add_seq_object", 3);
        }
        else {
            add_object = ec_did("add_object", 3);
        }

        pword add_object_term = ec_term(add_object, sr.name, sr.attribute_list,
                sr.constraint_list);
        ec_post_goal(add_object_term);

        err = run_eclipse(sqs);
        if (err_no(err) == SKB_ERR_GOAL_FAILURE) {
            DIST2_DEBUG("Goal failure during set record. Should not happen!");
            assert(!"SKB_ERR_GOAL_FAILURE during set?");
            // In case assertions are disabled we can just pass on the error
            // however it may be better to introduce a dist2 error for this
        }

        DIST2_DEBUG(" set_record:\n");
        debug_skb_output(sqs);
    }

    return err;
}

errval_t del_record(struct ast_object* ast, struct dist_query_state* dqs)
{
    // TODO sr.attributes, sr.constraints currently not used for delete
    // it's just based on the name
    // Think about how to constraints / attributes behave with del
    assert(ast != NULL);
    assert(dqs != NULL);

    struct skb_ec_terms sr;
    errval_t err = transform_record(ast, &sr);
    if (err_is_ok(err)) {
        // Calling del_object(Name)
        dident del_object = ec_did("del_object", 3);
        pword del_object_term = ec_term(del_object, sr.name,
                sr.attribute_list, sr.constraint_list);
        ec_post_goal(del_object_term);

        err = run_eclipse(dqs);
        if (err_no(err) == SKB_ERR_GOAL_FAILURE) {
            err = err_push(err, DIST2_ERR_NO_RECORD);
        }
        DIST2_DEBUG(" del_record:\n");
        debug_skb_output(dqs);
    }

    return err;
}

static errval_t find_free_id(struct bitfield* bf, uint64_t* id)
{
    for (size_t i=0; i<BITFIELD_MAX; i++) {
        if (!bitfield_get(bf, i)) {
            bitfield_on(bf, i);
            assert(bitfield_get(bf, i) != false);
            *id = i;
            return SYS_ERR_OK;
        }
    }

    return DIST2_ERR_MAX_SUBSCRIPTIONS;
}

static errval_t init_bitmap(struct bitfield** bf)
{
    errval_t err = SYS_ERR_OK;
    if (*bf == NULL) {
        err = bitfield_create(bf);
    }

    return err;
}

static void store_template(struct dist_reply_state* drs,
        struct skb_ec_terms* sr, pword storage, pword recipient)
{
    dident add_subscription = ec_did("add_subscription", 4);
    dident template = ec_did("template", 3);

    pword id_term = ec_long((long int) drs->watch_id);
    pword template_term = ec_term(template, sr->name, sr->attribute_list,
            sr->constraint_list);

    pword subscribe_term = ec_term(add_subscription, storage, id_term,
            template_term, recipient);

    ec_post_goal(subscribe_term);

}

errval_t set_watch(struct ast_object* ast, uint64_t mode,
        struct dist_reply_state* drs, uint64_t* wid)
{
    errval_t err = init_bitmap(&trigger_ids);
    if (err_is_fail(err)) {
        return err;
    }

    err = find_free_id(trigger_ids, wid);
    if (err_is_fail(err)) {
        return err;
    }

    struct skb_ec_terms sr;
    err = transform_record(ast, &sr);
    if (err_is_ok(err)) {

        dident subscriber = ec_did("subscriber", 4);
        pword binding_term = ec_long((long int) drs->binding);
        pword id_term = ec_long((long int) *wid);
        pword reply_state = ec_long((long int) drs);
        pword mode_term = ec_long((long int) mode);
        pword storage = ec_atom(ec_did("trigger", 0));
        pword subscriber_term = ec_term(subscriber, binding_term,
                id_term, reply_state, mode_term);

        drs->watch_id = *wid;
        store_template(drs, &sr, storage, subscriber_term);

        err = run_eclipse(&drs->query_state);
        if (err_no(err) == SKB_ERR_GOAL_FAILURE) {
            assert(!"set_watch failed - should not happen!");
            bitfield_off(trigger_ids, drs->watch_id);
        }

        DIST2_DEBUG("set watch\n");
        debug_skb_output(&drs->query_state);
    }

    return err;
}

errval_t del_watch(struct dist2_binding* b, dist2_trigger_id_t id,
        struct dist_query_state* dqs)
{
    dident remove_watch = ec_did("remove_watch", 2);
    pword binding_ptr = ec_long((long int) b);
    pword watch_id_val = ec_long((long int) id);
    pword remove_watch_term = ec_term(remove_watch, binding_ptr, watch_id_val);
    ec_post_goal(remove_watch_term);

    errval_t err = run_eclipse(dqs);
    if (err_no(err) == SKB_ERR_GOAL_FAILURE) {
        err = err_push(err, DIST2_ERR_INVALID_ID);
    }

    DIST2_DEBUG(" del_trigger id is %lu:\n", id);
    debug_skb_output(dqs);

    return err;
}

struct dist2_binding* get_event_binding(struct dist2_binding* b)
{
    errval_t err = SYS_ERR_OK;
    struct dist_query_state* dqs = calloc(1, sizeof(struct dist_query_state));
    if (dqs == NULL) {
        DIST2_DEBUG("Server out of memory.");
        return NULL;
    }

    // Calling binding(_, X, Binding), write(X).
    dident binding = ec_did("binding", 3);
    dident write = ec_did("writeln", 1);

    pword bind_term = ec_long((long int) b);
    pword var_ = ec_newvar();
    pword var_x = ec_newvar();
    pword binding_term = ec_term(binding, var_, var_x, bind_term);
    pword write_term = ec_term(write, var_x);

    ec_post_goal(binding_term);
    ec_post_goal(write_term);

    err = run_eclipse(dqs);
    if (err_is_fail(err)) {
        DIST2_DEBUG("No event binding found for client.");
        assert(!"Should not happen - check client initialization code!");

        return NULL;
    }

    DIST2_DEBUG("get_event_binding\n");
    debug_skb_output(dqs);

    struct dist2_binding* recipient = NULL;
    // TODO pointer size vs. long int in skb :-(
    sscanf(dqs->stdout.buffer, "%lu", (uintptr_t*) &recipient);

    free(dqs);
    return recipient;
}

errval_t add_subscription(struct dist2_binding* b, struct ast_object* ast,
        uint64_t client_state, struct dist_reply_state* drs)
{
    errval_t err = init_bitmap(&subscriber_ids);
    if (err_is_fail(err)) {
        return err;
    }

    err = find_free_id(subscriber_ids, &drs->watch_id);
    if (err_is_fail(err)) {
        return err;
    }

    struct skb_ec_terms sr;
    err = transform_record(ast, &sr);
    if (err_is_ok(err)) {
        // Calling add_subscription(ps, ServerID,
        // template(Name, Attributes, Constraints), subscriber(EventBinding, ClientState))
        dident subscriber = ec_did("subscriber", 2);
        pword binding_term = ec_long((long int) get_event_binding(b));
        pword storage = ec_atom(ec_did("ps", 0));
        pword subscriber_term = ec_term(subscriber, binding_term,
                ec_long(client_state));

        store_template(drs, &sr, storage, subscriber_term);

        err = run_eclipse(&drs->query_state);
        if (err_no(err) == SKB_ERR_GOAL_FAILURE) {
            DIST2_DEBUG("Subscription failed, check prolog code!");
            assert(!"add_subscription failed - should not happen!");
            bitfield_off(subscriber_ids, drs->watch_id);
        }

        DIST2_DEBUG("add_subscription\n");
        debug_skb_output(&drs->query_state);
    }

    return err;
}

errval_t del_subscription(struct dist2_binding* b, uint64_t id,
        struct dist_query_state* sqs)
{
    errval_t err = SYS_ERR_OK;
    pword binding_term = ec_long((long int) get_event_binding(b));

    dident delete_subscription = ec_did("delete_subscription", 3);
    pword storage = ec_atom(ec_did("ps", 0));
    pword id_term = ec_long(id);
    pword delete_subscription_term = ec_term(delete_subscription, storage,
            id_term, binding_term);

    ec_post_goal(delete_subscription_term);
    err = run_eclipse(sqs);
    if (err_is_ok(err)) {
        assert(subscriber_ids != NULL); // should not happen if eclipse succeeds
        bitfield_off(subscriber_ids, id);
    }
    if (err_no(err) == SKB_ERR_GOAL_FAILURE) {
        err = err_push(err, DIST2_ERR_NO_SUBSCRIPTION);
    }

    DIST2_DEBUG("del_subscription:\n");
    debug_skb_output(sqs);

    return err;
}

errval_t find_subscribers(struct ast_object* ast, struct dist_query_state* sqs)
{
    struct skb_ec_terms sr;
    errval_t err = transform_record(ast, &sr);
    // TODO error if we have constraints here?
    if (err_is_ok(err)) {
        // Calling findall(X, find_subscriber(object(Name, Attributes), X), L), write(L)
        dident findall = ec_did("findall", 3);
        dident find_subscriber = ec_did("find_subscriber", 3);
        dident object = ec_did("object", 2);
        dident write = ec_did("writeln", 1);

        pword storage = ec_atom(ec_did("ps", 0));
        pword var_x = ec_newvar();
        pword var_l = ec_newvar();
        pword object_term = ec_term(object, sr.name, sr.attribute_list);
        pword find_subs_term = ec_term(find_subscriber, storage, object_term, var_x);
        pword findall_term = ec_term(findall, var_x, find_subs_term, var_l);
        pword write_term = ec_term(write, var_l);

        ec_post_goal(findall_term);
        ec_post_goal(write_term);

        err = run_eclipse(sqs);
        if (err_no(err) == SKB_ERR_GOAL_FAILURE) {
            err = err_push(err, DIST2_ERR_NO_SUBSCRIBERS);
        }
    }

    DIST2_DEBUG("find_subscribers\n");
    debug_skb_output(sqs);

    return err;
}

errval_t set_binding(dist2_binding_type_t type, uint64_t id, void* binding)
{
    struct dist_query_state* dqs = malloc(sizeof(struct dist_query_state));
    if (dqs == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    dident set_binding;
    switch (type) {
    case dist2_BINDING_RPC:
        set_binding = ec_did("set_rpc_binding", 2);
        break;

    case dist2_BINDING_EVENT:
        set_binding = ec_did("set_event_binding", 2);
        break;

    default:
        return DIST2_ERR_UNSUPPORTED_BINDING;
        break;
    }

    pword id_term = ec_long(id);
    pword binding_term = ec_long((long int) binding);
    pword set_term = ec_term(set_binding, id_term, binding_term);
    ec_post_goal(set_term);

    errval_t err = run_eclipse(dqs);
    DIST2_DEBUG("set_binding: %p\n", binding);
    DIST2_DEBUG("error: %s\n", dqs->stderr.buffer);

    free(dqs);
    return err;
}
