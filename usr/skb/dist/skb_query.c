#include <stdio.h>
#include <string.h>
//#include <eclipse.h>

#include <barrelfish/barrelfish.h>
#include <include/skb_debug.h>

#include <if/dist_defs.h>
#include <if/dist_event_defs.h>

#include <include/skb_server.h>

#include <dist2_server/debug.h>
#include <dist2_server/query.h>
#include <dist2/parser/ast.h>

#include "code_generator.h"

static errval_t transform_ec_error(int res)
{
    errval_t err = SYS_ERR_OK;

    switch(res) {
        case PSUCCEED:
            err = SYS_ERR_OK;
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

static errval_t query_eclipse(char* query, struct dist_query_state* st, bool expect_output)
{
    //assert(query != NULL);
    assert(st != NULL);
    int res;

    st->exec_res = PFLUSHIO;
    st->output_length = 0;
    st->error_output_length = 0;

    if(query != NULL)
        ec_post_string(query);

    ec_ref Start = ec_ref_create_newvar();

    //debug_printf("query eclipse\n");
    // Process previously posted goals
    while(1) {
        st->exec_res = ec_resume1(Start);
        //debug_printf("st->exec_res is: %d\n", st->exec_res);

        switch(st->exec_res) {

            case PFLUSHIO:
                res = 0;
                do {
                    res = ec_queue_read(1, st->output_buffer + st->output_length,
                                        BUFFER_SIZE - res);
                    st->output_length += res;
                } while ((res != 0) && (st->output_length < BUFFER_SIZE));
                st->output_buffer[st->output_length] = 0;

                res = 0;
                do {
                    res = ec_queue_read(2, st->error_buffer + st->error_output_length,
                                        BUFFER_SIZE - res);
                    st->error_output_length += res;
                } while ((res != 0) &&
                            (st->error_output_length < BUFFER_SIZE));

                st->error_buffer[st->error_output_length] = 0;
            break;

            case PTHROW:
                debug_printf("skb execution got PFAIL!\n");
                assert(!"PTHROW");
            break;

            default:
                ec_ref_destroy(Start);

                return transform_ec_error(st->exec_res);
            break;
        }
    }
    ec_ref_destroy(Start);

    return transform_ec_error(st->exec_res);
}


static void debug_skb_output(struct dist_query_state* st) {
	DIST2_DEBUG(" output: %s error: %s error_code: %d\n",
	            st->output_buffer, st->error_buffer, st->exec_res);
}


errval_t get_record(struct ast_object* ast, struct dist_query_state* sqs)
{
	assert(ast != NULL);
	assert(sqs != NULL);

	struct skb_ec_terms sr;
	errval_t err = transform_record2(ast, &sr);
	if(err_is_ok(err)) {
		// Calling get_object(Name, Attrs, Constraints, Y), print_object(Y).
		dident get_object = ec_did("get_object", 4);
		pword print_var = ec_newvar();
		pword get_object_term = ec_term(get_object, sr.name, sr.attribute_list, sr.constraint_list, print_var);
		//ec_post_goal(get_object_term);
		//int res = ec_resume();
		//debug_printf("ec_resume: %d\n", res);
		pword get_print_term = ec_term(ec_did("print_object", 1), print_var);
		ec_post_goal(ec_term(ec_did(",", 2), get_object_term, get_print_term));



		err = query_eclipse(NULL, sqs, true);
		if(sqs->output_buffer[0] == '\0') {
		    err = DIST2_ERR_NO_RECORD;
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
    errval_t err = transform_record2(ast, &sr);
    if(err_is_ok(err)) {
        // Calling findall(X, get_object(X, Attrs, Constraints, _), L), print_names(L).
        dident findall = ec_did("findall", 3);
        dident get_object = ec_did("get_object", 4);
        dident print_names = ec_did("print_names", 1);

        pword var_x = ec_newvar();
        pword var_l = ec_newvar();
        pword var_attr = ec_newvar();
        pword get_object_term = ec_term(get_object, var_x, sr.attribute_list, sr.constraint_list, var_attr);
        pword findall_term = ec_term(findall, var_x, get_object_term, var_l);
        pword print_names_term = ec_term(print_names, var_l);

        ec_post_goal(findall_term);
        ec_post_goal(print_names_term);

        err = query_eclipse(NULL, dqs, true);
        if(err_is_fail(err)) {
            err_push(err, DIST2_ERR_NO_RECORD); // TODO ok here?
        }

        DIST2_DEBUG(" get_record_names:\n");
        debug_skb_output(dqs);
    }

    return err;
}


errval_t set_record(struct ast_object* ast, struct dist_query_state* sqs)
{
	assert(ast != NULL);
	assert(sqs != NULL);

	ec_ref Start = ec_ref_create_newvar();
    dident fail = ec_did("fail",0);
    ec_post_goal(ec_atom(fail));
    assert(ec_resume1(Start) == PFAIL);
    ec_ref_destroy(Start);

    struct skb_ec_terms sr;
    errval_t err = transform_record2(ast, &sr);
	if(err_is_ok(err)) {
	    // Calling add_object(Name, Attributes)
        dident add_object = ec_did("add_object", 2);
        pword add_object_term = ec_term(add_object, sr.name, sr.attribute_list);
        ec_post_goal(add_object_term);

		err = query_eclipse(NULL, sqs, true);

		DIST2_DEBUG(" set_record:\n");
		debug_skb_output(sqs);
	}

	return err;
}


errval_t del_record(struct ast_object* ast, struct dist_query_state* dqs)
{
	assert(ast != NULL);
	assert(dqs != NULL);

    struct skb_ec_terms sr;
    errval_t err = transform_record2(ast, &sr);
	if(err_is_ok(err)) {
	    // Calling del_object(Name)
	    dident del_object = ec_did("del_object", 1);
	    pword del_object_term = ec_term(del_object, sr.name);

	    ec_post_goal(del_object_term); // TODO what happens to unused sr.attributes, sr.constraints etc. :-(
        err = query_eclipse(NULL, dqs, false);

        DIST2_DEBUG(" del_record:\n");
        debug_skb_output(dqs);
	}

	return err;
}


errval_t set_trigger(enum dist_trigger_type type, struct ast_object* ast, struct dist_reply_state* drs)
{
    struct skb_ec_terms sr;
    errval_t err = transform_record2(ast, &sr);

    dident asserta = ec_did("asserta", 1);
    dident exists = ec_did("exists", 2);
    dident exists_not = ec_did("exists_not", 2);
    dident triplet = ec_did("triplet", 3);

    pword asserta_term;
    pword drs_ptr = ec_long((long int) drs);

    if(err_is_ok(err)) {
        switch(type) {
            case TRIGGER_EXISTS:
                // calling asserta(exists(Name, triplet(Attributes, Constraints, drs)))
                asserta_term = ec_term(asserta,
                               ec_term(exists, sr.name,
                               ec_term(triplet, sr.attribute_list, sr.constraint_list, drs_ptr)));
            break;

            case TRIGGER_NOT_EXISTS:
                asserta_term = ec_term(asserta,
                               ec_term(exists_not, sr.name,
                               ec_term(triplet, sr.attribute_list, sr.constraint_list, drs_ptr)));
            break;

            default:
                assert(!"Unsupported trigger type!");
            break;
        }

        ec_post_goal(asserta_term);
        err = query_eclipse(NULL, &drs->skb, false);
    }

    return err;
}


static struct dist_event_binding* get_event_binding(struct dist_binding* b)
{
	errval_t err =  SYS_ERR_OK;
	struct dist_query_state* dqs = malloc(sizeof(struct dist_query_state));
	assert(dqs != NULL);

	// Calling binding(_, X, Binding), write(X).
	dident binding = ec_did("binding", 3);
	dident write = ec_did("write", 1);
	pword bind_term = ec_long((long int) b);
	pword var_ = ec_newvar();
	pword var_x = ec_newvar();

	pword binding_term = ec_term(binding, var_, var_x, bind_term);
	pword write_term = ec_term(write, var_x);

	ec_post_goal(binding_term);
	ec_post_goal(write_term);

	err = query_eclipse(NULL, dqs, true);
	assert(err_is_ok(err)); // TODO err

	debug_skb_output(dqs);
	// TODO check error etc.

	struct dist_event_binding* recipient = NULL;
	sscanf(dqs->output_buffer, "%lu", (uintptr_t*) &recipient); // TODO

	assert(recipient != NULL); // TODO
	free(dqs);

	return recipient;
}


errval_t add_subscription(struct dist_binding* b, struct ast_object* ast, uint64_t id, struct dist_query_state* sqs)
{
    struct skb_ec_terms sr;
    errval_t err = transform_record2(ast, &sr);
	if(err_is_ok(err)) {
	    // Calling add_subscription(template(Name, Attributes) Constraints, subscriber(EventBinding, Id))
	    dident add_subscription = ec_did("add_subscription", 3);
	    dident template = ec_did("template", 2);
	    dident subscriber = ec_did("subscriber", 2);

	    pword template_term = ec_term(template, sr.name, sr.attribute_list);
	    pword subscriber_term = ec_term(subscriber, ec_long((long int)get_event_binding(b)), ec_long(id));
	    pword add_subscription_term = ec_term(add_subscription, template_term, sr.constraint_list, subscriber_term);
	    ec_post_goal(add_subscription_term);

		err = query_eclipse(NULL, sqs, false);
		DIST2_DEBUG("add_subscription\n");
		debug_skb_output(sqs);
	}

	return err;
}


errval_t del_subscription(struct dist_binding* b, uint64_t id, struct dist_query_state* sqs)
{
	errval_t err = SYS_ERR_OK;

	dident delete_subscription = ec_did("delete_subscription", 2);
	pword id_term = ec_long(id);
	pword binding_term = ec_long((long int) get_event_binding(b));
	pword delete_subscription_term = ec_term(delete_subscription, binding_term, id_term);

	ec_post_goal(delete_subscription_term);
	err = query_eclipse(NULL, sqs, false);

	DIST2_DEBUG("del_subscription:\n");
	debug_skb_output(sqs);

	return err;
}


errval_t find_subscribers(struct ast_object* ast, struct dist_query_state* sqs)
{
    struct skb_ec_terms sr;
    errval_t err = transform_record2(ast, &sr);
    // TODO error if we have constraints here?
    if(err_is_ok(err)) {
        // Calling findall(X, find_subscriber(object(Name, Attributes), X), L), write(L)
        dident findall = ec_did("findall", 3);
        dident find_subscriber = ec_did("find_subscriber", 2);
        dident object = ec_did("object", 2);
        dident write = ec_did("write", 1);
        pword var_x = ec_newvar();
        pword var_l = ec_newvar();

        pword object_term = ec_term(object, sr.name, sr.attribute_list);
        pword find_subs_term = ec_term(find_subscriber, object_term, var_x);
        pword findall_term = ec_term(findall, var_x, find_subs_term, var_l);
        pword write_term = ec_term(write, var_l);

        ec_post_goal(findall_term);
        ec_post_goal(write_term);

        err = query_eclipse(NULL, sqs, true);
    }

    DIST2_DEBUG("find_subscribers\n");
    debug_skb_output(sqs);

    return err;
}


errval_t set_events_binding(uint64_t id, struct dist_event_binding* b)
{
    errval_t err = SYS_ERR_OK;

    dident set_event_binding = ec_did("set_event_binding", 2);
    pword id_term = ec_long(id);
    pword binding_term = ec_long((long int) b);
    pword set_term = ec_term(set_event_binding, id_term, binding_term);
    ec_post_goal(set_term);

    struct dist_query_state* dqs = malloc(sizeof(struct dist_query_state));
    assert(dqs != NULL);

    err = query_eclipse(NULL, dqs, false);
    assert(err_is_ok(err));

    DIST2_DEBUG("identify_events_binding done for: %p\n", b);
    //debug_skb_output(st);

    free(dqs);
    return err;
}

// TODO same code as set_events_binding
errval_t set_rpc_binding(uint64_t id, struct dist_binding* b)
{
    errval_t err = SYS_ERR_OK;
    DIST2_DEBUG("identify_rpc_binding start: %p id:%lu\n", b, id);

    dident set_rpc_binding = ec_did("set_rpc_binding", 2);
    pword id_term = ec_long(id);
    pword binding_term = ec_long((long int) b);
    pword set_term = ec_term(set_rpc_binding, id_term, binding_term);
    ec_post_goal(set_term);

    struct dist_query_state* dqs = malloc(sizeof(struct dist_query_state));
    assert(dqs != NULL);

    err = query_eclipse(NULL, dqs, false);
    assert(err_is_ok(err));

    DIST2_DEBUG("identify_rpc_binding done for: %p\n", b);
    debug_skb_output(dqs);
    free(dqs);

    return err;
}

