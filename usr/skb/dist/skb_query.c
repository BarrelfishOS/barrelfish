#include <stdio.h>
#include <string.h>
#include <eclipse.h>

#include <barrelfish/barrelfish.h>
#include <include/skb_debug.h>

#include <if/dist_defs.h>
#include <if/dist_event_defs.h>

#include <include/skb_server.h>

#include <dist2_server/debug.h>
#include <dist2_server/query.h>
#include <dist2/parser/ast.h>

#include "code_generator.h"

static errval_t query_eclipse(char* query, struct dist_query_state* st)
{
    assert(query != NULL);
    assert(st != NULL);
    int res;

    st->exec_res = PFLUSHIO;
    st->output_length = 0;
    st->error_output_length = 0;

    /* Processing */
    ec_post_string(query);
    while(st->exec_res == PFLUSHIO) {
        st->exec_res = ec_resume();

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
    }

    return SYS_ERR_OK;
}


static void debug_skb_output(struct dist_query_state* st) {
	DIST2_DEBUG("\nst->output: %s\nerror: %s\nerror_code: %d\n", st->output_buffer, st->error_buffer, st->exec_res);
}


errval_t get_record(struct ast_object* ast, struct dist_query_state* sqs)
{
	assert(ast != NULL);
	assert(sqs != NULL);

	struct skb_record* sr = NULL;
	errval_t err = transform_record(ast, &sr);
	if(err_is_ok(err)) {

		char* name = sr->name.output;
		if (strcmp(sr->name.output, "_") == 0) {
			name = "X";
		}
		else {
			name = sr->name.output;
		}
		char* attributes = sr->attributes.output;
		char* constraints = sr->constraints.output;

		char* format = "get_object(%s, %s, %s, Y), print_object(Y).";

		size_t len = snprintf(NULL, 0, format, name, attributes, constraints, name);
		char* buf = malloc(len+1);
		snprintf(buf, len+1, format, name, attributes, constraints, name); // TODO

		err = query_eclipse(buf, sqs);

		DIST2_DEBUG("get_record: %s\n", buf);
		debug_skb_output(sqs);

		free(buf);
		free_parsed_object(sr);
	}

	// TODO hack FIX! output seems to be undefined when goal fails
	// (in our case always the same string)
	char* hack = "nrelements(2)[[";
	if(sqs->output_buffer[0] == '\0'
	   || strncmp(sqs->output_buffer, hack, strlen(hack)) == 0) {
		return DIST2_ERR_NO_RECORD;
	}

	return err;
}


errval_t get_record_names(struct ast_object* ast, struct dist_query_state* dqs)
{
    assert(ast != NULL);
    assert(dqs != NULL);

    struct skb_record* sr = NULL;
    errval_t err = transform_record(ast, &sr);
    if(err_is_ok(err)) {

        assert(strcmp(sr->name.output, "_") == 0); // TODO
        char* attributes = sr->attributes.output;
        char* constraints = sr->constraints.output;

        char* format = "findall(X, get_object(X, %s, %s, _), L), print_names(L).";

        size_t len = snprintf(NULL, 0, format, attributes, constraints);
        char* buf = malloc(len+1);
        snprintf(buf, len+1, format, attributes, constraints);

        err = query_eclipse(buf, dqs);

        DIST2_DEBUG("get_record_names: %s\n", buf);
        debug_skb_output(dqs);

        free(buf);
        free_parsed_object(sr);
    }

    // TODO hack FIX! output seems to be undefined when goal fails
    // (in our case always the same string)
    char* hack = "nrelements(2)[[";
    if(dqs->output_buffer[0] == '\0'
       || strncmp(dqs->output_buffer, hack, strlen(hack)) == 0) {
        return DIST2_ERR_NO_RECORD;
    }

    return err;
}


errval_t set_record(struct ast_object* ast, struct dist_query_state* sqs)
{
	assert(ast != NULL);
	assert(sqs != NULL);
	errval_t err = SYS_ERR_OK;

	struct skb_record* sr = NULL;
	err = transform_record(ast, &sr);
	if(err_is_ok(err)) {
		char* format = "add_object(%s, %s).";
		size_t len = strlen(sr->name.output) + strlen(sr->attributes.output) + strlen(format) + 1;
		char buf[len]; // TODO Stack or malloc?
		snprintf(buf, len, format, sr->name.output, sr->attributes.output);

		err = query_eclipse(buf, sqs);

		DIST2_DEBUG("set_record: %s:\n", buf);
		debug_skb_output(sqs);

		free_parsed_object(sr);
	}

	return err;
}


errval_t del_record(struct ast_object* ast, struct dist_query_state* sqs)
{
	assert(ast != NULL);
	assert(sqs != NULL);
	errval_t err = SYS_ERR_OK;

	struct skb_record* sr = NULL;
	err = transform_record(ast, &sr);
	if(err_is_ok(err)) {
		char* format = "retract(object(%s, X)), write(X)."; // TODO attributes, constraints...
		size_t write_length = snprintf(NULL, 0, format, sr->name.output);
		char* buf = malloc(write_length+1);
		if(buf == NULL) {
			err = LIB_ERR_MALLOC_FAIL;
		}

		if(err_is_ok(err)) {
			size_t written = snprintf(buf, write_length+1, format, sr->name.output);
			assert(write_length == written);

			err = query_eclipse(buf, sqs);

			DIST2_DEBUG("del_record: %s:\n", buf);
			debug_skb_output(sqs);
		}

		free(buf);
		free_parsed_object(sr);
	}

	return err;
}

errval_t set_trigger(struct ast_object* ast, struct dist_query_state* dqs)
{
    errval_t err = SYS_ERR_OK;
    struct skb_record* sr = NULL;
    err = transform_record(ast, &sr);
    if(err_is_ok(err)) {
        char* format = "retract(object(%s, X)), write(X)."; // TODO attributes, constraints...
        size_t write_length = snprintf(NULL, 0, format, sr->name.output);
        char* buf = malloc(write_length+1);
        if(buf == NULL) {
            err = LIB_ERR_MALLOC_FAIL;
        }

        if(err_is_ok(err)) {
            size_t written = snprintf(buf, write_length+1, format, sr->name.output);
            assert(write_length == written);

            err = query_eclipse(buf, dqs);

            DIST2_DEBUG("del_record: %s:\n", buf);
            debug_skb_output(dqs);
        }

        free(buf);
        free_parsed_object(sr);
    }

    return err;
}


static struct dist_event_binding* get_event_binding(struct dist_binding* b)
{
	errval_t err =  SYS_ERR_OK;
	struct dist_query_state* st = malloc(sizeof(struct dist_query_state));
	assert(st != NULL);

	char* format = "binding(_, X, %lu), write(X).";
	size_t len = strlen(format) + 20; // TODO 20
	char buf[len];
	snprintf(buf, len, format, b); // TODO check return
	err = query_eclipse(buf, st);
	assert(err_is_ok(err)); // TODO err

	debug_skb_output(st);
	// TODO check error etc.

	struct dist_event_binding* recipient = NULL;
	sscanf(st->output_buffer, "%lu", (uintptr_t*) &recipient); // TODO

	assert(recipient != NULL); // TODO
	free(st);
	return recipient;
}


errval_t add_subscription(struct dist_binding* b, struct ast_object* ast, uint64_t id, struct dist_query_state* sqs)
{
	errval_t err = SYS_ERR_OK;

	struct skb_record* sr = NULL;
	err = transform_record(ast, &sr);
	if(err_is_ok(err)) {
		char* format = "add_subscription(template(%s, %s), %s, subscriber(%lu, %lu)).";
		size_t len = snprintf(NULL, 0, format, sr->name.output,
				              sr->attributes.output, sr->constraints.output,
				              get_event_binding(b), id);

		char buf[len]; // TODO heap?
		snprintf(buf, len, format, sr->name.output, sr->attributes.output,
				 sr->constraints.output, get_event_binding(b), id);
		err = query_eclipse(buf, sqs);

		DIST2_DEBUG("add_subscription: %s\n", buf);
		debug_skb_output(sqs);

		free_parsed_object(sr);
	}

	return err;
}


errval_t del_subscription(struct dist_binding* b, uint64_t id, struct dist_query_state* sqs)
{
	errval_t err = SYS_ERR_OK;

	char* format = "delete_subscription(%lu, %lu).";
	size_t len = strlen(format) + 50 + 1; // todo 50 :-(
	char buf[len];
	snprintf(buf, len, format, get_event_binding(b), id);
	err = query_eclipse(buf, sqs);

	DIST2_DEBUG("del_subscription: %s\n", buf);
	debug_skb_output(sqs);

	return err;
}


errval_t find_subscribers(struct ast_object* ast, struct dist_query_state* sqs) {

    errval_t err = SYS_ERR_OK;

    struct skb_record* sr = NULL;
    err = transform_record(ast, &sr);
    if(err_is_fail(err)) {
        return err;
    }

    assert(strcmp(sr->constraints.output, "[  ]") == 0); // TODO error if we have constraints here?

    char* format = "findall(X, find_subscriber(object(%s, %s), X), L), write(L).";
    size_t len = snprintf(NULL, 0, format, sr->name.output, sr->attributes.output);
    char* buf = malloc(len+1);
    snprintf(buf, len+1, format, sr->name.output, sr->attributes.output);

    err = query_eclipse(buf, sqs);

    DIST2_DEBUG("buf: %s\n", buf);
    debug_skb_output(sqs);

    free_parsed_object(sr);
    free(buf);

    return err;
}


errval_t set_events_binding(uint64_t id, struct dist_event_binding* b)
{
    errval_t err = SYS_ERR_OK;

    char* format = "set_event_binding(%lu, %lu).";
    size_t len = strlen(format) + 50; // TODO maxlength of two int?
    char buf[len];
    snprintf(buf, len, format, id, b);

    struct dist_query_state* st = malloc(sizeof(struct dist_query_state));
    assert(st != NULL);

    err = query_eclipse(buf, st);
    assert(err_is_ok(err)); // TODO

    DIST2_DEBUG("identify_events_binding done %s for: %p\n", buf, b);
    debug_skb_output(st);


    free(st);

    return err;
}


errval_t set_rpc_binding(uint64_t id, struct dist_binding* b)
{
    errval_t err = SYS_ERR_OK;

    DIST2_DEBUG("identify_rpc_binding start: %p id:%lu\n", b, id);
    // duplicated code from events binding!
    char* format = "set_rpc_binding(%lu, %lu).";
    size_t len = strlen(format) + 200; // TODO maxlength of two int?
    char buf[len];
    snprintf(buf, len, format, id, b);

    assert(buf != NULL);

    struct dist_query_state* st = malloc(sizeof(struct dist_query_state));
    assert(st != NULL);
    err = query_eclipse(buf, st);
    assert(err_is_ok(err));

    DIST2_DEBUG("identify_rpc_binding done for: %p\n", b);
    debug_skb_output(st);

    free(st);
    return err;
}

