#include <stdio.h>
#include <string.h>
#include <eclipse.h>

#include <include/skb_debug.h>

#include "code_generator.h"
#include "ast.h"

#include "skb_query.h"


static void debug_skb_output(struct skb_query_state* st) {
	SKBD_DEBUG("\nst->output: %s\nerror: %s\nerror_code: %d\n", st->output_buffer, st->error_buffer, st->exec_res);
}


errval_t get_record(struct ast_object* ast, struct skb_query_state* sqs)
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

		err = execute_query(buf, sqs);

		SKBD_DEBUG("get_record: %s\n", buf);
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


errval_t set_record(struct ast_object* ast, struct skb_query_state* sqs)
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

		err = execute_query(buf, sqs);

		SKBD_DEBUG("set_record: %s:\n", buf);
		debug_skb_output(sqs);

		free_parsed_object(sr);
	}

	return err;
}


errval_t del_record(struct ast_object* ast, struct skb_query_state* sqs)
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

			err = execute_query(buf, sqs);

			SKBD_DEBUG("del_record: %s:\n", buf);
			debug_skb_output(sqs);
		}

		free(buf);
		free_parsed_object(sr);
	}

	return err;
}


static struct skb_events_binding* get_event_binding(struct skb_binding* b)
{
	errval_t err =  SYS_ERR_OK;
	struct skb_query_state* st = malloc(sizeof(struct skb_query_state));
	assert(st != NULL);

	char* format = "binding(_, X, %lu), write(X).";
	size_t len = strlen(format) + 20; // TODO 20
	char buf[len];
	snprintf(buf, len, format, b); // TODO check return
	err = execute_query(buf, st);
	assert(err_is_ok(err)); // TODO err

	debug_skb_output(st);
	// TODO check error etc.

	struct skb_events_binding* recipient = NULL;
	sscanf(st->output_buffer, "%lu", (uintptr_t*) &recipient); // TODO

	assert(recipient != NULL); // TODO
	free(st);
	return recipient;
}


errval_t add_subscription(struct skb_binding* b, struct ast_object* ast, uint64_t id, struct skb_query_state* sqs)
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
		err = execute_query(buf, sqs);

		SKBD_DEBUG("add_subscription: %s\n", buf);
		debug_skb_output(sqs);

		free_parsed_object(sr);
	}

	return err;
}


errval_t del_subscription(struct skb_binding* b, uint64_t id, struct skb_query_state* sqs)
{
	errval_t err = SYS_ERR_OK;

	char* format = "delete_subscription(%lu, %lu).";
	size_t len = strlen(format) + 50 + 1; // todo 50 :-(
	char buf[len];
	snprintf(buf, len, format, get_event_binding(b), id);
	err = execute_query(buf, sqs);

	SKBD_DEBUG("del_subscription: %s\n", buf);
	debug_skb_output(sqs);

	return err;
}
