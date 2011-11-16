#include <stdio.h>
#include <string.h>
#include <eclipse.h>

#include <include/skb_debug.h>

#include "code_generator.h"
#include "skb_query.h"


static void debug_skb_output(struct skb_query_state* st) {
	SKBD_DEBUG("\nst->output: %s\nerror: %s\nerror_code: %d\n", st->output_buffer, st->error_buffer, st->exec_res);
}


errval_t get_record(struct parsed_object* po, struct skb_query_state* sqs)
{
	assert(po != NULL);
	assert(sqs != NULL);

	char* name = po->name.output;
	if (strcmp(po->name.output, "_") == 0) {
		name = "X";
	}
	else {
		name = po->name.output;
	}
	char* attributes = po->attributes.output;
	char* constraints = po->constraints.output;

	char* format = "get_object(%s, %s, %s, Y), write(%s), write(' '), write(Y).";

	size_t len = snprintf(NULL, 0, format, name, attributes, constraints, name);
	char* buf = malloc(len+1);
	snprintf(buf, len+1, format, name, attributes, constraints, name); // TODO

	errval_t err = execute_query(buf, sqs);
	assert(err_is_ok(err)); // TODO
	SKBD_DEBUG("get_record: %s output was:\n", buf);
	debug_skb_output(sqs);

	free(buf);


	if(sqs->output_buffer[0] == '\0') {
		return DIST2_ERR_NO_RECORD;
	}

	return err;
}


errval_t set_record(struct parsed_object* po, struct skb_query_state* sqs)
{
	assert(po != NULL);
	assert(sqs != NULL);
	errval_t err = SYS_ERR_OK;

	char* format = "add_object(%s, %s), write('added').";
	size_t len = strlen(po->name.output) + strlen(po->attributes.output) + strlen(format) + 1;
	char buf[len];
	snprintf(buf, len, format, po->name.output, po->attributes.output);

	err = execute_query(buf, sqs);
	assert(err_is_ok(err)); // TODO

	SKBD_DEBUG("set_record: %s output was:\n", buf);
	debug_skb_output(sqs);

	return err;
}


errval_t del_record(struct parsed_object* po, struct skb_query_state* sqs)
{
	assert(po != NULL);
	assert(sqs != NULL);
	errval_t err = SYS_ERR_OK;

	char* format = "retract(object(%s, X)), write(X)."; // TODO attributes, constraints...
	size_t write_length = snprintf(NULL, 0, format, po->name.output);

	char* buf = malloc(write_length+1);
	if(buf == NULL) {
		return LIB_ERR_MALLOC_FAIL;
	}

	size_t written = snprintf(buf, write_length+1, format, po->name.output);
	assert(write_length == written);

	err = execute_query(buf, sqs);
	//ec_post_string(buf);
	assert(err_is_ok(err)); // TODO

	SKBD_DEBUG("del_record: %s output was:\n", buf);
	debug_skb_output(sqs);

	free(buf);
	return err;
}
