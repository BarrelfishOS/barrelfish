#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/skb_events_defs.h>

#include <include/skb_debug.h>
#include <include/skb_server.h>
#include <include/queue.h>

#include "code_generator.h"
#include "service.h"

// TODO: find a more portable way, check if prolog can even retrieve unsigned long somehow
STATIC_ASSERT(sizeof(uintptr_t) <= 8, "Otherwise need to change how pointers are stored in Prolog!");


static void debug_skb_output(struct skb_query_state* st) {
	SKBD_DEBUG("st->output: %s\nerror: %s\nerror_code: %d\n", st->output_buffer, st->error_buffer, st->exec_res);
}


static void get_reply(struct skb_binding* b, struct skb_reply_state* srt) {
    errval_t err;
    err = b->tx_vtbl.get_response(b, MKCONT(free_reply_state, srt),
			                      srt->skb.output_buffer,
			                      srt->skb.error_buffer,
			                      srt->skb.exec_res);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srt);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}


void get_object(struct skb_binding *b, char *query)
{
	errval_t err = SYS_ERR_OK;

	struct parsed_object* po = transform_query(query); // TODO error?
	SKBD_DEBUG("get_object: transformed = %s\n", po->attributes.output);

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
	size_t len = 2*strlen(name) + strlen(attributes) + strlen(constraints) + strlen(format) + 1;
	char buf[len];
	snprintf(buf, len, format, name, attributes, constraints, name);

	struct skb_reply_state* srt = NULL;
	err = new_reply_state(&srt, get_reply);
	assert(err_is_ok(err)); // TODO

	err = execute_query(buf, &srt->skb);
	assert(err_is_ok(err)); // TODO

	SKBD_DEBUG("buf: %s\n", buf);
	debug_skb_output(&srt->skb);

	get_reply(b, srt);

	free(query);
	free_parsed_object(po);
}


static void set_reply(struct skb_binding* b, struct skb_reply_state* srs) {
    errval_t err;
    err = b->tx_vtbl.set_response(b, MKCONT(free_reply_state, srs),
			                      srs->skb.error_buffer,
			                      srs->skb.exec_res);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srs);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}


void set_object(struct skb_binding *b, char *query)
{
	errval_t err = SYS_ERR_OK;

	SKBD_DEBUG("set_object: query = %s\n", query);
	struct parsed_object* po = transform_query(query); // TODO error
	SKBD_DEBUG("set_object: transformed = %s\n", po->attributes.output);

	struct skb_reply_state* srs = NULL;
	err = new_reply_state(&srs, set_reply);
	assert(err_is_ok(err)); // TODO

	char* format = "add_object(%s, %s).";
	size_t len = strlen(po->name.output) + strlen(po->attributes.output) + strlen(format) + 1;
	char buf[len];
	snprintf(buf, len, format, po->name.output, po->attributes.output);
	err = execute_query(buf, &srs->skb);
	assert(err_is_ok(err)); // TODO

	SKBD_DEBUG("buf: %s\n", buf);
	debug_skb_output(&srs->skb);

	set_reply(b, srs);

	free(query);
	free_parsed_object(po);
}


static struct skb_events_binding* get_event_binding(struct skb_binding* b)
{
	errval_t err =  SYS_ERR_OK;
	struct skb_query_state st;

	char* format = "binding(_, X, %lu), write(X).";
	size_t len = strlen(format) + 20; // TODO 20
	char buf[len];
	snprintf(buf, len, format, b); // TODO check return
	err = execute_query(buf, &st);
	assert(err_is_ok(err)); // TODO err

	debug_skb_output(&st);
	// TODO check error etc.

	struct skb_events_binding* recipient = NULL;
	sscanf(st.output_buffer, "%lu", (uintptr_t*) &recipient); // TODO

	assert(recipient != NULL); // TODO
	return recipient;
}


static void subscribe_reply(struct skb_binding* b, struct skb_reply_state* srs) {
    errval_t err;
    err = b->tx_vtbl.subscribe_response(b, MKCONT(free_reply_state, srs),
			                            srs->skb.exec_res);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srs);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}


void subscribe(struct skb_binding *b, char* query, uint64_t id)
{
	errval_t err = SYS_ERR_OK;

	SKBD_DEBUG("subscribe: query = %s\n", query);
	struct parsed_object* po = transform_query(query); // TODO error
	SKBD_DEBUG("subscribe: transformed = %s\n", po->attributes.output);

	struct skb_reply_state* srs = NULL;
	err = new_reply_state(&srs, subscribe_reply);
	assert(err_is_ok(err)); // TODO


	char* format = "add_subscription(template(%s, %s), %s, subscriber(%lu, %lu)).";
	size_t len = strlen(po->name.output) + strlen(po->attributes.output) + strlen(po->constraints.output) + strlen(format) + 1 + 50; // todo 50 :-(
	char buf[len];
	snprintf(buf, len, format, po->name.output, po->attributes.output, po->constraints.output, get_event_binding(b), id);
	err = execute_query(buf, &srs->skb);
	assert(err_is_ok(err)); // TODO

	SKBD_DEBUG("buf: %s\n", buf);
	debug_skb_output(&srs->skb);

	subscribe_reply(b, srs);

	free(query);
	free_parsed_object(po);
}


static void send_subscribed_message(struct skb_events_binding* b,
		                            uint64_t id,
		                            char* object)
{
    errval_t err;
    err = b->tx_vtbl.subscribed_message(b, NOP_CONT, id, object);
}


static void publish_reply(struct skb_binding* b, struct skb_reply_state* srs) {
    errval_t err;
    err = b->tx_vtbl.publish_response(b, MKCONT(free_reply_state, srs),
			                          srs->skb.exec_res);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srs);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

void publish(struct skb_binding *b, char* object)
{
	errval_t err = SYS_ERR_OK;

	SKBD_DEBUG("publish: query = %s\n", object);
	struct parsed_object* po = transform_query(object); // TODO err?
	SKBD_DEBUG("set_object: transformed = %s\n", po->attributes.output);
	assert(strcmp(po->constraints.output, "[  ]") == 0); // TODO error if we have constraints here?

	struct skb_reply_state* srs = NULL;
	err = new_reply_state(&srs, publish_reply);
	assert(err_is_ok(err)); // TODO

	char* format = "find_subscriber(object(%s, %s), X), write(X).";
	size_t len = strlen(po->name.output) + strlen(po->attributes.output) \
			     + strlen(format) + 1;
	char buf[len];
	// TODO check return value
	snprintf(buf, len, format, po->name.output, po->attributes.output);
	err = execute_query(buf, &srs->skb);
	assert(err_is_ok(err)); // TODO

	SKBD_DEBUG("buf: %s\n", buf);
	debug_skb_output(&srs->skb);

	publish_reply(b, srs);

	struct skb_events_binding* recipient = NULL;
	uint64_t id;
	sscanf(srs->skb.output_buffer, "subscriber(%lu, %lu)", (uintptr_t*) &recipient, &id);
	SKBD_DEBUG("send msg to: %p %lu\n", recipient, id);

	send_subscribed_message(recipient, id, object);

	//free(object); TODO
	free_parsed_object(po);
}


static void identify_events_binding(struct skb_events_binding* b, uint64_t id)
{
	SKBD_DEBUG("identify_events_binding\n");
	char* format = "set_event_binding(%lu, %lu).";
	size_t len = strlen(format) + 50; // TODO maxlength of two int?
	char buf[len];
	snprintf(buf, len, format, id, b);

	struct skb_query_state st;
	errval_t err = execute_query(buf, &st);
	assert(err_is_ok(err)); // TODO

	SKBD_DEBUG("identify_events_binding DONE\n");
	debug_skb_output(&st);
}


void identify_rpc_binding(struct skb_binding* b, uint64_t id)
{
	SKBD_DEBUG("identify_rpc_binding\n");
	// duplicated code from events binding!
	char* format = "set_rpc_binding(%lu, %lu).";
	size_t len = strlen(format) + 20; // TODO maxlength of two int?
	char buf[len];
	snprintf(buf, len, format, id, b);

	assert(buf != NULL);

	struct skb_query_state st;
	printf("before exec q\n");
	errval_t err = execute_query(buf, &st);
	assert(err_is_ok(err));
	printf("after exec q\n");

	SKBD_DEBUG("buf: %s\n", buf);
	debug_skb_output(&st);

	SKBD_DEBUG("identify_rpc_binding DONE\n");
	// Returning is done by prolog predicate C function!
}


static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    // register this iref with the name service
    err = nameservice_register("skb_events", iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        abort();
    }
}

struct skb_events_rx_vtbl rx_vtbl = {
		.identify = identify_events_binding,
};


static errval_t connect_cb(void *st, struct skb_events_binding *b)
{
	// Set up continuation queue


    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}


void event_server_init(void)
{
    errval_t err;
    err = skb_events_export(NULL, export_cb, connect_cb,
    		                get_default_waitset(),
                            IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    // TODO make sure this blocks until nameservice register complete
    // to avoid race conditions?
}

