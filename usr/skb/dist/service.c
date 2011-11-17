#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h> // read list

#include <if/skb_events_defs.h>

#include <include/skb_debug.h>
#include <include/skb_server.h>
#include <include/queue.h>

#include "ast.h"

#include "code_generator.h"
#include "skb_query.h"
#include "service.h"


static void debug_skb_output(struct skb_query_state* st) {
	SKBD_DEBUG("st->output: %s\nerror: %s\nerror_code: %d\n", st->output_buffer, st->error_buffer, st->exec_res);
}


static void get_reply(struct skb_binding* b, struct skb_reply_state* srt) {
    errval_t err;
    err = b->tx_vtbl.get_response(b, MKCONT(free_reply_state, srt),
    		                      srt->skb.output_buffer, srt->skb.error_buffer,
    		                      srt->error);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srt);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}


void get_handler(struct skb_binding *b, char *query)
{
	errval_t err = SYS_ERR_OK;

	struct skb_reply_state* srt = NULL;
	err = new_reply_state(&srt, get_reply);
	assert(err_is_ok(err));

	struct ast_object* ast = NULL;
	err = generate_ast(query, &ast);
	if(err_is_ok(err)) {
		err = get_record(ast, &srt->skb);
	}

	srt->error = err;
	srt->rpc_reply(b, srt);

	free_ast(ast);
	free(query);
}


static void set_reply(struct skb_binding* b, struct skb_reply_state* srs)
{
    errval_t err;
    err = b->tx_vtbl.set_response(b, MKCONT(free_reply_state, srs),
    							  srs->skb.error_buffer, srs->error);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srs);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}


void set_handler(struct skb_binding *b, char *query)
{
	errval_t err = SYS_ERR_OK;

	struct skb_reply_state* srs = NULL;
	err = new_reply_state(&srs, set_reply);
	assert(err_is_ok(err));

	struct ast_object* ast = NULL;
	err = generate_ast(query, &ast);
	if(err_is_ok(err)) {
		err = set_record(ast, &srs->skb);
	}

	srs->error = err;
	srs->rpc_reply(b, srs);

	free_ast(ast);
	free(query);
}


static void del_reply(struct skb_binding* b, struct skb_reply_state* srs)
{
    errval_t err;
    err = b->tx_vtbl.del_response(b, MKCONT(free_reply_state, srs),
    		                      srs->error);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srs);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}


void del_handler(struct skb_binding* b, char* query)
{
	errval_t err = SYS_ERR_OK;

	struct skb_reply_state* srs = NULL;
	err = new_reply_state(&srs, del_reply);
	assert(err_is_ok(err));

	struct ast_object* ast = NULL;
	err = generate_ast(query, &ast);
	if(err_is_ok(err)) {
		err = del_record(ast, &srs->skb);
	}

	srs->error = err;
	srs->rpc_reply(b, srs);

	free_ast(ast);
	free(query);
}





static void subscribe_reply(struct skb_binding* b, struct skb_reply_state* srs)
{
    errval_t err;
    err = b->tx_vtbl.subscribe_response(b, MKCONT(free_reply_state, srs),
    		                            srs->error);

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

	struct skb_reply_state* srs = NULL;
	err = new_reply_state(&srs, subscribe_reply);
	assert(err_is_ok(err));

	struct ast_object* ast = NULL;
	err = generate_ast(query, &ast);
	if(err_is_ok(err)) {
		err = add_subscription(b, ast, id, &srs->skb);
	}

	srs->error = err;
	srs->rpc_reply(b, srs);

	free_ast(ast);
	free(query);
}


static void unsubscribe_reply(struct skb_binding* b, struct skb_reply_state* srs) {
    errval_t err;
    err = b->tx_vtbl.unsubscribe_response(b, MKCONT(free_reply_state, srs),
			                              srs->skb.exec_res);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srs);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}


void unsubscribe(struct skb_binding *b, uint64_t id)
{
	errval_t err = SYS_ERR_OK;

	SKBD_DEBUG("unsubscribe: id = %lu\n", id);

	struct skb_reply_state* srs = NULL;
	err = new_reply_state(&srs, unsubscribe_reply);
	assert(err_is_ok(err));

	err = del_subscription(b, id, &srs->skb);

	srs->error = err;
	srs->rpc_reply(b, srs);
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
	SKBD_DEBUG("publish: query = %s\n", object);
	errval_t err = SYS_ERR_OK;

	struct ast_object* ast = NULL;
	err = generate_ast(object, &ast);
	assert(err_is_ok(err));

	struct skb_record* sr = NULL;
	err = transform_record(ast, &sr);
	assert(err_is_ok(err));

	SKBD_DEBUG("set_object: transformed = %s\n", sr->attributes.output);
	assert(strcmp(sr->constraints.output, "[  ]") == 0); // TODO error if we have constraints here?

	struct skb_reply_state* srs = NULL;
	err = new_reply_state(&srs, publish_reply);
	assert(err_is_ok(err)); // TODO

	char* format = "findall(X, find_subscriber(object(%s, %s), X), L), write(L).";
	size_t len = strlen(sr->name.output) + strlen(sr->attributes.output) \
			     + strlen(format) + 1;
	char buf[len];
	// TODO check return value
	snprintf(buf, len, format, sr->name.output, sr->attributes.output);
	err = execute_query(buf, &srs->skb);
	assert(err_is_ok(err)); // TODO

	SKBD_DEBUG("buf: %s\n", buf);
	debug_skb_output(&srs->skb);

	publish_reply(b, srs);

	struct skb_events_binding* recipient = NULL;
	uint64_t id = 0;

	struct list_parser_status status;
    skb_read_list_init_offset(&status, srs->skb.output_buffer, 0);

	// Send to all subscribers
    while(skb_read_list(&status, "subscriber(%lu, %lu)", (uintptr_t*) &recipient, &id) ) {
    	SKBD_DEBUG("publish msg to: recipient:%p id:%lu\n", recipient, id);
    	send_subscribed_message(recipient, id, object);
    }

	//free(object); TODO: used by send_subscribed_object
	free_parsed_object(sr);
	free_ast(ast);
}


static void lock_reply(struct skb_binding* b, struct skb_reply_state* srs) {
    errval_t err;
    err = b->tx_vtbl.lock_response(b, MKCONT(free_reply_state, srs),
			                          srs->skb.exec_res);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srs);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}

/*
static void append_attr_ptr(struct writer* w, char* name, void* val) {
	w->pos = w->pos-2;
	if(strcmp(w->output, "[  ]") == 0) {
		emit(w, "%s::%lu ]", name, val);
	}
	else {
		emit(w, ", %s::%lu ]", name, val);
	}

	SKBD_DEBUG("append_attr_ptr w: %s\n", w->output);
}


static void append_attr_str(struct writer* w, char* name, char* val) {
	w->pos = w->pos-2;
	if(strcmp(w->output, "[  ]") == 0) {
		emit(w, "%s::'%s' ]", name, val);
	}
	else {
		emit(w, ", %s::'%s' ]", name, val);
	}
}
*/

//static uint64_t enumerator = 0;
void lock_handler(struct skb_binding* b, char* query)
{
	assert(query != NULL); // todo
	errval_t err = SYS_ERR_OK;

	struct skb_reply_state* srs = NULL;
	err = new_reply_state(&srs, lock_reply);
	assert(err_is_ok(err)); // TODO

	struct ast_object* ast = NULL;
	err = generate_ast(query, &ast);
	assert(err_is_ok(err));

	err = get_record(ast, &srs->skb);
	//append_attr_ptr(&po->attributes, "owner", b);

	if(err_no(err) == DIST2_ERR_NO_RECORD)
	{
		// Create new Lock
		SKBD_DEBUG("lock did not exist, create!\n");
		set_record(ast, &srs->skb);
		srs->rpc_reply(b, srs);
	}
	else {
		// TODO if we already have the lock reply immediately

		// Add to waiting list
		SKBD_DEBUG("lock already exist, add waiting list record!\n");
		/*char* lock_name = po->name.output;
		append_attr_str(&po->attributes, "wait_for", lock_name);

		// Generate new name
		po->name.output = NULL;
		emit(&po->name, "%s_%lu", lock_name, enumerator++);
		free(lock_name);*/

		set_record(ast, &srs->skb);
	}

	/*
	 * 	if not exists(lock_X)
	 * 		set(lock_X { owner: &binding })
	 * 		reply(&binding)
	 *  else:
	 *  	set(lock_X-{count} { wait: lock_X, owner: &binding }
	 **/

	free_ast(ast);
	free(query);
}


static void unlock_reply(struct skb_binding* b, struct skb_reply_state* srs)
{
    errval_t err;
    err = b->tx_vtbl.unlock_response(b, MKCONT(free_reply_state, srs),
			                          srs->error);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        	enqueue_reply_state(b, srs);
        	return;
        }
        USER_PANIC_ERR(err, "SKB sending %s failed!", __FUNCTION__);
    }
}


void unlock_handler(struct skb_binding* b, char* query)
{
	assert(query != NULL); // todo
	errval_t err = SYS_ERR_OK;

	struct skb_reply_state* srs = NULL;
	err = new_reply_state(&srs, unlock_reply);
	assert(err_is_ok(err)); // TODO

	struct ast_object* ast = NULL;
	err = generate_ast(query, &ast);
	assert(err_is_ok(err));

	err = get_record(ast, &srs->skb);
	if(err_is_ok(err)) {
		// TODO Check owner
		char* findChild = "_ { wait_for: '%s' }";
		size_t length = snprintf(NULL, 0, findChild, ast->on.name->in.str);
		char buf[length+1]; // TODO stack or heap?
		snprintf(buf, length+1, findChild, ast->on.name->in.str);

		struct ast_object* ast2 = NULL;
		err = generate_ast(query, &ast2);
		assert(err_is_ok(err));

		struct skb_query_state* sqs = malloc(sizeof(struct skb_query_state));

		err = get_record(ast2, sqs);
		if(err_is_ok(err)) {
			// set as new lock record
			//set_record(po, sqs);
			// TODO return lock() call of new owner
			// append_attr_ptr(po2->attributes, "owner", owner in sqs);
			del_record(ast2, sqs); // delete wait_for element
			srs->error = SYS_ERR_OK;

		} else if(err_no(err) == DIST2_ERR_NO_RECORD) {
			// No one waits for lock, we're done.
			srs->error = SYS_ERR_OK;
		} else {
			// Return to client?
			assert(!"unlock_handler unexpected error code");
		}

		free_ast(ast2);
		free(sqs);

	} else if(err_no(err) == DIST2_ERR_NO_RECORD) {
		srs->error = err_push(err, DIST2_ERR_NOT_LOCKED);
	} else {
		// Return to client?
		assert(!"unlock_handler unexpected error code");
	}
	free_ast(ast);
	free(query);

	srs->rpc_reply(b, srs);
}


static void identify_events_binding(struct skb_events_binding* b, uint64_t id)
{
	SKBD_DEBUG("identify_events_binding\n");
	char* format = "set_event_binding(%lu, %lu).";
	size_t len = strlen(format) + 50; // TODO maxlength of two int?
	char buf[len];
	snprintf(buf, len, format, id, b);

	struct skb_query_state* st = malloc(sizeof(struct skb_query_state));
	assert(st != NULL);

	errval_t err = execute_query(buf, st);
	assert(err_is_ok(err)); // TODO

	SKBD_DEBUG("identify_events_binding DONE\n");
	debug_skb_output(st);
	free(st);
}


void identify_rpc_binding(struct skb_binding* b, uint64_t id)
{
	SKBD_DEBUG("identify_rpc_binding\n");
	// duplicated code from events binding!
	char* format = "set_rpc_binding(%lu, %lu).";
	size_t len = strlen(format) + 200; // TODO maxlength of two int?
	char buf[len];
	snprintf(buf, len, format, id, b);

	assert(buf != NULL);

	struct skb_query_state* st = malloc(sizeof(struct skb_query_state));
	assert(st != NULL);
	printf("before exec q\n");
	errval_t err = execute_query(buf, st);
	assert(err_is_ok(err));
	printf("after exec q\n");

	SKBD_DEBUG("buf: %s\n", buf);
	debug_skb_output(st);

	SKBD_DEBUG("identify_rpc_binding DONE\n");
	free(st);
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

