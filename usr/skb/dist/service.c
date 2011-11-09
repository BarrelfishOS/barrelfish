#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/skb_events_defs.h>

#include <include/skb_server.h>

#include "code_generator.h"
#include "service.h"
#include "../skb_debug.h"
#include "../queue.h"

// TODO: find a more portable way, check if prolog can even retrieve unsigned long somehow
STATIC_ASSERT(sizeof(uintptr_t) <= 8, "Otherwise need to change how pointers are stored in Prolog!");

static void get_object_handler(struct skb_binding *b,
                               struct skb_msg_queue_elem *e);


struct get_object_request_state {
    struct skb_msg_queue_elem elem;
    struct skb_get_response__args args;
};


static void get_object_cont(struct skb_binding* skb_closure,
		                    char* output, char* error, int32_t error_code)
{
    errval_t err;

    /* Send the request to the monitor on the server's core */
    err = skb_closure->tx_vtbl.
          get_response(skb_closure, NOP_CONT, output, error, error_code);

    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct get_object_request_state *me =
                malloc(sizeof(struct get_object_request_state));
            struct skb_queue_state *ist = skb_closure->st;
            me->args.error = error;
            me->args.error_code = error_code;
            me->args.output = output;
            me->elem.cont = get_object_handler;

            err = skb_enqueue_send(skb_closure, &ist->queue,
                                   get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "SKB sending get_object response failed!");
    }
}


static void get_object_handler(struct skb_binding *b,
                               struct skb_msg_queue_elem *e)
{
    struct get_object_request_state *st = (struct get_object_request_state *)e;
    get_object_cont(b, st->args.output, st->args.error,
                    st->args.error_code);
    free(e);
}


void get_object(struct skb_binding *b, char *query)
{
	SKBD_DEBUG("get_object: query = %s\n", query);

	struct parsed_object* po = transform_query(query);
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
	struct state* st = execute_query(buf); // TODO free

	SKBD_DEBUG("buf: %s\n", buf);
	SKBD_DEBUG("get_object: st->output: %s error: %s\n", st->output_buffer, st->error_buffer);

	get_object_cont(b, st->output_buffer, st->error_buffer, st->exec_res);

	free(query);

	free(po->name.output);
	free(po->attributes.output);
	free(po->constraints.output);
	free(po);
}



static void set_object_handler(struct skb_binding *b,
                               struct skb_msg_queue_elem *e);


struct set_object_request_state {
    struct skb_msg_queue_elem elem;
    struct skb_set_response__args args;
};


static void set_object_cont(struct skb_binding* skb_closure,
		                    char* error, int32_t error_code)
{
    errval_t err;

    /* Send the request to the monitor on the server's core */
    err = skb_closure->tx_vtbl.
                set_response(skb_closure, NOP_CONT, error, error_code);

    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct set_object_request_state *me =
                malloc(sizeof(struct set_object_request_state));
            struct skb_queue_state *ist = skb_closure->st;
            me->args.error = error;
            me->args.error_code = error_code;
            me->elem.cont = set_object_handler;

            err = skb_enqueue_send(skb_closure, &ist->queue,
                                   get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "SKB sending set_object response failed!");
    }
}


static void set_object_handler(struct skb_binding *b,
                               struct skb_msg_queue_elem *e)
{
    struct set_object_request_state *st = (struct set_object_request_state *)e;
    set_object_cont(b, st->args.error, st->args.error_code);
    free(e);
}



void set_object(struct skb_binding *b, char *query)
{
	SKBD_DEBUG("set_object: query = %s\n", query);
	struct parsed_object* po = transform_query(query);
	SKBD_DEBUG("set_object: transformed = %s\n", po->attributes.output);

	char* format = "add_object(%s, %s).";
	size_t len = strlen(po->name.output) + strlen(po->attributes.output) + strlen(format) + 1;

	char buf[len];
	snprintf(buf, len, format, po->name.output, po->attributes.output);
	struct state* st = execute_query(buf); // TODO free

	SKBD_DEBUG("buf: %s\n", buf);
	SKBD_DEBUG("set_object: execute result: %s error: %s\n", st->output_buffer, st->error_buffer);

	set_object_cont(b, st->error_buffer, st->exec_res);

	free(query);
	free(po->name.output);
	free(po->attributes.output);
	free(po->constraints.output);
	free(po);
}


static void subscribe_handler(struct skb_binding *b,
                              struct skb_msg_queue_elem *e);


struct subscribe_request_state {
    struct skb_msg_queue_elem elem;
    struct skb_subscribe_response__args args;
};


static void subscribe_cont(struct skb_binding* skb_closure,
		                    errval_t error_code)
{
    errval_t err;

    /* Send the request to the monitor on the server's core */
    err = skb_closure->tx_vtbl.
                subscribe_response(skb_closure, NOP_CONT, error_code);

    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct subscribe_request_state *me =
                malloc(sizeof(struct subscribe_request_state));
            struct skb_queue_state *ist = skb_closure->st;
            me->args.err = error_code;
            me->elem.cont = subscribe_handler;

            err = skb_enqueue_send(skb_closure, &ist->queue,
                                   get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "SKB sending set_object response failed!");
    }
}


static void subscribe_handler(struct skb_binding *b,
                               struct skb_msg_queue_elem *e)
{
    struct subscribe_request_state *st = (struct subscribe_request_state *)e;
    subscribe_cont(b, st->args.err);
    free(e);
}

static struct skb_events_binding* get_event_binding(struct skb_binding* b)
{
	char* format = "binding(_, X, %lu), write(X).";
	size_t len = strlen(format) + 20; // TODO
	char buf[len];
	snprintf(buf, len, format, b); // TODO check return
	struct state* st = execute_query(buf);

	SKBD_DEBUG("get_event_binding: execute result: %s error: %s\n", st->output_buffer, st->error_buffer);

	// TODO check error etc.

	struct skb_events_binding* recipient = NULL;
	sscanf(st->output_buffer, "%lu", (uintptr_t*) &recipient); // TODO


	free(st);

	assert(recipient != NULL);
	return recipient;
}

void subscribe(struct skb_binding *b, char* query, uint64_t id)
{
	SKBD_DEBUG("subscribe: query = %s\n", query);
	struct parsed_object* po = transform_query(query);
	SKBD_DEBUG("subscribe: transformed = %s\n", po->attributes.output);

	char* format = "add_subscription(template(%s, %s), %s, subscriber(%lu, %lu)).";
	size_t len = strlen(po->name.output) + strlen(po->attributes.output) + strlen(po->constraints.output) + strlen(format) + 1 + 50; // todo 50 :-(
	char buf[len];
	snprintf(buf, len, format, po->name.output, po->attributes.output, po->constraints.output, get_event_binding(b), id);
	struct state* st = execute_query(buf); // TODO free

	SKBD_DEBUG("buf: %s\n", buf);
	SKBD_DEBUG("set_object: execute result: %s error: %s\n", st->output_buffer, st->error_buffer);


	subscribe_cont(b, st->exec_res);

	free(query);
	free(po->name.output);
	free(po->attributes.output);
	free(po->constraints.output);
	free(po);
}

/*
static void subscribed_message_handler(struct skb_binding *b,
                                       struct skb_msg_queue_elem *e);


struct subscribed_message_state {
    struct skb_msg_queue_elem elem;
    struct skb_subscribed_message__args args;
};*/


static void send_subscribed_message(struct skb_events_binding* b,
		                            uint64_t id,
		                            char* object)
{
    errval_t err;

    /* Send the request to the monitor on the server's core */
    err = b->tx_vtbl.subscribed_message(b, NOP_CONT, id, object);
/*
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct subscribed_message_state* me =
                malloc(sizeof(struct subscribed_message_state));
            struct skb_queue_state *ist = b->st;
            me->args.id = id;
            me->args.object = object;
            me->elem.cont = subscribed_message_handler;

            err = skb_enqueue_send(b, &ist->queue,
                                   get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "SKB sending set_object response failed!");
    }*/
}
/*

static void subscribed_message_handler(struct skb_binding *b,
                               struct skb_msg_queue_elem *e)
{
    struct subscribed_message_state* st =
    		(struct subscribed_message_state*) e;
    send_subscribed_message(b, st->args.id, st->args.object);
    free(e);
}*/


static void publish_handler(struct skb_binding *b,
                            struct skb_msg_queue_elem *e);


struct publish_request_state {
    struct skb_msg_queue_elem elem;
    struct skb_publish_response__args args;
};


static void publish_cont(struct skb_binding* skb_closure,
		                    errval_t error_code)
{
    errval_t err;

    /* Send the request to the monitor on the server's core */
    err = skb_closure->tx_vtbl.
                publish_response(skb_closure, NOP_CONT, error_code);

    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct publish_request_state *me =
                malloc(sizeof(struct publish_request_state));
            struct skb_queue_state *ist = skb_closure->st;
            me->args.err = error_code;
            me->elem.cont = publish_handler;

            err = skb_enqueue_send(skb_closure, &ist->queue,
                                   get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "SKB sending set_object response failed!");
    }
}


static void publish_handler(struct skb_binding *b,
                               struct skb_msg_queue_elem *e)
{
    struct publish_request_state *st = (struct publish_request_state *)e;
    publish_cont(b, st->args.err);
    free(e);
}


void publish(struct skb_binding *b, char* object)
{
	SKBD_DEBUG("publish: query = %s\n", object);
	struct parsed_object* po = transform_query(object);
	SKBD_DEBUG("set_object: transformed = %s\n", po->attributes.output);
	// TODO error if we have constraints here
	assert(strcmp(po->constraints.output, "[  ]") == 0);
	//printf("po->constraints.output: %s\n", po->constraints.output);

	char* format = "find_subscriber(object(%s, %s), X), write(X).";
	size_t len = strlen(po->name.output) + strlen(po->attributes.output) \
			     + strlen(format) + 1;
	char buf[len];
	// TODO check return value
	snprintf(buf, len, format, po->name.output, po->attributes.output);
	struct state* st = execute_query(buf); // TODO free state

	SKBD_DEBUG("buf: %s\n", buf);
	SKBD_DEBUG("publish: execute result: %s error: %s\n",
			   st->output_buffer, st->error_buffer);

	publish_cont(b, st->exec_res);

	struct skb_events_binding* recipient = NULL;
	uint64_t id;
	sscanf(st->output_buffer, "subscriber(%lu, %lu)", (uintptr_t*) &recipient, &id);
	SKBD_DEBUG("send msg to: %p %lu\n", recipient, id);

	send_subscribed_message(recipient, id, object);

	//free(object); TODO
	free(po->name.output);
	free(po->attributes.output);
	free(po->constraints.output);
	free(po);
}


static void identify_events_binding(struct skb_events_binding* b, uint64_t id)
{
	char* format = "set_event_binding(%lu, %lu).";
	size_t len = strlen(format) + 50; // TODO maxlength of two int?
	char buf[len];
	snprintf(buf, len, format, id, b);

	struct state* st = execute_query(buf);
	SKBD_DEBUG("identify_events_binding:" \
			   "execute result: %s error: %s\n",
			   st->output_buffer, st->error_buffer);

	free(st);
}


void identify_rpc_binding(struct skb_binding* b, uint64_t id)
{
	SKBD_DEBUG("identify_rpc_binding\n");
	// duplicated code from events binding!
	char* format = "set_rpc_binding(%lu, %lu).";
	size_t len = strlen(format) + 20; // TODO maxlength of two int?
	char buf[len];
	snprintf(buf, len, format, id, b);
	SKBD_DEBUG("identify_rpc_binding before execute\n");
	assert(buf != NULL);
	SKBD_DEBUG("buf: %s\n", buf);
	struct state* st = execute_query(buf);
	assert(st != NULL);
	SKBD_DEBUG("identify_rpc_binding:" \
			   "execute result: %s error: %s\n",
			   st->output_buffer, st->error_buffer);

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

