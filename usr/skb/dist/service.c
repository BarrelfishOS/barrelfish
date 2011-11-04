#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <include/skb_server.h>
#include "code_generator.h"
#include "service.h"
#include "../skb_debug.h"
#include "../queue.h"


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

	struct parsed_object* po = transform_query(query);

	SKBD_DEBUG("get_object: query = %s\n", query);
	SKBD_DEBUG("get_object: transformed = %s\n", po->attributes.output);

	char* attributes = po->attributes.output;
	if(po->attributes.output == NULL) {
		attributes = "X";
	}

	char* format = "get_object(%s, %s), write(X).";
	size_t len = strlen(po->name.output) + strlen(attributes) + strlen(format) + 1;

	char buf[len];
	snprintf(buf, len, format, po->name.output, attributes);
	struct state* st = execute_query(buf);

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
	struct state* st = execute_query(buf);

	SKBD_DEBUG("buf: %s\n", buf);
	SKBD_DEBUG("set_object: execute result: %s error: %s\n", st->output_buffer, st->error_buffer);

	set_object_cont(b, st->error_buffer, st->exec_res);

	free(query);

	free(po->name.output);
	free(po->attributes.output);
	free(po->constraints.output);
	free(po);
}
