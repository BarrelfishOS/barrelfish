/**
 * \file
 * \brief Server part of the SKB
 *
 * This file exports the SKB functions
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

//turn on if you want to see everything which is executed on the SKB.
//this helps to create a (data) file which can be used to play on linux
//a sed script can filter these lines by checking the prefix

//#define PRINT_SKB_FILE

#define SKB_FILE_OUTPUT_PREFIX "SKB_FILE:"


#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <eclipse.h>
#include <include/skb_server.h>

#include <stdlib.h>
#include <stdio.h>
#include <skb/skb.h>

#include "queue.h"
#include "dist/service.h"
#include "skb_debug.h"

/*
void enqueue_state(struct skb_binding *b, struct state* st)
{
    struct state** walk = (struct state**) &(b->st);
    for(; *walk != NULL; walk = &(*walk)->next) {
    	// continue
    }
    *walk = st;
}

struct state* dequeue_state(struct skb_binding *b)
{
    // Head was sent successfully, free it
    struct state *head = b->st;
    struct state *point = b->st = head->next;
    assert(head);
    free(head);

    return point;
}


static void run_done(void *arg)
{
    struct skb_binding *b = arg;
    errval_t err;

    struct state* point = dequeue_state(b);
    // If more state in the queue, send them
    if (point) {
        // a send just finished so this one should succeed
        err = b->tx_vtbl.
            run_response(b, MKCONT(run_done,b), point->output_buffer,
                         point->error_buffer, point->exec_res);
        assert(err_is_ok(err));
    }
}*/


struct state* execute_query(char* query)
{
	SKB_DEBUG("execute query: %s\n", query);
	assert(query != NULL);

	int res;

    // Allocate the state for continuation
    struct state *st = malloc(sizeof(struct state));
    assert(st);
    st->exec_res = PFLUSHIO;
    st->output_length = 0;
    st->error_output_length = 0;
    st->next = NULL;

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

    return st;
}


static void run_handler(struct skb_binding *b,
                               struct skb_msg_queue_elem *e);


struct run_request_state {
    struct skb_msg_queue_elem elem;
    struct skb_run_response__args args;
};


static void run_cont(struct skb_binding* skb_closure,
		             char* output, char* error, int32_t error_code)
{
    errval_t err;

    /* Send the request to the monitor on the server's core */
    err = skb_closure->tx_vtbl.
          run_response(skb_closure, NOP_CONT, output, error, error_code);

    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct run_request_state *me =
                malloc(sizeof(struct run_request_state));
            struct skb_queue_state *ist = skb_closure->st;
            me->args.str_error = error;
            me->args.int_error = error_code;
            me->args.output = output;
            me->elem.cont = run_handler;

            err = skb_enqueue_send(skb_closure, &ist->queue,
                                   get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        USER_PANIC_ERR(err, "SKB sending get_object response failed!");
    }
}


static void run_handler(struct skb_binding *b,
                        struct skb_msg_queue_elem *e)
{
    struct run_request_state *st = (struct run_request_state *)e;
    run_cont(b, st->args.output, st->args.str_error,
             st->args.int_error);
    free(e);
}


static void run(struct skb_binding *b, char *query)
{
	struct state* st = execute_query(query);
    // Query succeeded
    free(query);

    run_cont(b, st->output_buffer, st->error_buffer, st->exec_res);
}


static struct skb_rx_vtbl rx_vtbl = {
    .run_call = run,
    .get_call = get_object,
    .set_call = set_object,
    .subscribe_call = subscribe,
    .publish_call = publish,
    .identify_call = identify_rpc_binding,
};

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    // register this iref with the name service
    err = nameservice_register("skb", iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        abort();
    }
}

static errval_t connect_cb(void *st, struct skb_binding *b)
{
	// Set up continuation queue
	struct skb_queue_state *sqt = malloc(sizeof(struct skb_queue_state));
    assert(sqt != NULL);
    sqt->queue.head = sqt->queue.tail = NULL;
    b->st = sqt;

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}

void skb_server_init(void)
{
    errval_t err;
    err = skb_export(NULL, export_cb, connect_cb, get_default_waitset(),
                     IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));
}
