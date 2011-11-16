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
#include <stdlib.h>
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <eclipse.h>
#include <include/skb_server.h>
#include <include/skb_debug.h>
#include <include/queue.h>

#include <skb/skb.h>


errval_t new_reply_state(struct skb_reply_state** srs, rpc_reply_handler_fn reply_handler)
{
	*srs = malloc(sizeof(struct skb_reply_state));
	if(*srs == NULL) {
		return LIB_ERR_MALLOC_FAIL;
	}
	memset(*srs, 0, sizeof(struct skb_reply_state));

	(*srs)->rpc_reply = reply_handler;
	(*srs)->next = NULL;

	return SYS_ERR_OK;
}


void free_reply_state(void* arg) {
	if(arg != NULL) {
		struct skb_reply_state* srt = (struct skb_reply_state*) arg;
		free(srt);
	}
	else {
		assert(!"free_reply_state with NULL argument?");
	}
}


errval_t execute_query(char* query, struct skb_query_state* st)
{
	assert(query != NULL);
    assert(st != NULL);
	SKBD_DEBUG("execute query: %s\n", query);

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


static void run_reply(struct skb_binding* b, struct skb_reply_state* srt) {
    errval_t err;
    err = b->tx_vtbl.run_response(b, MKCONT(free_reply_state, srt),
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


static void run(struct skb_binding *b, char *query)
{
	struct skb_reply_state* srt = NULL;
	errval_t err = new_reply_state(&srt, run_reply);
	assert(err_is_ok(err)); // TODO

	err = execute_query(query, &srt->skb);
	assert(err_is_ok(err));

    run_reply(b, srt);
	free(query);
}


static struct skb_rx_vtbl rx_vtbl = {
    .run_call = run,
    .get_call = get_handler,
    .set_call = set_handler,
    .del_call = del_handler,
    .subscribe_call = subscribe,
    .publish_call = publish,
    .identify_call = identify_rpc_binding,
    .unsubscribe_call = unsubscribe,
    .lock_call = lock_handler,
    .unlock_call = unlock_handler,
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
    b->st = NULL;

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
