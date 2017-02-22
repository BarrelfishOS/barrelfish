/**
 * \file
 * \brief Server part of the SKB
 *
 * This file exports the SKB functions
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

//turn on if you want to see everything which is executed on the SKB.
//this helps to create a (data) file which can be used to play on linux
//a sed script can filter these lines by checking the prefix

//#define PRINT_SKB_FILE

#define SKB_FILE_OUTPUT_PREFIX "SKB_FILE:"
#define PBUFFER_OVERLFLOW 100


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
    assert(*srs == NULL);
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


static errval_t do_listing_on_stdout(struct skb_query_state* st)
{
    assert(st != NULL);
    assert(st->output_buffer != NULL);

    char output_buffer[1024];
    st->exec_res = PFLUSHIO;
    st->output_length = 0;
    st->error_output_length = 0;

    ec_ref Start = ec_ref_create_newvar();

    // Processing
    ec_post_string("listing.");
    // Flush manually
    ec_post_goal(ec_term(ec_did("flush", 1), ec_atom(ec_did("output", 0))));
    ec_post_goal(ec_term(ec_did("flush", 1), ec_atom(ec_did("error", 0))));

    printf("========== SKB LISTING START ==========\n");
    while(st->exec_res == PFLUSHIO) {
        st->exec_res = ec_resume1(Start);

        int res = 0;
        int output_length = 0;
        do {
            res = ec_queue_read(1, output_buffer + output_length,
                              sizeof(output_buffer) - output_length);
            output_length += res;
        } while (res != 0);
        output_buffer[output_length] = '\0';
        if(output_length != 0){
            printf("%s", output_buffer);
        }
    }
    printf("========== SKB LISTING END ==========\n");

    return SYS_ERR_OK;
}


errval_t execute_query(const char* query, struct skb_query_state* st)
{
    SKB_DEBUG("Executing query: %s\n", query);
    assert(query != NULL);
    assert(st != NULL);
    assert(st->output_buffer != NULL);

    // HACK to make fact listing work, we print it directly
    // on stdout instead of returning it to the client
    if(strcmp(query,"listing") == 0){
        return do_listing_on_stdout(st);
    }

    int res;

    st->exec_res = PFLUSHIO;
    st->output_length = 0;
    st->error_output_length = 0;

    ec_ref Start = ec_ref_create_newvar();

    /* Processing */
    ec_post_string(query);
    // Flush manually
    ec_post_goal(ec_term(ec_did("flush", 1), ec_atom(ec_did("output", 0))));
    ec_post_goal(ec_term(ec_did("flush", 1), ec_atom(ec_did("error", 0))));

    int output_overflow = 0;
    int error_overflow = 0;

    while(st->exec_res == PFLUSHIO && (!output_overflow || !error_overflow)) {
        st->exec_res = ec_resume1(Start);

        res = 0;
        do {
            res = ec_queue_read(1, st->output_buffer + st->output_length,
                              sizeof(st->output_buffer) - st->output_length);
            st->output_length += res;
        } while ((res != 0) && (st->output_length < sizeof(st->output_buffer)));

        // Check for overflow
        if(st->output_length == sizeof(st->output_buffer) &&
                !output_overflow){
            debug_printf("st->output_buffer overflow. Query: %s\n", query);
            output_overflow = 1;
        }

        st->output_buffer[st->output_length] = 0;

        res = 0;
        do {
            res = ec_queue_read(2, st->error_buffer + st->error_output_length,
                            sizeof(st->error_buffer) - st->error_output_length);
            st->error_output_length += res;
        } while ((res != 0) &&
                    (st->error_output_length < sizeof(st->error_buffer)));

        // Check for overflow
        if(st->error_output_length == sizeof(st->error_buffer) &&
                !error_overflow){
            debug_printf("st->error_buffer overflow. Query: %s\n", query);
            error_overflow = 1;
        }

        st->error_buffer[st->error_output_length] = 0;
    }

    if(st->exec_res == PSUCCEED) {
        ec_cut_to_chp(Start);
        ec_resume();
    }

    ec_ref_destroy(Start);

#ifdef SKB_SERVICE_DEBUG
    if (st->exec_res) {
        debug_printf("skb exec res: %d\n", st->exec_res);
    }
    if (strlen(st->output_buffer) > 0) {
        debug_printf("skb output: %s", st->output_buffer);
    }
    if (strlen(st->error_buffer) > 0) {
        debug_printf("skb error: %s", st->error_buffer);
    }
#endif
    
    if((error_overflow || output_overflow) && st->exec_res == PSUCCEED){
        st->exec_res = PBUFFER_OVERLFLOW;
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


static void run(struct skb_binding *b, const char *query)
{
    struct skb_reply_state* srt = NULL;
    errval_t err = new_reply_state(&srt, run_reply);
    assert(err_is_ok(err)); // TODO

    err = execute_query(query, &srt->skb);
    assert(err_is_ok(err));

    run_reply(b, srt);
}


static struct skb_rx_vtbl rx_vtbl = {
    .run_call = run,
};


static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    // register this iref with the name service
    char buf[100];
    sprintf(buf, "add_object(skb, [val(iref, %"PRIu32")], []).", iref);

    struct skb_query_state* sqs = malloc(sizeof(struct skb_query_state));
    err = execute_query(buf, sqs);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice register failed");
        abort();
    }

    free(sqs);
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
