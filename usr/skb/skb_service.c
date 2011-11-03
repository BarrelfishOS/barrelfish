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
#include <if/skb_defs.h>
#include <skb/skb.h>

#include "dist/service.h"
#include "skb_debug.h"

#define BUFFER_SIZE (32 * 1024)

struct state {
    char output_buffer[BUFFER_SIZE];
    char error_buffer[BUFFER_SIZE];
    int output_length;
    int error_output_length;
    int exec_res;
    struct state *next;
};

static void run_done(void *arg)
{
    struct skb_binding *b = arg;
    errval_t err;
    // Head was sent successfully, free it
    struct state *head = b->st;
    struct state *point = b->st = head->next;
    assert(head);
    free(head);

    // If more state in the queue, send them
    if (point) {
        // a send just finished so this one should succeed
        err = b->tx_vtbl.
            run_response(b, MKCONT(run_done,b), point->output_buffer,
                         point->error_buffer, point->exec_res);
        assert(err_is_ok(err));
    }
}

static void run(struct skb_binding *b, char *query)
{
    int res;
    errval_t err;

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

    // Query succeeded
    free(query);
    if (!b->st) { // If queue is empty, try to send
        b->st = st;
        // queue is empty so no pending sends so send should not fail
        err = b->tx_vtbl.
            run_response(b, MKCONT(run_done,b), st->output_buffer,
                         st->error_buffer, st->exec_res);
        assert(err_is_ok(err));
    } else { // Queue is present, enqueue. For in order replies, enqueue at end
        struct state *walk = b->st;
        struct state *prev = NULL;
        while (walk) {
            prev = walk;
            walk = walk->next;
        }
        if (prev) {
            prev->next = st;
        } else {
            b->st = st;
        }        
    }
}

static struct skb_rx_vtbl rx_vtbl = {
    .run_call = run,
    .get_call = get_object,
    .set_call = set_object,
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
