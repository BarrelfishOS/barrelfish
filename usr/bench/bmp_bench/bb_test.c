/** \file
 *  \brief IDC system test code
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN /* for strdup() */
#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/debug.h>
#include <bench/bench.h>
#include <if/test_defs.h>

#include "bmpbench.h"

static const char *my_service_name = "bmpbench-test";

/* ------------------------ COMMON MESSAGE HANDLERS ------------------------ */

static void rx_basic(struct test_binding *b, uint32_t arg)
{
    //printf("rx_basic %u\n", arg);
}

static struct test_rx_vtbl rx_vtbl = {
    .basic = rx_basic,
};

/* ------------------------------ CLIENT ------------------------------ */

#define REPEAT_BASIC 100

struct client_state {
    int nextmsg;
};

cycles_t times[REPEAT_BASIC+1];

// send the next message in our sequence
static void send_cont(void *arg)
{
    struct test_binding *b = arg;
    struct client_state *myst = b->st;
    errval_t err;

    //printf("client sending msg %d\n", myst->nextmsg);

    times[myst->nextmsg] = bench_tsc();
    if (myst->nextmsg < REPEAT_BASIC) {
	err = b->tx_vtbl.basic(b, MKCONT(send_cont, b), myst->nextmsg);
    }
    else {
	printf("client all done\n");
	for(int i=0; i<REPEAT_BASIC; i++)
	    printf("%u\n", times[i+1] - times[i]);
	return;
    }

    if (err_is_ok(err)) {
        myst->nextmsg++;
    } else {
        DEBUG_ERR(err, "error sending message %d", myst->nextmsg);
        abort();
    }
}

static void bind_cb(void *st, errval_t err, struct test_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }

    printf("client bound!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // construct local per-binding state
    struct client_state *myst = malloc(sizeof(struct client_state));
    assert(myst != NULL);
    myst->nextmsg = 0;
    b->st = myst;

    // start sending stuff to the service
    send_cont(b);
}

void bb_test_start_client(void)
{
    iref_t iref;
    errval_t err;

    printf("client looking up '%s' in name service...\n", my_service_name);
    err = nameservice_blocking_lookup(my_service_name, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_blocking_lookup failed");
        abort();
    }

    printf("client binding to %u...\n", iref);
    err = test_bind(iref, bind_cb, NULL /* state pointer for bind_cb */,
                    get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }
}

/* ------------------------------ SERVER ------------------------------ */

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    printf("service exported at iref %u\n", iref);

    // register this iref with the name service
    err = nameservice_register(my_service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        abort();
    }
}

static errval_t connect_cb(void *st, struct test_binding *b)
{
    printf("service got a connection!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}

void bb_test_start_server(void)
{
    errval_t err;

    err = test_export(NULL /* state pointer for connect/export callbacks */,
                      export_cb, connect_cb, get_default_waitset(),
                      IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }
}

