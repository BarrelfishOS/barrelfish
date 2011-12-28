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
#include <if/ping_pong_defs.h>

#include <trace/trace.h>
#ifdef __BEEHIVE__
#include <simctrl.h>
#else
#define BEE_SIMCTRL(x) (void)0
#endif
#include "bmpbench.h"

static const char *my_service_name = "bmpbench-pingpong";

/* ------------------------ COMMON MESSAGE HANDLERS ------------------------ */

static void send_cont(void *);

static void rx_ping(struct ping_pong_binding *b, uint64_t val)
{
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_RXPING, val); 
  //printf("rx_ping %" PRIu64 "\n", val);
    b->tx_vtbl.pong(b, NOP_CONT, val);
}

static void rx_pong(struct ping_pong_binding *b, uint64_t val)
{
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_RXPONG, val);
  //printf("rx_pong %" PRIu64 "\n", val);
    send_cont(b);
}

static void rx_slow_op(struct ping_pong_binding *b, uint64_t val)
{
    BEE_SIMCTRL(BEE_SIMCTRL_CACHE_STAT);
    if (val >= 100) {
	printf("rx_slow_op: %" PRIu64 "\n", val);
    }
    else {
	val = icachetest_run((int)val);
    }
    b->tx_vtbl.slow_reply(b, NOP_CONT, val);
}

static void rx_slow_reply(struct ping_pong_binding *b, uint64_t val)
{
    printf("rx_slow_reply %" PRIu64 "\n", val);
    printf("rx_slow_reply locally %lu\n", icachetest_run(20));
}

static void rx_stop(struct ping_pong_binding *b)
{
    printf("rx_stop:\n");
}

static void rx_testrpc_call(struct ping_pong_binding *b, uint64_t testin)
{
    printf("rx_testrpc_call: %" PRIu64 "\n", testin);
    b->tx_vtbl.testrpc_response(b, NOP_CONT, testin);
}

static void rx_testrpc_response(struct ping_pong_binding *b, uint64_t testout)
{
    printf("rx_testrpc_response: %" PRIu64 "\n", testout);
}

static void rx_outoforder_call(struct ping_pong_binding *b,
			       uint64_t seq_in, uint64_t testin)
{
    printf("rx_outoforder_call: %" PRIu64 " %" PRIu64 "\n", seq_in, testin);
    b->tx_vtbl.outoforder_response(b, NOP_CONT, seq_in, testin);
}

static void rx_outoforder_response(struct ping_pong_binding *b,
				   uint64_t seq_out, uint64_t testout)
{
    printf("rx_outoforder_response: %" PRIu64 " %" PRIu64 "\n", seq_out, testout);
}

static struct ping_pong_rx_vtbl rx_vtbl = {
    .ping = rx_ping,
    .pong = rx_pong,
    .slow_op = rx_slow_op,
    .slow_reply = rx_slow_reply,
    .stop = rx_stop,
    .testrpc_call = rx_testrpc_call,
    .testrpc_response = rx_testrpc_response,
    .outoforder_call = rx_outoforder_call,
    .outoforder_response = rx_outoforder_response
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
    struct ping_pong_binding *b = arg;
    struct client_state *myst = b->st;
    errval_t err;

    //printf("client sending msg %d\n", myst->nextmsg);

    times[myst->nextmsg] = bench_tsc();
    if (myst->nextmsg < REPEAT_BASIC) {
	BEE_SIMCTRL(BEE_SIMCTRL_CACHE_DELTA);
	err = b->tx_vtbl.ping(b, NOP_CONT, myst->nextmsg);
    }
    else if (myst->nextmsg == REPEAT_BASIC) {
	err = b->tx_vtbl.slow_op(b, MKCONT(send_cont, b), 20);
    }
    else {
	BEE_SIMCTRL(BEE_SIMCTRL_CACHE_STAT);
	cycles_t cy = icachetest_run(20);
	printf("cy=%lu\n", cy);
	BEE_SIMCTRL(BEE_SIMCTRL_CACHE_STAT);
	printf("client all done\n");
	err = trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 0); 
	if (err_is_fail(err)) {
	    DEBUG_ERR(err, "trace_event off");
	    return;
	}

	void *dump = malloc(1000000);
	size_t dumpsize = trace_dump(dump, 1000000);
	printf("trace_dump: resulted in %lu bytes\n", dumpsize);
	sys_print(dump, dumpsize);
	printf("Cycles times for %u ping operations\n", REPEAT_BASIC);
	for(int i=0; i<REPEAT_BASIC; i++)
	    printf("%u\n", times[i+1] - times[i]);
	printf("Done.\n");
	return;
    }

    if (err_is_ok(err)) {
        myst->nextmsg++;
    } else {
        DEBUG_ERR(err, "error sending message %d", myst->nextmsg);
        abort();
    }
}

static void bind_cb(void *st, errval_t err, struct ping_pong_binding *b)
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
    err = trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 1);
    if (err_is_fail(err)) {
	DEBUG_ERR(err, "trace_event on");
	return;
    }
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 0xbee);
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 0xbee);
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 0xbee);
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 0xbee);
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 0xbee);
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 0xbee);
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 0xbee);
    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 0xbee);

    send_cont(b);
}

void bb_pingpong_start_client(void)
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
    err = ping_pong_bind(iref, bind_cb, NULL /* state pointer for bind_cb */,
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

static errval_t connect_cb(void *st, struct ping_pong_binding *b)
{
    printf("service got a connection!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}

void bb_pingpong_start_server(void)
{
    errval_t err;

    err = ping_pong_export(NULL /* state pointer for connect/export callbacks */,
                      export_cb, connect_cb, get_default_waitset(),
                      IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }
}

