/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <bench/bench.h>
#include <if/bench_defs.h>

// for cache benchmark
#include <barrelfish/sys_debug.h>
#include <arch/x86/barrelfish/perfmon.h>
#include <arch/x86/barrelfish_kpi/perfmon_amd.h>

static bool start_benchmark_flag = false;
static struct bench_binding *binding;
static struct lmp_chan *chan;

#define ITERATIONS      1000

#define SERVER_VAL 42.0
#define CLIENT_VAL 1337.0

static bool reply_received;

static void set_xmm1(double val)
{
    __asm volatile(
        "movq %0, %%xmm1   \n\t"
        : /* no output registers */
        : "r" (val)
        : "xmm1");
}

static double get_xmm1(void)
{
    double val = -1;

    __asm volatile(
        "movq %%xmm1, %0  \n\t"
        : "=r" (val));

    return val;
}

// server side handler
static void lrpc_bench_handler(void *arg)
{
    errval_t err;
    uint64_t val;
    val = bench_tsc();

    double d_val = get_xmm1();
    if (d_val != SERVER_VAL) {
        printf("SERVER: %lf != %lf\n", SERVER_VAL, d_val);
        abort();
    }

    // try to pull a message out of the channel
    struct lmp_recv_msg msg = LMP_RECV_MSG_INIT;
    err = lmp_chan_recv(chan, &msg, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "endpoint poll failed");
        printf("endpoint_poll failed\n");
        abort();
    }

    // send back reply
    struct bench_binding *b = arg;
    err = b->tx_vtbl.lrpc_bench_reply(b, NOP_CONT, val);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error sending back reply");
        printf("lrpc_bench_reply failed\n");
        abort();
    }

    // re-register
    struct event_closure ec = {
        .handler = lrpc_bench_handler,
        .arg = b,
    };
    lmp_chan_register_recv(chan, get_default_waitset(), ec);
}

static void lrpc_init(struct bench_binding *b)
{
    binding = b;
    chan = &((struct bench_lmp_binding *)b)->chan;
}

// client side handler
static void lrpc_bench_reply_handler(struct bench_binding *b,
                                     uint64_t value)
{
    reply_received = true;
}

static void lrpc_init_reply(struct bench_binding *b)
{
    chan = &((struct bench_lmp_binding *)b)->chan;
    start_benchmark_flag = true;
}

/// server and client rx vtbl
static struct bench_rx_vtbl rx_vtbl = {
    .lrpc_init        = lrpc_init,
    .lrpc_init_reply  = lrpc_init_reply,
    .lrpc_bench_reply = lrpc_bench_reply_handler,
};

/// Called on the client side when client connected to the server
static void bind_cb(void *st, errval_t err, struct bench_binding *b)
{
    assert(err_is_ok(err));
    b->rx_vtbl = rx_vtbl;

    err = b->tx_vtbl.lrpc_init(b, NOP_CONT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "lrpc_init_reply failed");
        printf("lrpc_init_reply failed\n");
        abort();
    }
}

static void lrpc_test(void)
{
    errval_t err;

    set_xmm1(CLIENT_VAL);

    for(int currentiter = 0; currentiter < ITERATIONS; currentiter++) {
        // yield to receiver to make sure they are ready
        thread_yield_dispatcher(chan->remote_cap);

        reply_received = false;

        err = lmp_ep_send0(chan->remote_cap, LMP_FLAG_SYNC, NULL_CAP);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "LRPC %d failed", currentiter);
            printf("lmp_ep_send0 failed\n");
            abort();
        }
        while (!reply_received) {
            messages_wait_and_handle_next();
        }

        double val = get_xmm1();
        if (val != CLIENT_VAL) {
            printf("CLIENT: %lf != %lf\n", CLIENT_VAL, val);
            abort();
        }
    }
}

/// Called when servers setup their services
static void export_cb(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    err = nameservice_register("lrpc_server", iref);
    assert(err_is_ok(err));
}

/// Called when the client connects
static errval_t connect_cb(void *st, struct bench_binding *b)
{
    b->rx_vtbl = rx_vtbl;
    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    enum {CLIENT, SERVER} mode = -1;
    errval_t err;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "client") == 0) {
            mode = CLIENT;
        } else if (strcmp(argv[i], "server") == 0) {
            mode = SERVER;
        } else {
            fprintf(stderr, "%s: unknown argument '%s'\n", argv[0], argv[i]);
            return -1;
        }
    }

    if (mode == -1) {
        fprintf(stderr, "Usage: %s client|server\n", argv[0]);
        return -1;
    }

    if (mode == SERVER) { /* Server */
        set_xmm1(SERVER_VAL);

        /* Setup a server */
        err = bench_export(NULL, export_cb, connect_cb, get_default_waitset(),
                           IDC_EXPORT_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to setup a server");
            exit(EXIT_FAILURE);
        }

        while (chan == NULL || binding == NULL) {
            messages_wait_and_handle_next();
        }

        // cancel flounder-generated event loop and install our own
        lmp_chan_deregister_recv(chan);
        struct event_closure ec = {
            .handler = lrpc_bench_handler,
            .arg = binding,
        };
        lmp_chan_register_recv(chan, get_default_waitset(), ec);

        err = binding->tx_vtbl.lrpc_init_reply(binding, NOP_CONT);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "lrpc_init_reply failed");
            printf("lrpc_init_reply failed\n");
            abort();
        }

        messages_handler_loop();
    }

    /* Client */
    /* Initialize the benchmarking library */
    bench_init();

    /* Connect to the server */
    iref_t serv_iref;
    err = nameservice_blocking_lookup("lrpc_server", &serv_iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to lookup server");
        exit(EXIT_FAILURE);
    }
    assert(serv_iref != 0);

    err = bench_bind(serv_iref, bind_cb, NULL, get_default_waitset(),
                     IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to connect to server");
        exit(EXIT_FAILURE);
    }

    while (!start_benchmark_flag) {
        messages_wait_and_handle_next();
    }

    printf("LRPC-FPU test:\n");

    lrpc_test();

    printf("End of LRPC-FPU test: Everything OK.\n");
    return 0;
}
