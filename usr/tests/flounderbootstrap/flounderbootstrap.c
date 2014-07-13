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

#include <string.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/flounderbootstrap_defs.h>

static const char *my_service_name = "flounderbootstrap";

struct flounderbootstrap_binding *binding;

struct flounderbootstrap_binding *test_b;

uint8_t is_server = 0x0;

static void *addr;

#define CHANNEL_SIZE BASE_PAGE_SIZE

#define BOOTSTRAP_FRAME_SIZE (2 * CHANNEL_SIZE)

struct flounderbootstrap_frameinfo frame_info;

struct capref frame;

static void bind_bootstrap_cb(void *st,
                              errval_t err,
                              struct flounderbootstrap_binding *b);

/* */

struct tx_msg_st
{
    struct capref cap;
    uint32_t msg;
};

static void tx_init(void *a)
{
    errval_t err;

    struct event_closure txcont = MKCONT(NULL, a);

    err = flounderbootstrap_init__tx(binding, txcont, frame);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(tx_init, a);
            err = binding->register_send(binding, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not send\n");
            }
        }
    }
}

static void tx_ack(void *a)
{
    errval_t err;

    struct event_closure txcont = MKCONT(NULL, a);

    err = flounderbootstrap_ack__tx(binding, txcont);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(tx_ack, NULL);
            err = binding->register_send(binding, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not send\n");
            }
        }
    }
}

static void tx_test(void *a)
{
    errval_t err;

    struct event_closure txcont = MKCONT(NULL, a);

    err = flounderbootstrap_test__tx(test_b, txcont, 0xcafebabe);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(tx_test, a);
            err = binding->register_send(binding, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not send\n");
            }
        }
    }
}

/* ------------------------ COMMON MESSAGE HANDLERS ------------------------ */

static void rx_init(struct flounderbootstrap_binding *b,
                    struct capref cap)
{
    errval_t err;

    debug_printf("rx_init message\n");

    frame = cap;

    struct frame_identity id;
    err = invoke_frame_identify(cap, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to identify the frame");
    }

    assert((1UL << id.bits) >= (2 * CHANNEL_SIZE));

    frame_info.sendbase = id.base;

    err = vspace_map_one_frame(&addr, (1UL << id.bits), cap, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to map the frame");
    }

    frame_info.outbuf = addr;
    frame_info.outbufsize = CHANNEL_SIZE;
    frame_info.inbuf = ((uint8_t *)addr) + CHANNEL_SIZE;
    frame_info.inbufsize = CHANNEL_SIZE;

    debug_printf("Bootstrap channel: IN[%p], OUT[%p]\n", frame_info.inbuf, frame_info.outbuf);


    debug_printf("BOOTSTRAP: CONNECT\n");
    struct waitset *ws = get_default_waitset();
    err = flounderbootstrap_connect(&frame_info, bind_bootstrap_cb, NULL, ws, IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not connect \n");
    }

    debug_printf("BOOTSTRAP: CONNECT succeeded.\n");

    tx_ack(NULL);
}

static void rx_ack(struct flounderbootstrap_binding *b)
{
    debug_printf("rx_ack message\n");
    while(test_b == NULL) {
        messages_wait_and_handle_next();
    }

    tx_test(NULL);
}

uint32_t counter = 0x10;

static void rx_test(struct flounderbootstrap_binding *b,
                    uint32_t arg)
{
    if (is_server) {
        debug_printf("SERVER: rx_test message: %x\n", arg);
    } else {
        debug_printf("CLIENT: rx_test message: %x\n", arg);
    }

    assert(b == test_b);

    if (counter--) {
        tx_test(NULL);
    }
}

static struct flounderbootstrap_rx_vtbl rx_vtbl = {
    .init = rx_init,
    .ack = rx_ack,
    .test = rx_test,
};

/* ------------------------------ CLIENT ------------------------------ */

static void connect_bootstrap_cb(void *st,
                                 errval_t err,
                                 struct flounderbootstrap_binding *b)
{
    debug_printf("BOOTSTRAP:  connect callback\n");

    b->rx_vtbl = rx_vtbl;

    test_b = b;
}

static void bind_cb(void *st,
                    errval_t err,
                    struct flounderbootstrap_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    debug_printf("client bound!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    binding = b;

    tx_init(NULL);
}

static void start_client(void)
{
    iref_t iref;
    errval_t err;

    debug_printf("allocating frame for bootstraping test\n");
    err = frame_alloc(&frame, BOOTSTRAP_FRAME_SIZE, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not allocate frame\n");
    }

    err = vspace_map_one_frame(&addr, BOOTSTRAP_FRAME_SIZE, frame, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not map the frame\n");
    }

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "frameid failed");
    }
    frame_info.sendbase = id.base + CHANNEL_SIZE;
    frame_info.inbuf = ((uint8_t *)addr);
    frame_info.inbufsize = CHANNEL_SIZE;
    frame_info.outbuf = ((uint8_t *)addr) + CHANNEL_SIZE;
    frame_info.outbufsize = CHANNEL_SIZE;

    debug_printf("Bootstrap channel: IN[%p], OUT[%p]\n", frame_info.inbuf, frame_info.outbuf);

    struct waitset *ws = get_default_waitset();

    debug_printf("Bootstraping: ACCEPT\n");
    err = flounderbootstrap_accept(&frame_info, NULL, connect_bootstrap_cb, ws, IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not accept.\n");
    }

    debug_printf("Bootstraping: ACCEPT succeded.\n");

    debug_printf("client looking up '%s' in name service...\n", my_service_name);
    err = nameservice_blocking_lookup(my_service_name, &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
    }

    debug_printf("client binding to %"PRIuIREF"...\n", iref);
    err = flounderbootstrap_bind(iref, bind_cb, NULL ,
                                 get_default_waitset(),
                                 IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }
}

/* ------------------------------ SERVER ------------------------------ */

static void bind_bootstrap_cb(void *st,
                              errval_t err,
                              struct flounderbootstrap_binding *b)
{
    debug_printf("BOOTSTRAP:  bind callback\n");

    b->rx_vtbl = rx_vtbl;

    test_b = b;
}

static void export_cb(void *st,
                      errval_t err,
                      iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    // register this iref with the name service
    err = nameservice_register(my_service_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }

    debug_printf("service %s exported at iref %"PRIuIREF"\n", my_service_name, iref);
}

static errval_t connect_cb(void *st,
                           struct flounderbootstrap_binding *b)
{
    debug_printf("client connected!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    assert(binding == NULL);
    binding = b;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}

static void start_server(void)
{
    errval_t err;

    debug_printf("Starting server...\n");

    is_server = 0x1;

    err = flounderbootstrap_export(NULL,
                                   export_cb,
                                   connect_cb,
                                   get_default_waitset(),
                                   IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }
}

/* ------------------------------ MAIN ------------------------------ */

int main(int argc,
         char *argv[])
{
    errval_t err;

    if (argc == 2 && strcmp(argv[1], "client") == 0) {
        start_client();
    } else if (argc == 2 && strcmp(argv[1], "server") == 0) {
        start_server();
    } else {
        printf("Usage: %s client|server\n", argv[0]);
        return EXIT_FAILURE;
    }

    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

    return EXIT_FAILURE;
}
