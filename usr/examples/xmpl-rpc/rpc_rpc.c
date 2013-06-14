/** \file
 *  \brief Example rpc application using rpc stubs
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/xmplrpc_defs.h>
#include <if/xmplrpc_rpcclient_defs.h>

const char *service_name = "xmplrpc_rpc_service";

/* --------------------- Client ------------------------------ */

static struct xmplrpc_rpc_client xmplrpc_client;


static void my_notify(struct xmplrpc_binding *_binding, int32_t i) {
    debug_printf("client: got notification from server! %i\n", i);
}

static void client_notify_response_cb(void *a) {
    debug_printf("client: nofitycation sent successfully");
}

static void client_send_notify(void *a) {
        errval_t err;

        debug_printf("client: sending notify\n");
        struct xmplrpc_binding *b = xmplrpc_client.b;

        struct event_closure txcont = MKCONT(client_notify_response_cb, NULL);
        err = xmplrpc_notify__tx(b, txcont, 5678);

        if (err_is_fail(err)) {
            if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
                debug_printf("server: re-sending myresponse\n");
                struct waitset *ws = get_default_waitset();
                txcont = MKCONT(client_send_notify, NULL);
                err = b->register_send(b, ws, txcont);
                if (err_is_fail(err)) {
                    // note that only one continuation may be registered at a time
                    DEBUG_ERR(err, "register_send on binding failed!");
                }
            } else {
                DEBUG_ERR(err, "error sending mycall message\n");
            }
        }
}

static void send_myrpc(void)
{
    errval_t err = SYS_ERR_OK;

    int in;
    char *s_out = malloc(1);

    debug_printf("client: sending myrpc\n");

    in = 42;
    //err = xmplrpc_client.vtbl.myrpc(&xmplrpc_client, in, &s_out);

    if (err_is_ok(err)) {
        debug_printf("client: myrpc(in: %u, out: '%s')\n", in, s_out);
        free(s_out);
    } else {
        DEBUG_ERR(err, "xmlrpc myrpc");
    }

    client_send_notify(NULL);
}

struct xmplrpc_rx_vtbl c_rx_tbl;

static void bind_cb(void *st, errval_t err, struct xmplrpc_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    b->rx_vtbl =c_rx_tbl;
    xmplrpc_client.b = b;
   // xmplrpc_rpc_client_init(&xmplrpc_client, b);

    b->rx_vtbl.notify = my_notify;




    printf("client: finished xmlrpc_rpc_client_init\n");

    send_myrpc();
}


static void start_client(void)
{
    errval_t err;
    iref_t iref;

    err = nameservice_blocking_lookup(service_name, &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
    }

    err = xmplrpc_bind(iref,
                     bind_cb,
                     NULL /* state for bind_cb */,
                     get_default_waitset(),
                     IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }
}

/* --------------------- Server ------------------------------ */

struct server_state {
    struct xmplrpc_binding *b;
    char *s;
};

static void free_st(struct server_state* st)
{
    if (st->s != NULL) free(st->s);
    free(st);
}


static void send_notify_response_cb(void *a) {
    debug_printf("server: nofitycation sent successfully");

    struct server_state *st = (struct server_state*)a;

    free_st(st);
}

static void send_notify(void *a) {
        errval_t err;

        debug_printf("server: sending notify\n");
        struct server_state *st = (struct server_state*)a;

        struct event_closure txcont = MKCONT(send_notify_response_cb, st);
        err = xmplrpc_notify__tx(st->b, txcont, 1234);

        if (err_is_fail(err)) {
            if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
                debug_printf("server: re-sending myresponse\n");
                struct waitset *ws = get_default_waitset();
                txcont = MKCONT(send_notify, st);
                err = st->b->register_send(st->b, ws, txcont);
                if (err_is_fail(err)) {
                    // note that only one continuation may be registered at a time
                    DEBUG_ERR(err, "register_send on binding failed!");
                    free_st(st);
                }
            } else {
                DEBUG_ERR(err, "error sending mycall message\n");
                free_st(st);
            }
        }
}


static void send_myrpc_response_cb(void *a)
{
    debug_printf("server: myresponse sent succesfully\n");

    send_notify(a);
}

static void send_myrpc_response(void *a)
{
    errval_t err;
    struct server_state *st = (struct server_state*)a;

    debug_printf("server: sending myresponse\n");

    struct event_closure txcont = MKCONT(send_myrpc_response_cb, st);
    err = xmplrpc_myrpc_response__tx(st->b, txcont, st->s);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            debug_printf("server: re-sending myresponse\n");
            struct waitset *ws = get_default_waitset();
            txcont = MKCONT(send_myrpc_response, st);
            err = st->b->register_send(st->b, ws, txcont);
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
                free_st(st);
            }
        } else {
            DEBUG_ERR(err, "error sending mycall message\n");
            free_st(st);
        }
    }
}

static void rx_myrpc_call(struct xmplrpc_binding *b, int32_t i)
{
    debug_printf("server: received myrpc_call: %d\n", i);

    // prepare and send reply

    struct server_state *st = malloc(sizeof(struct server_state));
    if (st == NULL) {
        USER_PANIC("cannot reply, out of memory");
    }

    st->b = b;
    st->s = malloc(20);
    if (st->s != NULL) {
        snprintf(st->s, 20, "!%d!", i);
    }

    send_myrpc_response(st);
}

static void rx_notify(struct xmplrpc_binding *_binding, int32_t i) {
    debug_printf("server got notification from client %i\n", i);
    struct server_state *st = malloc(sizeof(struct server_state));
        if (st == NULL) {
            USER_PANIC("cannot reply, out of memory");
        }

        st->b = _binding;
        send_notify(st);

}

static struct xmplrpc_rx_vtbl s_rx_vtbl = {
    .myrpc_call = rx_myrpc_call,
    .notify = rx_notify,
};

static errval_t connect_cb(void *st, struct xmplrpc_binding *b)
{
    b->rx_vtbl = s_rx_vtbl;

    return SYS_ERR_OK;
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static void start_server(void)
{
    errval_t err;

    err = xmplrpc_export(NULL /* state pointer for connect/export callbacks */,
                        export_cb, connect_cb,
                        get_default_waitset(),
                        IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }
}



/* --------------------- Main ------------------------------ */

int main(int argc, char *argv[])
{
    errval_t err;

    if ((argc >= 2) && (strcmp(argv[1], "client") == 0)) {
        start_client();
    } else if ((argc >= 2) && (strcmp(argv[1], "server") == 0)) {
        start_server();
    } else {
        printf("usage: %s client|server\n", argv[0]);
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
