/**
 * \file
 * \brief Distops common server/client framework
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/test_defs.h>

#include "test.h"

static const char *service_name = "distops_test";

//{{{1 Generic testing function for retype
void test_retype(struct capref src, struct capref *dest, gensize_t offset,
                 gensize_t size, size_t count, errval_t expected_err, const char *role)
{
    errval_t err;
    struct capref result = NULL_CAP;
    bool free = false;

    if (!dest) {
        dest = &result;
        free = true;
    }

    if (capref_is_null(*dest)) {
        if (count != 1) {
            USER_PANIC("%s_%s: did not provide capref, but more than one object requested",
                    role, __FUNCTION__);
        }
        err = slot_alloc(dest);
        PANIC_IF_ERR(err, "in %s: slot_alloc for retype result", role);
    }

    // Tests come here
    err = cap_retype(*dest, src, offset, ObjType_Frame, size, count);
    if (err != expected_err) {
        printf("distops_retype: retype(offset=%"PRIuGENSIZE", size=%"PRIuGENSIZE
                     ", count=%zu) to Frame returned %s, expected %s\n",
                     offset, size, count, err_getcode(err), err_getcode(expected_err));
    }
    if (free) {
        if (err_is_ok(err)) {
            // Cap delete only necessary if retype successful
            err = cap_delete(result);
            PANIC_IF_ERR(err, "cap_delete in %s_test_retype", role);
        }
        err = slot_free(result);
        PANIC_IF_ERR(err, "slot_free in %s_test_retype", role);
    }
}

//{{{1 Unused message handlers
static void rx_str(struct test_binding *b, uint32_t arg, const char *s)
{
    printf("rx_str %"PRIu32" '%s'\n", arg, s);
}

static void rx_buf(struct test_binding *b, const uint8_t *buf, size_t buflen)
{
    printf("rx_buf buflen=%zu\n", buflen);
}

//{{{1 Server

//{{{2 Server-side message handlers

static void server_rx_basic(struct test_binding *b, uint32_t arg)
{
    printf("server rx_basic %"PRIu32"\n", arg);
    server_do_test(b, arg, NULL_CAP);
}

static void server_rx_caps(struct test_binding *b, uint32_t arg, struct capref cap1,
        struct capref cap2)
{
    server_do_test(b, arg, cap1);
}

static struct test_rx_vtbl server_rx_vtbl = {
    .basic = server_rx_basic,
    .str = rx_str,
    .caps = server_rx_caps,
    .buf = rx_buf,
};

// {{{2 Server export & connect

static void export_cb(void *st, errval_t err, iref_t iref)
{
    PANIC_IF_ERR(err, "export failed");

    printf("service exported at iref %"PRIuIREF"\n", iref);

    // register this iref with the name service
    err = nameservice_register(service_name, iref);
    PANIC_IF_ERR(err, "nameservice_register failed");
}

static errval_t connect_cb(void *st, struct test_binding *b)
{
    printf("service got a connection!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = server_rx_vtbl;

    // Create a server state struct for this connection
    init_server(b);

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}

static void run_server(void)
{
    errval_t err;

    err = test_export(NULL /* state pointer for connect/export callbacks */,
            export_cb, connect_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);
    PANIC_IF_ERR(err, "export failed");
}
//{{{1 Client

//{{{2 Client-side message handlers

// We use test.basic to signal that server has done it's retypes
static void client_rx_basic(struct test_binding *b, uint32_t arg)
{
    printf("client rx_basic %"PRIu32": b->st = %p\n", arg, b->st);

    client_do_test(b, arg, NULL_CAP);
}

static void client_rx_caps(struct test_binding *b, uint32_t arg, struct capref cap1,
        struct capref cap2)
{
    printf("client rx_caps: arg=%"PRIu32"\n", arg);
    cap_destroy(cap1);
    cap_destroy(cap2);
}

static struct test_rx_vtbl client_rx_vtbl = {
    .basic = client_rx_basic,
    .str = rx_str,
    .caps = client_rx_caps,
    .buf = rx_buf,
};

//{{{2 Client bind

static void bind_cb(void *st, errval_t err, struct test_binding *b)
{
    PANIC_IF_ERR(err, "bind failed");

    printf("client bound!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = client_rx_vtbl;

    // Initialize client side
    init_client(b);
}

static void run_client(void)
{
    errval_t err;
    iref_t iref;

    printf("client looking up '%s' in name service...\n", service_name);
    err = nameservice_blocking_lookup(service_name, &iref);
    PANIC_IF_ERR(err, "nameservice_blocking_lookup failed");

    printf("client binding to %"PRIuIREF"...\n", iref);

    err = test_bind(iref, bind_cb, NULL, get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    PANIC_IF_ERR(err, "bind failed");
}

//{{{1 Main
int main(int argc, char *argv[])
{
    char *role;
    if (argc < 2) {
        printf("remote retype test needs an argument of \"server\" or \"client\"\n");
    }
    if (strncmp(argv[1], "server", 6) == 0) {
        printf("we are the server\n");
        role = "server";
        run_server();
    }
    if (strncmp(argv[1], "client", 6) == 0) {
        printf("we are the client\n");
        role = "client";
        run_client();
    }

    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (true) {
        err = event_dispatch(ws);
        PANIC_IF_ERR(err, "in %s: event_dispatch", role);
    }

    return 0;
}
