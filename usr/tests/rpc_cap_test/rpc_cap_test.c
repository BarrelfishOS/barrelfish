/** \file
 *  \brief RPC system test code
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

#include <if/test_rpc_cap_defs.h>
#include <if/test_rpc_cap_defs.h>

static const char *my_service_name = "rpc_cap_test";
uint8_t is_server = 0x0;
int client_id = 0;

__attribute__((format(printf, 1, 2)))
static void my_debug_printf(const char *fmt, ...)
{
    struct thread *me = thread_self();
    va_list argptr;
    char id[32] = "-";
    char str[1024];
    size_t len;

    if (me)
        snprintf(id, sizeof(id), "%"PRIuPTR, thread_get_id(me));
    len = snprintf(str, sizeof(str), "\033[34m%.*s.\033[31m%u.%s\033[0m: ",
                   DISP_NAME_LEN, disp_name(), disp_get_core_id(), id);

    if(is_server){
        len += snprintf(str + len, sizeof(str) - len, "server: ");
    } else {
        len += snprintf(str + len, sizeof(str) - len, "client(%d): ",
                client_id);
    }

    if (len < sizeof(str)) {
        va_start(argptr, fmt);
        vsnprintf(str + len, sizeof(str) - len, fmt, argptr);
        va_end(argptr);
    }
    sys_print(str, sizeof(str));
}



struct echo_response {
    struct test_rpc_cap_binding *b;
    uint32_t response;
};

static void send_echo_response(void *arg){
    struct echo_response * resp = arg;
    errval_t err;
    err = test_rpc_cap_echo_response__tx(resp->b, NOP_CONT, resp->response);
    assert(err_is_ok(err));
}


static void handle_echo_call(struct test_rpc_cap_binding *b,
        uint32_t arg_in)
{
    my_debug_printf("handle_echo_call (bind=%p) arg=%"PRIu32"\n", b, arg_in);
    struct echo_response * resp = malloc(sizeof(*resp));
    resp->b = b;
    resp->response = arg_in;
    send_echo_response(resp);
}


static void handle_send_cap_one_call(struct test_rpc_cap_binding *b,
        struct capref incap)
{
    my_debug_printf("handle_send_cap_one_call (bind=%p)\n", b);
    errval_t err = SYS_ERR_OK;
    struct capability cap;
    err = debug_cap_identify(incap, &cap );

    test_rpc_cap_send_cap_one_response__tx(b, NOP_CONT, err);
}

static void handle_send_cap_two_call(struct test_rpc_cap_binding *b,
        struct capref incap1, struct capref incap2)
{
    my_debug_printf("handle_send_cap_two_call (bind=%p)\n", b);
    errval_t err = SYS_ERR_OK;
    struct capability cap;

    err = debug_cap_identify(incap1, &cap);
    assert(err_is_ok(err));
    err = debug_cap_identify(incap2, &cap);
    assert(err_is_ok(err));

    test_rpc_cap_send_cap_two_response__tx(b, NOP_CONT, err);
}

static struct test_rpc_cap_rx_vtbl rx_vtbl = {
    .echo_call = handle_echo_call,
    .send_cap_one_call = handle_send_cap_one_call,
    .send_cap_two_call = handle_send_cap_two_call
};

/* ------------------------------ CLIENT ------------------------------ */

static struct test_rpc_cap_binding *test_rpc_binding;

static void client_call_test_1(void){
    uint32_t res=0;
    errval_t err;
    err = test_rpc_binding->rpc_tx_vtbl.echo(test_rpc_binding, client_id, &res);
    if(err_is_fail(err)){
        my_debug_printf("Error in rpc call (1)\n");
    } else if(res != client_id) {
        my_debug_printf("Wrong result?\n");
    } else {
        my_debug_printf("client_call_test_1 successful!\n");
    }
}

static void client_call_test_2(void){
    struct capref my_frame;
    errval_t err, msg_err;

    err = frame_alloc(&my_frame, BASE_PAGE_SIZE, NULL);
    assert(err_is_ok(err));

    err = test_rpc_binding->rpc_tx_vtbl.send_cap_one(test_rpc_binding, my_frame, &msg_err);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "Error in rpc call (2)\n");
    } else if(err_is_fail(msg_err)) {
        USER_PANIC_ERR(err, "Server msg (2)\n");
    } else {
        my_debug_printf("client_call_test_2 successful!\n");
    }
}

static void client_call_test_3(int i){
    struct capref frame1, frame2;
    struct capability cap1, cap2;
    char buf[1024];
    int buf_idx=0;
    errval_t err, msg_err;

    err = frame_alloc(&frame1, BASE_PAGE_SIZE, NULL);
    assert(err_is_ok(err));
    err = debug_cap_identify(frame1, &cap1);
    assert(err_is_ok(err));
    buf_idx += debug_print_cap(buf, sizeof(buf), &cap1);

    err = frame_alloc(&frame2, BASE_PAGE_SIZE, NULL);
    assert(err_is_ok(err));
    err = debug_cap_identify(frame2, &cap2);
    assert(err_is_ok(err));
    buf_idx += debug_print_cap(buf+buf_idx, sizeof(buf)-buf_idx, &cap2);

    my_debug_printf("Calling send_cap_two: %s\n", buf);

    err = test_rpc_binding->rpc_tx_vtbl.send_cap_two(test_rpc_binding, frame1, frame2, &msg_err);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "Error in rpc call (3)\n");
    } else if(err_is_fail(msg_err)) {
        USER_PANIC_ERR(err, "Server msg (3)\n");
    } else {
        my_debug_printf("client_call_test_3(%d) successful!\n", i);
    }
}

static void bind_cb(void *st,
                    errval_t err,
                    struct test_rpc_cap_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    my_debug_printf("client: bound!\n");

    test_rpc_binding = b;
    test_rpc_cap_rpc_client_init(b);

    client_call_test_1();
    client_call_test_2();
    for(int i=0; i<100;i++){
        client_call_test_3(i);
    }
    printf("TEST PASSED\n");
}

static void start_client(void)
{
    iref_t iref;
    errval_t err;

    my_debug_printf("client: looking up '%s' in name service...\n", my_service_name);
    err = nameservice_blocking_lookup(my_service_name, &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
    }

    my_debug_printf("client: binding to %"PRIuIREF"...\n", iref);
    err = test_rpc_cap_bind(iref, bind_cb, NULL ,
                                 get_default_waitset(),
                                 IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }
}

/* ------------------------------ SERVER ------------------------------ */


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

    my_debug_printf("server: service %s exported at iref %"PRIuIREF"\n", my_service_name, iref);
}

static errval_t connect_cb(void *st,
                           struct test_rpc_cap_binding *b)
{
    my_debug_printf("server: client connected!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}

static void start_server(void)
{
    errval_t err;

    my_debug_printf("server: Starting server...\n");

    is_server = 0x1;

    err = test_rpc_cap_export(NULL,
                              export_cb,
                              connect_cb,
                              get_default_waitset(),
                              IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }
}

/* ------------------------------ MAIN ------------------------------ */

static int usage(char * argv[]){
    printf("Usage: %s client|server [id=INT]\n", argv[0]);
    return EXIT_FAILURE;
}

int main(int argc,
         char *argv[])
{
    errval_t err;

    if (argc >= 2 && strcmp(argv[1], "client") == 0) {
        for(int i = 2; i < argc; i++){
            if(strncmp(argv[i], "id=", strlen("id=")) == 0){
                client_id = atoi(argv[i] + 3);
            } else {
                printf("Unknonw argument: %s\n", argv[i]);
                return usage(argv);
            }
        }
        start_client();
    } else if (argc == 2 && strcmp(argv[1], "server") == 0) {
        start_server();
    } else {
        return usage(argv);
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
