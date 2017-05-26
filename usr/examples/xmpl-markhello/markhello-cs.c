/** \file
 *  \brief Hello World application
 */

/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


/* NOTICE THE if/markhello include file */
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/markhello_defs.h> 


/* if/marhello_defs.h is auto generated from if/markhello.if */






/* *************** SERVER SIDE ***********************
   ***************************************************
   *************************************************** */



static void export_cb(void *st, errval_t err, iref_t iref);
static errval_t connect_cb(void *st, struct markhello_binding *b);
static void bind_cb(void *st, errval_t err, struct markhello_binding *b);

/*  Server code in 3 steps:
    - exporting service
    - registering the service
    - handling actual requests */


/*  1. EXPORT THE SERVICE

    The server needs to export the service it wants to provide 
    by calling export function on the service interface. In this case, server 
    will call markhello export as we are providing Hello World service. */

static void start_server(void){
    errval_t err;
    err = markhello_export(NULL /* state for callbacks */,
            export_cb, /* Callback for export */
            connect_cb, /* Callback for client connects */
            get_default_waitset(), /* waitset where events will be sent */
            IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }
}

/*  This call also registers two callback handles. Once the service
    is successfully exported, then the export callback provided. 
    When any client connects with the service then the connect callback
    will be called. We will see the use of these callbacks in next few steps. */




/*  2. REGISTER THE SERVICE

    The Server also need to register the service so that other applications 
    can find it. Following code registers the "hello service" on the callback
    from successful completion of export: */

const static char *service_name = "markhello_service";

static void export_cb(void *st, errval_t err, iref_t iref) {
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }
    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

/*  The nameservice register is a blocking call which will connect to the 
    global nameserver and publish the service name. */



/*  3. CONNECT AND HANDLE REQUESTS

    This section describes how exactly the connection is established 
    and requests are handled. */


/*  This function handles requests. In our case we are just printing
    the string we received in the message */
static void rx_markhello_msg(struct markhello_binding *b, int num){
    printf("server: received markhello_msg:\n\t%d\n", num);
    //free(str);
}


/*  We need to create a list  of function pointers where one function pointer 
    is assigned for every possible incoming message. So we are creating an instance
    rx vtbl of type hello rx vtbl. On arrival of new connection, we provide this list 
    of function pointers to register which functions to call for handling particular 
    type of message. */
static struct markhello_rx_vtbl rx_vtbl = {
    .markhello_msg = rx_markhello_msg,
}; /* I belive more function pointers would go in this struct */

static errval_t connect_cb(void *st, struct markhello_binding *b) {
    b->rx_vtbl = rx_vtbl;
    return SYS_ERR_OK;
}






/* *************** CLIENT SIDE ***********************
   ***************************************************
   *************************************************** */


static void run_client(void *arg);

/*  Client needs to:
    - find the service and connect to it
    - Once the connection is successfully established then
      it can send the actual requests. */


/*  1. Find service and connect (called binding)*/

static void start_client(void) {
    
    errval_t err;
    iref_t iref;

    /* nameservice blocking lookup is a blocking call 
    which will connect with the nameserver and query for the given service name.
    The returned iref handle is used for binding with the service */
    err = nameservice_blocking_lookup(service_name, &iref);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err,"nameservice_blocking_lookup failed");
    }

    /*  this function is generated from markhello.if and attempts
        to connect with the server on the appropriate channel */

    err = markhello_bind(iref, bind_cb, /* Callback function */
                        NULL /* State for the callback */,
                        get_default_waitset(),
                        IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }
}


/* 
    The following code is the callback function that is called when the client 
    successfully connects with the server. 

    It also creates a client state that stores pointer to the communication 
    channel binding. It calls run_client(client state) */

struct client_state {
    struct markhello_binding *binding;
    int count;
};

static void bind_cb(void *st, errval_t err, struct markhello_binding *b) {
    struct client_state *myst = malloc(sizeof(struct client_state));
    assert(myst != NULL);
    myst->binding = b;
    myst->count = 0;
    run_client(myst); /* calling run_client for first time */
}


/*  2. Send Messeges!!! */

/*  The following function does the actual work of sending client messages */

static void run_client(void *arg) {
    errval_t err;
    struct client_state *myst = arg;
    struct markhello_binding *b = myst->binding;

    /*  Creating a continuation callback function which will call run_client 
        again in an infinite loop */
    struct event_closure txcont = MKCONT(run_client, myst);

    /*  registers a message to be sent on the send side of binding
        and a callback that will be triggered when message is successful. */
    err = b->tx_vtbl.markhello_msg(b, txcont, 500);
    
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error sending message %d\n", myst->count);
    }
}










/* ****************** MAIN **************************
   ***************************************************
   *************************************************** */

int main(int argc, char *argv[]) {
    errval_t err;
    if ((argc >= 2) && (strcmp(argv[1], "client") == 0)) {
        start_client();
    } else if ((argc >= 2) && (strcmp(argv[1], "server") == 0)) {
        start_server();
    } else {
        return EXIT_FAILURE;
    }
    /* The dispatch loop */
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws); /* get and handle next event */
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }
    return EXIT_FAILURE;
}