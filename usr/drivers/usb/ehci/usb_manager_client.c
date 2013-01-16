/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <if/usb_manager_defs.h>

#include "usb_manager_client.h"

#include "ehci_debug.h"

volatile static bool connect = false;
static struct usb_manager_client_response *resp = NULL;

/**
 * \brief  Notifies USB manager about device attachment. This is a proxy 
 *         function to the actual function in USB manager. Behind the 
 *         curtain it does IPC to the server. 
 * \param port Port where device attachment is detected 
 */
void notify_new_device(int port)
{
    dprintf("usb_manager_client_sub glue: notify_new_device\n");
    // Wait till we are connected to server
    assert(connect && resp);

    // upwards call
    resp->call_vtbl->device_connect(resp, port);
    dprintf("usb_manager_client_sub upwards call done\n");

}

/**
 * \brief  Notifies USB manager about device detachment. This is a proxy 
 *         function to the actual function in USB manager. Behind the 
 *         curtain it does IPC to the server.
 * \param port Port where device detachment is detected  
 */
void notify_device_removal(int port)
{
    return;

    // Wait till we are connected to server 
    assert(connect && resp);

    //upwards call
    resp->call_vtbl->device_disconnect(resp, port);

}


/*
 * USB manager client side interfacing 
 */


static void notify_conn_handler(struct usb_manager_client_response *rsp,
                                int response)
{
    dprintf("Device enumuration done [%u]\n", response);

}

static void notify_disconn_handler(struct usb_manager_client_response *rsp,
                                   int response)
{
    dprintf("Device detachment done [%u]\n", response);
}

static void client_connect(struct usb_manager_client_response *rsp)
{
    dprintf("****** EHCI connected to USB manager\n");
    connect = true;
    resp = rsp;
}

static void client_disconnect(struct usb_manager_client_response *rsp)
{
    dprintf("EHCI disconnected from USB manager\n");
    connect = false;
    resp = NULL;
}

static void start_client(struct chips_context *context)
{
    iref_t iref;
    errval_t e = chips_blocking_lookup(context, "usb_manager", &iref);
    if (err_is_fail(e)) {
        fprintf(stderr, "\n\n EHCI: could not connect to the USB manager.\n"
                "Terminating.\n");
        abort();
    }

    assert(iref != 0);


    static struct usb_manager_client_response_vtbl crv = {
        .notify_conn = notify_conn_handler,
        .notify_disconn = notify_disconn_handler,
        .pipe_resp = NULL,
        ._disconnect = client_disconnect,
        ._connected = client_connect
    };

    static struct usb_manager_client_response cr = {
        .f = &crv
    };

    e = usb_manager_connect(iref, &cr, 4096);
    assert(!err_is_fail(e));
}


/**
 *  \brief USB manager's client side interfacing. Functions in this 
 *         file acts as glue to the client side call. 
 */
int connect_to_usb_manager(void *args)
{
    struct chips_context *context = chips_get_context();
    context->init();
    dprintf("Context init at usb_manager client" "Trying to connect\n");
    start_client(context);
    return 0;
}
