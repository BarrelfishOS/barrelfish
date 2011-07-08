/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <if/usb_manager_defs.h>

#include "usb_manager_client.h"

static struct thread_sem sync = THREAD_SEM_INITIALIZER;

volatile static bool connect = false;
static struct usb_manager_client_response *resp = NULL;

volatile usb_pipe_t pipe;

void init_pipe(uint8_t dev, uint8_t type, uint8_t dir, usb_pipe_t * req_p)
{
    while (!resp || !connect);
    resp->call_vtbl->pipe_req(resp, dev, type, dir);
    thread_sem_wait(&sync);
    *(req_p) = pipe;
}

static void pipe_response_handler(struct usb_manager_client_response *rsp,
                                  uint8_t status, struct _usb_manager_pipe_t p)
{
    assert(status == 0);
    // copy the data
    pipe.dev = p.dev;
    pipe.ep_number = p.ep_number;
    pipe.ep_address = p.ep_address;
    pipe.ep_dir = p.ep_dir;
    pipe.ep_type = p.ep_type;
    pipe.ep_psz = p.ep_psz;
    pipe.multi = p.multi;
    pipe.valid = p.valid;

    thread_sem_post(&sync);
}


int get_curr_intf_num(uint8_t dev)
{
    //FIXME: 
    return 0;
}


/*
 * USB manager client side interfacing 
 */

static void client_connect(struct usb_manager_client_response *rsp)
{
    printf("\n USB_DRIVER: MSD connected to USB manager \n\n");
    connect = true;
    resp = rsp;
}

static void client_disconnect(struct usb_manager_client_response *rsp)
{
    printf("\n USB_DRIVER: MSD disconnected from USB manager \n");
    connect = false;
    resp = NULL;
}

static void start_client(struct chips_context *context)
{
    iref_t iref;
    errval_t e = chips_blocking_lookup(context, "usb_manager", &iref);
    if (err_is_fail(e)) {
        fprintf(stderr, "\n\n MSD: could not connect to the USB manager.\n"
                "Terminating.\n");
        abort();
    }

    assert(iref != 0);


    static struct usb_manager_client_response_vtbl crv = {
        .notify_conn = NULL,
        .notify_disconn = NULL,
        .pipe_resp = pipe_response_handler,
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
    printf("\n\n\n\n Thread entry -- %s", __func__);
    struct chips_context *context = chips_get_context();
    context->init();
    printf("\n USB_DRIVER: Context init at usb_manager client"
           "Trying to connect\n ");
    start_client(context);
    return 0;
}
