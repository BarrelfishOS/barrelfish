/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
//#include "driver.h"
#include <if/usb_driver_defs.h>

#include "driver_managment.h"

#include <usb/usb_debug.h>
#include <usb/usbd.h>

struct thread_sem sync = THREAD_SEM_INITIALIZER;
volatile int status;
struct usb_driver_client_response *dd_ctx = NULL;
volatile bool connect = false;

/*
 * \brief which sends a probe request to the device driver server
 *
 * \param dev Device number, in case driver accepts
 * \param class Class code for the device
 * \param subclass Subclass code for the device
 * \param protocol Protocol code for the device
 */

int probe(uint8_t dev, uint8_t class, uint8_t subclass, uint8_t protocol)
{
    dprintf("%s\n", __func__);
    assert(connect && dd_ctx);

    dd_ctx->call_vtbl->probe(dd_ctx, dev, class, subclass, protocol);
    status = REJECT;
    thread_sem_wait(&sync);
    return status;
}

static void probe_done_handler(struct usb_driver_client_response *rsp,
                               uint8_t response, uint8_t address)
{
    dprintf("%s\n", __func__);
    if (response == ACCEPT) {
        printf("\n USBD: Driver accepted the device");
    } else {
        printf("\n USBD: Driver rejected the device");
        release_device(address);
    }

    status = response;
    thread_sem_post(&sync);
}

int disconnect(uint8_t dev)
{
    assert(connect && dd_ctx);

    // upwards call 
    //clean and exit 
    return 0;
}

static void disconnect_done_handler(struct usb_driver_client_response *rsp,
                                    uint8_t response)
{
    dprintf("%s\n", __func__);
    status = response;
    thread_sem_post(&sync);
}


/* DRIVER client logic */
static void client_connect(struct usb_driver_client_response *rsp)
{
    dprintf("Connected to DRIVER manager\n");
    connect = true;
    dd_ctx = rsp;
}

static void client_disconnect(struct usb_driver_client_response *rsp)
{
    dprintf("disconnected from DRIVER manager\n");
    connect = false;
    dd_ctx = NULL;
}

static void start_client(struct chips_context *context)
{
    iref_t iref;
    errval_t e = chips_blocking_lookup(context, "msd_driver", &iref);
    if (err_is_fail(e)) {
        fprintf(stderr, "USBD: could not connect to the DRIVER.\n"
                "Terminating.\n");
        abort();
    }

    assert(iref != 0);


    static struct usb_driver_client_response_vtbl crv = {
        .probe_done = probe_done_handler,
        .disconnect_done = disconnect_done_handler,
        ._disconnect = client_disconnect,
        ._connected = client_connect
    };

    static struct usb_driver_client_response cr = {
        .f = &crv
    };

    e = usb_driver_connect(iref, &cr, 4096);
    assert(!err_is_fail(e));
    dprintf("-----------\n");
}

static int connect_to_dd(void *args)
{
    struct chips_context *context = chips_get_context();
    context->init();
    dprintf("Trying to connect to DRIVER server ....\n");
    start_client(context);
    return 0;
}

void locate_and_notify_driver(uint8_t dev,
                              uint8_t class, uint8_t subclass, uint8_t protocol)
{
    //FIXME: extract the service name from the SKB
    int r;
    connect_to_dd(0);
    while (!connect);
    dprintf("Manager connected to the DRIVER....\n");
    r = probe(dev, class, subclass, protocol);
}
