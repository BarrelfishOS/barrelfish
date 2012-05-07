/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <if/ehci_defs.h>

#include <usb/usb_device.h>
#include <usb/usb_debug.h>

/* 
 * Implements functions from it as 
 * proxy.
 */

#include "ehci_client.h"

/* 
 * These are proxy for upward calls.
 */
static volatile bool connect = false;

struct ehci_client_response *ctx = NULL;

// Used to give sync look and feel. Easy to debug and program
static struct thread_sem sync = THREAD_SEM_INITIALIZER;
volatile int response;


int usb_dctrl_exe(usb_device_request req, void *buff, uint64_t sz,
                  uint8_t device, int debug)
{
    assert(connect && ctx);
    struct _ehci_udr_t usb_req;
    usb_req.bmRequestType = req.bmRequestType;
    usb_req.bRequest = req.bRequest;
    usb_req.wValue = req.wValue;
    usb_req.wIndex = req.wIndex;
    usb_req.wLength = req.wLength;
    //FIXME: id is always 1         
    ctx->call_vtbl->dctrl_exe(ctx, usb_req,
                              (uint64_t) buff, sz, device, debug, 1);
    response = -1;
    thread_sem_wait(&sync);

    return response;
}

volatile uint64_t core_id;

uint64_t get_ehci_core_id(void)
{
    assert(connect && ctx);
    ctx->call_vtbl->get_core_id(ctx);
    core_id = -1;

    thread_sem_wait(&sync);

    return core_id;
}



int usb_ctrl_exe(usb_device_request req, uint8_t device, int debug)
{
    assert(connect && ctx);

    struct _ehci_udr_t usb_req;
    usb_req.bmRequestType = req.bmRequestType;
    usb_req.bRequest = req.bRequest;
    usb_req.wValue = req.wValue;
    usb_req.wIndex = req.wIndex;
    usb_req.wLength = req.wLength;

    //FIXME: id is always 1         
    ctx->call_vtbl->ctrl_exe(ctx, usb_req, device, debug, 1);
    response = -1;

    thread_sem_wait(&sync);

    return response;
}


int usb_bulk_exe(usb_pipe_t p, void *io_buff, uint32_t len, int debug)
{
    assert(connect && ctx);
    struct _ehci_pipe_t pipe;
    pipe.dev = p.dev;
    pipe.ep_number = p.ep_number;
    pipe.ep_address = p.ep_address;
    pipe.ep_dir = p.ep_dir;
    pipe.ep_type = p.ep_type;
    pipe.ep_psz = p.ep_psz;
    pipe.multi = p.multi;
    pipe.valid = p.valid;
    ctx->call_vtbl->bulk_exe(ctx, pipe, (uintptr_t) io_buff, len, debug, 1);

    response = -1;
    thread_sem_wait(&sync);
    return response;
}

void map_dev_page(struct capref cap, uint32_t sz)
{
    dprintf("%s\n", __func__);
    assert(connect && ctx);
    ctx->call_vtbl->map_dev_arr(ctx, cap, sz);
    //printf("\n USBD: >>>>>>>%s>>>>>>>>  mapping done !!", __func__);
}

/* Client of EHCI side handlers */

static void map_dev_arr_done_handler(struct ehci_client_response *rsp,
                                     uint8_t resp)
{
    dprintf("%s\n", __func__);
    assert(resp == 0);
}

static void dctrl_done_handler(struct ehci_client_response *rsp, uint32_t id)
{
    dprintf("%s: response[%u]\n ", __func__, id);
    response = id;
    thread_sem_post(&sync);
}


static void ctrl_done_handler(struct ehci_client_response *rsp, uint32_t id)
{
    dprintf("%s: response %u\n", __func__, id);
    response = id;
    thread_sem_post(&sync);
}

static void bulk_done_handler(struct ehci_client_response *rsp, uint32_t id)
{
    dprintf("%s: \n", __func__);
    response = id;
    thread_sem_post(&sync);
}

static void get_core_id_handler(struct ehci_client_response *rsp, uint64_t id)
{
    dprintf("%s\n", __func__);
    core_id = id;
    thread_sem_post(&sync);
}


/* EHCI client logic */
static void client_connect(struct ehci_client_response *rsp)
{
    printf("USBD: ########### connected to EHCI manager\n");
    connect = true;
    ctx = rsp;
}

static void client_disconnect(struct ehci_client_response *rsp)
{
    printf("USBD: disconnected from EHCI manager \n");
    connect = false;
    ctx = NULL;
}

static void start_client(struct chips_context *context)
{
    iref_t iref;
    errval_t e = chips_blocking_lookup(context, "ehci_hc", &iref);
    if (err_is_fail(e)) {
        fprintf(stderr, "USBD: could not connect to the EHCI manager.\n"
                "Terminating.\n");
        abort();
    }

    assert(iref != 0);


    static struct ehci_client_response_vtbl crv = {
        .map_dev_arr_done = map_dev_arr_done_handler,
        .dctrl_done = dctrl_done_handler,
        .ctrl_done = ctrl_done_handler,
        .bulk_done = bulk_done_handler,
        .get_core_id_resp = get_core_id_handler,
        ._disconnect = client_disconnect,
        ._connected = client_connect
    };

    static struct ehci_client_response cr = {
        .f = &crv
    };

    e = ehci_connect(iref, &cr, 4096);
    assert(!err_is_fail(e));
    printf("USBD: -----------\n");
}

int connect_to_ehci_manager(void *args)
{
    struct chips_context *context = chips_get_context();
    context->init();
    printf("USBD: Trying to connect to EHCI server ....\n");
    start_client(context);
    return 0;
}
