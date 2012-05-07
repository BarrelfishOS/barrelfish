/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * main.c 
 *
 * This file is contains the primary server connection 
 * logic to the EHCI server. Upon receiving the request 
 * it just forwards to them to their respective functions
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>

#include <usb/usb_device.h>
#include <usb/usb_pipe.h>
#include <usb/mem/usb_mem.h>
#include "ehci_core.h"

//#define EHCI_LOCAL_DEBUG
#include "ehci_debug.h"

#include <if/ehci_defs.h>

#include "usb_manager_client.h"
#include "boot.h"
#include "toggle_state.h"
#include "context.h"


typedef struct client_state_t {
    struct ehci_service_response rsp;
    uint64_t qh_id;
    uint64_t client_id;
} client_state_t;

// Just for now.
client_state_t client_state[128];

volatile bool usb_to_ehci_connected = false;


static void map_dev_arr_handler(struct ehci_service_response *rsp,
                                struct capref cap, uint32_t sz)
{
    dprintf("%s \n", __func__);
    map_dev_arr(cap, sz);
    rsp->f->map_dev_arr_done(rsp, 0);
}

static void get_core_id_handler(struct ehci_service_response *rsp)
{
    uint64_t core = disp_get_core_id();
    rsp->f->get_core_id_resp(rsp, core);
}

static void dctrl_exe_handler(struct ehci_service_response *rsp,
                              struct _ehci_udr_t req, uint64_t buff_ptr,
                              uint64_t sz, uint8_t device, uint8_t debug,
                              uint32_t id)
{
    uint64_t ret_data = 0;
    dprintf("%s\n", __func__);
    usb_device_request usb_req;
    usb_req.bmRequestType = req.bmRequestType;
    usb_req.bRequest = req.bRequest;
    usb_req.wValue = req.wValue;
    usb_req.wIndex = req.wIndex;
    usb_req.wLength = req.wLength;

    //call actual core function 
    set_context(rsp);
    ret_data = usb_dctrl_exe(usb_req, (void *)buff_ptr, sz, device, debug);
    //rsp->f->dctrl_done(rsp, ret_data);
}

static void ctrl_exe_handler(struct ehci_service_response *rsp,
                             struct _ehci_udr_t req, uint8_t device,
                             uint8_t debug, uint32_t id)
{
    uint64_t ret_data = 0;
    dprintf("%s\n", __func__);
    usb_device_request usb_req;
    usb_req.bmRequestType = req.bmRequestType;
    usb_req.bRequest = req.bRequest;
    usb_req.wValue = req.wValue;
    usb_req.wIndex = req.wIndex;
    usb_req.wLength = req.wLength;

    //call exe 
    set_context(rsp);
    ret_data = usb_ctrl_exe(usb_req, device, debug);
    //rsp->f->ctrl_done(rsp, ret_data);
}

static void bulk_exe_handler(struct ehci_service_response *rsp,
                             struct _ehci_pipe_t p,
                             uintptr_t buff, uint32_t len,
                             uint8_t debug, uint32_t id)
{
    dprintf("%s\n", __func__);
    usb_pipe_t pipe;
    pipe.dev = p.dev;
    pipe.ep_number = p.ep_number;
    pipe.ep_address = p.ep_address;
    pipe.ep_dir = p.ep_dir;
    pipe.ep_type = p.ep_type;
    pipe.ep_psz = p.ep_psz;
    pipe.multi = p.multi;
    pipe.valid = p.valid;

    set_context(rsp);
    usb_bulk_exe(pipe, (void *)buff, len, debug);
}


static errval_t server_connect(struct ehci_service_response *rsp)
{
    usb_to_ehci_connected = true;
    dprintf("########## Connect at server\n");
    return SYS_ERR_OK;
}

static void server_disconnect(struct ehci_service_response *rsp)
{
    dprintf("Disconnect at server \n");
}

static void listen_cb(struct ehci_service *st, iref_t iref)
{
    assert(iref != 0);
    struct chips_context *context = chips_get_context();
    errval_t err = context->register_service("ehci_hc", iref, NULL, NULL);
    assert(err_is_ok(err));
    dprintf("EHCI server services registered at CHIPS\n");
}

static int server_thread(void *args)
{
    struct chips_context *context = chips_get_context();
    context->init();
    static struct ehci_server_call_vtbl ehci_server_call_vtbl = {
        .map_dev_arr = map_dev_arr_handler,
        .dctrl_exe = dctrl_exe_handler,
        .ctrl_exe = ctrl_exe_handler,
        .bulk_exe = bulk_exe_handler,
        .get_core_id = get_core_id_handler,
        ._disconnect = server_disconnect,
        ._listening = listen_cb,
        ._connected = server_connect,
    };

    static struct ehci_service ehci_service = {
        .f = &ehci_server_call_vtbl,
    };

    ehci_listen(&ehci_service);
    return 0;

}


/**
 * \brief Primary entry function, which starts the EHCI server and connects
 *        to the USB manager as a client. It also initiates the EHCI hardware 
 *        booting logic.
 */

int main(int argc, char **argv)
{
    printf("\n\n\n\n\n");
    printf("\n EHCI: This is a EHCI Server (Host Controller Server)\n");

    server_thread(0);           // register the HC services
    //here we go, tricky connection setup between EHCI & USB 
    // Step 1. EHCI tries to connect to USB 
    connect_to_usb_manager(0);

    // Get server response 
    messages_wait_and_handle_next();


    //EHCI connected to USB 
    dprintf("%s next step starting full fledge server\n ", __func__);

    //XXX: Wait untill USB client is connected 
    while (!usb_to_ehci_connected) {
        messages_wait_and_handle_next();
    }


    dprintf("Now booting the EHCI hardware ...\n");
    //Boot up the Host controller 
    ehci_boot(0);

    //XXX: Start out the server and wait connection from USB or driver 
    messages_handler_loop();

    return 0;
}
