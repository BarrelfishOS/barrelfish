/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <if/usb_manager_defs.h>

#include <usb/usbd.h>
#include <usb/mem/usb_mem.h>

#include "ehci_client.h"
/* 
 * This file contains primary server logic for the 
 * USB manager. Upon receiving the request, it is 
 * forwarded to the respective function on server 
 * side (proxy). 
 */

//#define USB_LOCAL_DEBUG
#include <usb/usb_debug.h>

/*typedef struct client_state_t 
{
        struct usb_manager_service_response rsp;
        uint64_t client_id;
}client_state_t;*/


volatile bool ehci_connected = false;

static void device_connect_handler(struct usb_manager_service_response *rsp,
                                   int port_num)
{
    dprintf("%s\n", __func__);
    notify_new_device(port_num);
}

static void device_disconn_handler(struct usb_manager_service_response *rsp,
                                   int port_num)
{
    //call internal port function 
    dprintf("%s\n", __func__);
}

static void pipe_req_handler(struct usb_manager_service_response *rsp,
                             uint8_t dev, uint8_t type, uint8_t dir)
{
    // Call internal function 
    printf("%s\n", __func__);
    usb_pipe_t p;
    init_pipe(dev, type, dir, &p);

    //copy, pack and send
    struct _usb_manager_pipe_t pipe;
    pipe.dev = p.dev;
    pipe.ep_number = p.ep_number;
    pipe.ep_address = p.ep_address;
    pipe.ep_dir = p.ep_dir;
    pipe.ep_type = p.ep_type;
    pipe.ep_psz = p.ep_psz;
    pipe.multi = p.multi;
    pipe.valid = p.valid;
    printf("Sending pipe valid -- [%d]\n", pipe.valid);
    rsp->f->pipe_resp(rsp, 0, pipe);
}

static void get_curr_config_handler(struct usb_manager_service_response *rsp,
                                    uint8_t dev)
{
    usb_configuration_descriptor from;
    struct _usb_manager_usb_config_t to;

    int status = get_curr_config(dev, &from);
    if (status == 0) {
        // Marshalling of parameters
        to.bLength = from.bLength;
        to.bDescriptorType = from.bDescriptorType;
        to.wTotalLength = from.wTotalLength;
        to.bNumInterfaces = from.bNumInterfaces;
        to.bConfigurationValue = from.bConfigurationValue;
        to.iConfiguration = from.iConfiguration;
        to.bmAttributes = from.bmAttributes;
        to.bMaxPower = from.bMaxPower;
    }

    rsp->f->get_curr_config_resp(rsp, status, to);
}


static void get_num_config_handler(struct usb_manager_service_response *rsp,
                                   uint8_t dev)
{
    uint8_t num;
    get_num_config(dev, &num);
    rsp->f->get_num_config_resp(rsp, num);

}


static void get_config_handler(struct usb_manager_service_response *rsp,
                               uint8_t dev, uint8_t num)
{
    usb_configuration_descriptor from;
    struct _usb_manager_usb_config_t to;

    int status = get_config(dev, num, &from);
    if (status == 0) {
        // Marshalling of parameters
        to.bLength = from.bLength;
        to.bDescriptorType = from.bDescriptorType;
        to.wTotalLength = from.wTotalLength;
        to.bNumInterfaces = from.bNumInterfaces;
        to.bConfigurationValue = from.bConfigurationValue;
        to.iConfiguration = from.iConfiguration;
        to.bmAttributes = from.bmAttributes;
        to.bMaxPower = from.bMaxPower;
    }

    rsp->f->get_config_resp(rsp, status, to);

}

static void set_config_handler(struct usb_manager_service_response *rsp,
                               uint8_t dev, uint8_t num)
{
    //XXX: Not implemented 
    rsp->f->set_config_resp(rsp, -1);
}

static void get_curr_intf_handler(struct usb_manager_service_response *rsp,
                                  uint8_t dev)
{
    usb_interface_descriptor from;
    struct _usb_manager_usb_intf_t to;

    int status = get_curr_intf(dev, &from);
    if (status == 0) {
        // Marshalling of paramters
        to.bLength = from.bLength;
        to.bDescriptorType = from.bDescriptorType;
        to.bInterfaceNumber = from.bInterfaceNumber;
        to.bAlternateSetting = from.bAlternateSetting;
        to.bNumEndpoints = from.bNumEndpoints;
        to.bInterfaceClass = from.bInterfaceClass;
        to.bInterfaceSubClass = from.bInterfaceSubClass;
        to.bInterfaceProtocol = from.bInterfaceProtocol;
        to.iInterface = from.iInterface;
    }

    rsp->f->get_curr_intf_resp(rsp, status, to);
}


static void get_num_intf_handler(struct usb_manager_service_response *rsp,
                                 uint8_t dev)
{
    uint8_t num;
    get_num_intf(dev, &num);
    rsp->f->get_num_intf_resp(rsp, num);

}


static void get_intf_handler(struct usb_manager_service_response *rsp,
                             uint8_t dev, uint8_t num)
{
    usb_interface_descriptor from;
    struct _usb_manager_usb_intf_t to;

    int status = get_intf(dev, num, &from);
    if (status == 0) {
        // Marshalling of parameters    
        to.bLength = from.bLength;
        to.bDescriptorType = from.bDescriptorType;
        to.bInterfaceNumber = from.bInterfaceNumber;
        to.bAlternateSetting = from.bAlternateSetting;
        to.bNumEndpoints = from.bNumEndpoints;
        to.bInterfaceClass = from.bInterfaceClass;
        to.bInterfaceSubClass = from.bInterfaceSubClass;
        to.bInterfaceProtocol = from.bInterfaceProtocol;
        to.iInterface = from.iInterface;

    }

    rsp->f->get_intf_resp(rsp, status, to);

}

static void set_intf_handler(struct usb_manager_service_response *rsp,
                             uint8_t dev, uint8_t num)
{
    //XXX: Not implemented 
    rsp->f->set_intf_resp(rsp, -1);
}

static errval_t server_connect(struct usb_manager_service_response *rsp)
{
    printf("USBD: Connect at usb_manager server\n");
    ehci_connected = true;
    return SYS_ERR_OK;
}

static void server_disconnect(struct usb_manager_service_response *rsp)
{
    printf("USBD: Disconnect at server usb_manager\n");
}

static void listen_cb(struct usb_manager_service *st, iref_t iref)
{
    assert(iref != 0);
    struct chips_context *context = chips_get_context();
    errval_t err = context->register_service("usb_manager", iref, NULL, NULL);
    assert(err_is_ok(err));
    printf("USBD: Service usb_manager registered to the CHIPS server...\n");
}

static int start_server(void *args)
{
    struct chips_context *context = chips_get_context();
    context->init();

    printf("USBD: Context started \n");

    static struct usb_manager_server_call_vtbl usb_manager_server_call_vtbl = {
        .device_connect = device_connect_handler,
        .device_disconnect = device_disconn_handler,
        .pipe_req = pipe_req_handler,
        .get_curr_config = get_curr_config_handler,
        .get_num_config = get_num_config_handler,
        .get_config = get_config_handler,
        .set_config = set_config_handler,
        .get_curr_intf = get_curr_intf_handler,
        .get_num_intf = get_num_intf_handler,
        .get_intf = get_intf_handler,
        .set_intf = set_intf_handler,

        ._disconnect = server_disconnect,
        ._listening = listen_cb,
        ._connected = server_connect,
    };

    static struct usb_manager_service usb_manager_service = {
        .f = &usb_manager_server_call_vtbl,
    };



    //start thread for client connection 
    usb_manager_listen(&usb_manager_service);
    return 0;
}

int main(int argc, char **argv)
{
    printf("\n\n\n\n ");
    printf("USBD: This is USB Manager Server \n");
    usb_mem_init();

    start_server(0);
    //XXX: Requires 2 steps to connect to server from EHCI
    printf("USBD: Waiting for EHCI messages for connections\n ");
    while (!ehci_connected) {
        messages_wait_and_handle_next();
    }

    printf("USBD: EHCI is not now connected to USBD \n");
    //XXX: EHCI connected to USB server
    //XXX: Our turn to connect to EHCI server       

    connect_to_ehci_manager(0);
    messages_wait_and_handle_next();

    //XXX: Connected to EHCI server ...start out server     
    usb_manager_init();

    printf("USBD: Starting messages handler loop\n ");
    messages_handler_loop();

    return 0;
}
