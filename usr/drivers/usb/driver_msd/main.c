/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <stdio.h>

#include "driver.h"

#include <if/usb_driver_defs.h>
#include <usb/mem/usb_mem.h>

#include "mass_storage_driver.h"
#include "ehci_client.h"
#include "usb_manager_client.h"


#define SELF_CLASS     0x08
#define SELF_SUBCLASS  0x06
#define SELF_PROTOCOL  0x50

volatile bool usb_client = false;
volatile bool probe_req = false;

/*
 * \brief Connect function which connects driver to the 
 *        EHCI and USB manager as clients. 
 */

static int connT(void)
{
    connect_to_ehci_manager(0);
    messages_wait_and_handle_next();

    connect_to_usb_manager(0);
    messages_wait_and_handle_next();
    return 0;
}

/*
 * \brief Internal probe handler. Right now it does nothing except 
 *        accepting the device and sending the ACCEPT response back. 
 *        But in future, acceptance of device can be subjected to 
 *        many questions like resource constrains. 
 */
static void probe_handler(struct usb_driver_service_response *rsp,
                          uint8_t dev, uint8_t class, uint8_t subclass,
                          uint8_t protocol)
{
    probe_req = true;

    printf("USB_DRIVER: %s for dev[%u] \n", __func__, dev);

    if (class == SELF_CLASS
        && subclass == SELF_SUBCLASS && protocol == SELF_PROTOCOL) {
        connT();
        rsp->f->probe_done(rsp, ACCEPT, dev);
        handle_device(dev);
    } else
        rsp->f->probe_done(rsp, REJECT, dev);
}


static void disconnect_handler(struct usb_driver_service_response *rsp,
                               uint8_t dev)
{
    printf("\n USB_DRIVER: %s ", __func__);
    //clean_up_and_shutdown();
}

static void get_scsi_dev_handler(struct usb_driver_service_response *rsp)
{
    scsi_device_t *dev = export_scsi_dev();
    struct _usb_driver_scsi_dev_t device;
    if (dev == NULL) {
        device.valid = 0;
    } else {
        device.llb = dev->block_last;
        device.size = dev->block_size;
        device.valid = 1;
        device.usb_add = dev->usb_address;
    }

    rsp->f->send_dev(rsp, device);
}

static void read_scsi_handler(struct usb_driver_service_response *rsp,
                              struct _usb_driver_scsi_dev_t dev, uint32_t start,
                              uint16_t num, uint32_t buff, uint8_t cache)
{
    void *temp = (void *)((uint64_t) buff);
    read10(dev.usb_add, start, num, temp, cache);
}

static void write_scsi_handler(struct usb_driver_service_response *rsp,
                               struct _usb_driver_scsi_dev_t dev,
                               uint32_t start, uint16_t num, uint32_t buff,
                               uint8_t cache)
{
    void *temp = (void *)((uint64_t) buff);
    write10(dev.usb_add, start, num, temp, cache);

}

static void sync_cache_handler(struct usb_driver_service_response *rsp,
                               struct _usb_driver_scsi_dev_t dev,
                               uint32_t start, uint16_t num)
{
    sync_cache_on_dev(dev.usb_add, start, num);

}

static errval_t server_connect(struct usb_driver_service_response *rsp)
{
    printf("USB_DRIVER: Connect at server \n");
    usb_client = true;

    return SYS_ERR_OK;
}

static void server_disconnect(struct usb_driver_service_response *rsp)
{
    printf("\n USB_DRIVER: Disconnect at server \n");
}

static void listen_cb(struct usb_driver_service *st, iref_t iref)
{
    assert(iref != 0);
    struct chips_context *context = chips_get_context();
    errval_t err = context->register_service("msd_driver",
                                             iref, NULL, NULL);
    assert(err_is_ok(err));
    printf("\n USB_DRIVER: Mass storage driver services" "registered at CHIPS");
}

static void start_server(void)
{
    struct chips_context *context = chips_get_context();
    context->init();
    static struct usb_driver_server_call_vtbl usb_driver_server_call_vtbl = {
        .probe = probe_handler,
        .disconnect = disconnect_handler,
        .get_scsi_dev = get_scsi_dev_handler,
        .read_scsi = read_scsi_handler,
        .write_scsi = write_scsi_handler,
        .sync_cache = sync_cache_handler,
        ._disconnect = server_disconnect,
        ._listening = listen_cb,
        ._connected = server_connect,
    };

    static struct usb_driver_service usb_driver_service = {
        .f = &usb_driver_server_call_vtbl,
    };

    usb_driver_listen(&usb_driver_service);
}



/*
 * \brief MSD (Mass Storage Driver) has a very tricky setup. It explicitly 
 *        waits for USB manager to ask it, when manager found a device. 
 *        When probe is called, and driver accepts the device, then it 
 *        connects as a client to both EHCI and USB manager for further 
 *        execution. 
 */

int main(int argc, char **argv)
{
    printf("\nUSB_DRIVER: Booting the standalone driver "
           "Registering the device server.\n");
    start_server();
    usb_mem_init();

    //USB manager notifies 
    while (!usb_client) {
        messages_wait_and_handle_next();
    }
    //Ok USB client connected  

    //Wait for explicit probe request 
    while (!probe_req) {
        messages_wait_and_handle_next();
    }


    //probe done 
    printf("\n USB_DRIVER: loop started\n");
    messages_handler_loop();
    return 0;
}
