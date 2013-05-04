#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <usb/usb.h>
#include <usb/usb_error.h>
#include <usb/usb_device.h>

#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>
#include <if/monitor_blocking_rpcclient_defs.h>

#include "usb_request.h"
#include "usb_device.h"











/*
 * ========================================================================
 * Service Export Functions
 * ========================================================================
 */

/*
 * service name
 */
static const char *usb_manager_name = "usb_manager_service";


/**
 * \brief
 */
static void usb_rx_connect_call(struct usb_manager_binding *bind,
        uint16_t init_config)
{
    debug_printf("server: received connect call from new device driver\n");

    /*
     * TODO: set the initial configuration
     */

    struct usb_device *dev = usb_device_get_pending();

    assert(dev != NULL);

    bind->st = dev;
    dev->usb_manager_binding = bind;

    /*
     * TODO: Configure Device
     */

    usb_device_config_complete(dev);
}

/**
 *
 */
static struct usb_manager_rx_vtbl usb_manager_handle_fn = {
.request_read_call = usb_rx_request_read_call, .request_write_call =
        usb_rx_request_write_call, .request_call = usb_rx_request_call,
        .connect_call = usb_rx_connect_call,
};

/**
 *
 */
static errval_t service_connected_cb(void *st, struct usb_manager_binding *b)
{
    debug_printf("service_connected_cb(): Setting handler functions.\n");
    b->rx_vtbl = usb_manager_handle_fn;

    return SYS_ERR_OK;
}

/**
 *
 */
static void service_exported_cb(void *st, errval_t err, iref_t iref)
{
    debug_printf("service_exported_cb(): Registring Nameserver.\n");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "service export failed.");
    }

    err = nameservice_register(usb_manager_name, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "registration with name server failed");
    }
}


/*
 *
 */
static errval_t init_device_range(void)
{
    debug_printf("Getting the phyaddr cap...\n");
    errval_t err;

    struct monitor_blocking_rpc_client *cl = get_monitor_blocking_rpc_client();
    assert(cl != NULL);

    // Request I/O Cap
    struct capref requested_caps;
    errval_t error_code;
    err = cl->vtbl.get_io_cap(cl, &requested_caps, &error_code);
    assert(err_is_ok(err) && err_is_ok(error_code));

    debug_printf("Ok, have the cap try to mint it...\n");

    /*
     * USBTLLHS_config 0x4A06 2000 2KB
USBTLLHS_ULPI 0x4A06 2800 2KB
HSUSBHOST 0x4A06 4000 2KB
OHCI 0x4A06 4800 1KB
EHCI 0x4A06 4C00 1KB
     *
     */
#define USB_PANDABOARD_OHCI_BASE 0x4A064800
#define USB_PANDABAORD_OHCI_SIZE 1024
    // Copy into correct slot

    debug_printf("calling devframe_type\n");

    struct capref ohci_cap = NULL_CAP;



    err = slot_alloc(&ohci_cap);
           if (err_is_fail(err)) {

                   printf(" slot alloc failed.\n");
           }

           err = cap_mint(ohci_cap, requested_caps, USB_PANDABOARD_OHCI_BASE, USB_PANDABOARD_OHCI_BASE+USB_PANDABAORD_OHCI_SIZE);

           if (err_is_fail(err)) {
               DEBUG_ERR(err, "failed to mint the cap");
           }
struct capref dev_frame_cap = NULL_CAP;
           err = devframe_type(&dev_frame_cap, ohci_cap, 30);
              if (err_is_fail(err)) {
                  DEBUG_ERR(err, "failed to devframe\n");
              }

           debug_printf("invoke frame identify\n");
               struct frame_identity frameid = {0, 0};
                       err = invoke_frame_identify(ohci_cap, &frameid);
                       if (err_is_fail(err)) {
                               DEBUG_ERR(err, "frameid\n");
                       }
                       debug_printf("flush....\n");
                       uint32_t first = (uint32_t)( 0xFFFFFFFF & (frameid.base>>32));
                       uint32_t last = (uint32_t)( 0xFFFFFFFF & (frameid.base));
                       uint32_t size = frameid.bits;

                       debug_printf("base addr %x, %x, , bfits=%u\n", first, last, size);
if (err_is_fail(err)) {
    DEBUG_ERR(err, "failed to mint the cap");
}
/*
struct capref caps_io = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_IO
    };

    err = cap_copy(caps_io, requested_caps);*/

    return SYS_ERR_OK;
}

/*
 * ========================================================================
 * MAIN
 */

/*
 * \brief   main
 */
int main(int argc, char *argv[])
{
    errval_t err;

    debug_printf("USB Manager started.\n");

    init_device_range();

    while(1);

    /*
     * start the server
     */
    err = usb_manager_export(
            NULL /* state pointer for connect/export callbacks */,
            service_exported_cb, service_connected_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);

// ff
    struct waitset *ws = get_default_waitset();
        while (1) {
            err = event_dispatch(ws);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch");
                break;
            }
        }
}
