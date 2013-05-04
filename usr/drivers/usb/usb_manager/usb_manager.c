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
    return;

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
        .request_read_call = usb_rx_request_read_call,
        .request_write_call = usb_rx_request_write_call,
        .request_call = usb_rx_request_call,
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

static void* usb_subsystem_base = NULL;
#define USB_SUBSYSTEM_L4_OFFSET 0x0A062000
#define USB_OHCI_OFFSET         0x00002800
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


    // Copy into correct slot

    struct capref device_range_cap = NULL_CAP;

    err = slot_alloc(&device_range_cap);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }
    struct capref tiler_cap = NULL_CAP;

    err = slot_alloc(&tiler_cap);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }

    err = cap_retype(device_range_cap, requested_caps, ObjType_DevFrame, 29);

    struct capref l3_ocm_ram = NULL_CAP;

    err = slot_alloc(&l3_ocm_ram);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }

    err = cap_retype(l3_ocm_ram, device_range_cap, ObjType_DevFrame, 26);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to mint the cap");
    }

    struct capref l3_config_registers_cap;
    err = slot_alloc(&l3_config_registers_cap);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }

    struct capref l4_domains_cap;
    err = slot_alloc(&l4_domains_cap);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }

    struct capref emif_registers_cap;
    err = slot_alloc(&emif_registers_cap);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }

    struct capref gpmc_iss_cap;
    err = slot_alloc(&gpmc_iss_cap);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }

    struct capref l3_emu_m3_sgx_cap;
    err = slot_alloc(&l3_emu_m3_sgx_cap);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }

    struct capref display_iva_cap;
    err = slot_alloc(&display_iva_cap);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }
    struct capref tmp_cap = display_iva_cap;
    tmp_cap.slot++;
    cap_delete(tmp_cap);

    struct capref l4_PER_domain_cap;
    err = slot_alloc(&l4_PER_domain_cap);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }
    struct capref l4_ABE_domain_cap;
    err = slot_alloc(&l4_ABE_domain_cap);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }
    struct capref l4_CFG_domain_cap;
    err = slot_alloc(&l4_CFG_domain_cap);
    if (err_is_fail(err)) {
        printf(" slot alloc failed.\n");
    }
    err = cap_retype(l4_PER_domain_cap, l4_domains_cap, ObjType_DevFrame, 24);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to mint the cap");
    }
    tmp_cap = l4_CFG_domain_cap;
    tmp_cap.slot++;
    cap_delete(tmp_cap);

    debug_printf("invoke frame identify\n");
    struct frame_identity frameid = {
    0, 0
    };
    struct capref iter_cap = device_range_cap;
    for (uint16_t i = 0; i < 12; i++) {
        err = invoke_frame_identify(iter_cap, &frameid);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "frameid\n");
        }
        uint32_t last = (uint32_t) (0xFFFFFFFF & (frameid.base));
        uint32_t size = frameid.bits;

        debug_printf("DevFrame: [base %x,  size=%u kB]\n", last,
                (1 << size) / 1024);
        iter_cap.slot++;
    }

    void *ret_addr;
    vspace_map_one_frame_attr(&ret_addr, 16 * 1024 * 1024, l4_CFG_domain_cap,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);

    usb_subsystem_base = ret_addr + USB_SUBSYSTEM_L4_OFFSET;
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

    /*
     * start the server
     */
    err = usb_manager_export(
            NULL /* state pointer for connect/export callbacks */,
            service_exported_cb, service_connected_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);



    /*
     * registring interrupt handler
     */


    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }
}
