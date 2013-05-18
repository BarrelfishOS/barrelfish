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

#include "usb_controller.h"
#include "usb_request.h"
#include "usb_device.h"
#include "usb_transfer.h"

static usb_host_controller_t *host_controllers = NULL;

/*
 * ------------------------------------------------------------------------
 * Service connect
 * ------------------------------------------------------------------------
 */

/// the service name to export
static const char *usb_manager_name = "usb_manager_service";

/**
 * struct representing the state of a new USB driver connection
 */
struct usb_manager_connect_state {
    struct usb_manager_binding *b;  ///< the usb_manager_binding struct
    usb_error_t error;              ///< the outcome of the initial setup
};

static void usb_driver_connect_cb(void *a)
{
    USB_DEBUG("driver connect call sucessfull terminated\n");
    free(a);
}

static void usb_driver_test_cb(void *a)
{
    USB_DEBUG("driver xfer done notify successful terminated\n");
}

struct usb_manager_connect_state test;

static void usb_driver_test_response(void *a) {
    USB_DEBUG("sending xfer done notify\n");
    struct event_closure txcont = MKCONT(usb_driver_test_cb, &test);
    errval_t err;
        uint8_t data[4];
        err = usb_manager_transfer_done_notify__tx(test.b, txcont, 1, 0,  data, 4);


        if (err_is_fail(err)) {
            if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
                // try to resend
                USB_DEBUG("resending done notify response\n");
                            txcont = MKCONT(usb_driver_test_response, &test);
                            err = test.b->register_send(test.b, get_default_waitset(), txcont);
                            if (err_is_fail(err)) {
                                DEBUG_ERR(err, "failed to register send");
                            }
            } else {
                // error
                DEBUG_ERR(err, "error while sending done notify");

            }
        }
}


static void usb_driver_connect_response(void *a)
{
    errval_t err;
    struct usb_manager_connect_state *st = a;

    test.b = st->b;

    USB_DEBUG("sending driver connect response\n");

    struct event_closure txcont = MKCONT(usb_driver_connect_cb, st);

    err = usb_manager_connect_response__tx(st->b, txcont, st->error);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            // try to resend
            USB_DEBUG("resending driver connect response\n");
            txcont = MKCONT(usb_driver_connect_response, st);
            err = st->b->register_send(st->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "failed to register send");
            }
        } else {
            // error
            DEBUG_ERR(err, "error while seniding driver connect response");
            free(st);
        }
    }
    usb_driver_test_response(NULL);
}


/**
 * \brief
 */
static void usb_rx_connect_call(struct usb_manager_binding *bind,
        uint16_t init_config)
{
    debug_printf("server: received connect call from new device driver\n");

    struct usb_manager_connect_state *st;

    st = malloc(sizeof(struct usb_manager_connect_state));

    if (st == NULL) {
        USER_PANIC("cannot reply, out of memory!");
    }

    st->b = bind;

    /*
     * TODO: DEVICE SETUP
     */
#if 0
    struct usb_device *dev = usb_device_get_pending();

        assert(dev != NULL);

        bind->st = dev;
        dev->usb_manager_binding = bind;

        /*
         * TODO: Configure Device
         */

        usb_device_config_complete(dev);
#endif
    st->error = USB_ERR_OK;

    // send response
    usb_driver_connect_response(st);
}

/**
 *
 */
static struct usb_manager_rx_vtbl usb_manager_handle_fn = {
        .request_read_call = usb_rx_request_read_call,
        .request_write_call = usb_rx_request_write_call,
        .request_call = usb_rx_request_call,
        .connect_call = usb_rx_connect_call,
        .transfer_setup_call = usb_rx_transfer_setup_call,
         .transfer_unsetup_call = usb_rx_transfer_unsetup_call,
         .transfer_start_call =  usb_rx_transfer_start_call,
         .transfer_stop_call = usb_rx_transfer_stop_call,
         .transfer_status_call =  usb_rx_transfer_status_call,
         .transfer_state_call = usb_rx_transfer_state_call,
         .transfer_clear_stall_call = usb_rx_transfer_clear_stall_call,
};




/**
 *
 */
static errval_t service_connected_cb(void *st, struct usb_manager_binding *b)
{
    debug_printf("service_connected_cb(): Setting handler functions.\n");
    b->rx_vtbl = usb_manager_handle_fn;

    return (SYS_ERR_OK);
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
#define USB_SUBSYSTEM_L4_OFFSET 0x00062000
//#define USB_OHCI_OFFSET         (0x000A9000-USB_SUBSYSTEM_L4_OFFSET)
#define USB_OHCI_OFFSET         0x00002800
#define USB_EHCI_OFFSET         0x00002C00

/*
 *
 */
static errval_t init_device_range(void)
{
    USB_DEBUG("Setting up device range.\n");
    errval_t err;

    struct monitor_blocking_rpc_client *cl = get_monitor_blocking_rpc_client();
    assert(cl != NULL);

    // Request I/O Cap
    struct capref requested_caps;
    errval_t error_code;
    err = cl->vtbl.get_io_cap(cl, &requested_caps, &error_code);
    assert(err_is_ok(err) && err_is_ok(error_code));

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
#if 0

    struct capref iter_cap = device_range_cap;
    for (uint16_t i = 0; i < 12; i++) {
        err = invoke_frame_identify(iter_cap, &frameid);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "frameid\n");
        }
        uint32_t last = (uint32_t) (0xFFFFFFFF & (frameid.base));
        uint32_t size = frameid.bits;

        debug_printf("DevFrame: [base %p,  size=%u kB]\n", last,
                (1 << size) / 1024);
        iter_cap.slot++;
    }
#endif

    err = invoke_frame_identify(l4_CFG_domain_cap, &frameid);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frameid\n");
    }
    uint32_t last = (uint32_t) (0xFFFFFFFF & (frameid.base));
    uint32_t size2 = frameid.bits;

    debug_printf("L4_CFG_domain_cap: [base %p,  size=%u kB]\n", last,
            (1 << size2) / 1024);

    void *ret_addr = NULL;
    size_t size = 16 * 1024 * 1024;

#define MAP_ANON 0

#if MAP_ANON
    struct memobj *memobj;
    struct vregion *vregion;
    err = vspace_map_anon_attr(&ret_addr, &memobj, &vregion, size, NULL,
            VREGION_FLAGS_READ_WRITE_NOCACHE);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to create a vspace mapping");
    }

    err = memobj->f.fill(memobj, 0, l4_CFG_domain_cap, size);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to fill");
    }

    err = memobj->f.pagefault(memobj, vregion, USB_SUBSYSTEM_L4_OFFSET, 0);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to fault");
    }

#else
    err = vspace_map_one_frame_attr(&ret_addr, size, l4_CFG_domain_cap,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to create a vspace mapping");
    }
#endif

    usb_subsystem_base = ret_addr + USB_SUBSYSTEM_L4_OFFSET;

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



    /*
     * start the server
     */
    err = usb_manager_export(
            NULL /* state pointer for connect/export callbacks */,
            service_exported_cb, service_connected_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);

#if __arm__
    init_device_range();
    argc = 4;
    uint32_t tmp =USB_OHCI_OFFSET;
    char ohci_base[4];
    memcpy(ohci_base, &tmp, 4);

    tmp = USB_EHCI_OFFSET;
    char ehci_base[4];
    memcpy(ehci_base, &tmp, 4);
    argv = (char *[]) {"ehci", ehci_base, "ohci", ohci_base};
#endif

    /*
     * parse command line args
     */
    if (argc == 0 || argc % 2) {
        debug_printf("Usage: usb_manager [host-controller offset]\n");
    }

    usb_error_t uerr = USB_ERR_OK;
    for (uint16_t i = 0; i < argc; i+=2) {
        usb_host_controller_t *hc = NULL;
        uint32_t offset = *((uint32_t*)argv[i+1]);
        if (strcmp(argv[i], "ehci") == 0) {
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_EHCI, usb_subsystem_base + offset);
        }

        if (strcmp(argv[i], "ohci") == 0) {
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_OHCI, usb_subsystem_base + offset);
        }
        if (strcmp(argv[i], "uhci") == 0) {
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_UHCI, usb_subsystem_base + offset);
        }

        if (strcmp(argv[i], "xhci") == 0) {
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_XHCI, usb_subsystem_base + offset);
        }

        if (uerr != USB_ERR_OK && hc != NULL) {
            free(hc);
            continue;
        }

        hc->next = host_controllers;
        host_controllers = hc;
    }

    /*
     * registring interrupt handler
     * inthandler_setup()
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
