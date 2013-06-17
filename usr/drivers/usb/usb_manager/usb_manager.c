#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/inthandler.h>

#include <usb/usb.h>
#include <usb/usb_error.h>

#include <if/usb_driver_defs.h>
#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>

#include <usb_controller.h>
#include <usb_request.h>
#include <usb_device.h>
#include <usb_transfer.h>
#include <usb_driver.h>

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
    struct usb_manager_binding *b;      ///< the usb_manager_binding struct
    struct usb_driver_binding *driver;
    void *desc;                        ///< generic descriptor
    uint32_t length;                    ///< length of the descirptor
    usb_error_t error;                  ///< the outcome of the initial setup
    iref_t driver_iref;
};

static void usb_driver_bind_cb(void *st, errval_t err,
        struct usb_driver_binding *b)
{
    USB_DEBUG("usb_driver_bind_cb\n");
    if (err_is_fail(err)) {
        USB_DEBUG("bind failed..\n");
    }

    struct usb_manager_connect_state *cs = st;

    cs->driver = b;
    struct usb_device *dev = cs->b->st;
    dev->usb_driver_binding = b;

    free(cs->desc);
    free(cs);
}

static void usb_driver_connect_cb(void *a)
{
    USB_DEBUG("usb_driver_connect_cb->binding...\n");
    struct usb_manager_connect_state *st = a;
    errval_t err = usb_driver_bind(st->driver_iref, usb_driver_bind_cb, st,
            get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {

    }
}

static void usb_driver_connect_response(void *a)
{
    errval_t err;
    struct usb_manager_connect_state *st = a;

    struct event_closure txcont = MKCONT(usb_driver_connect_cb, st);

    err = usb_manager_connect_response__tx(st->b, txcont, st->error, st->desc,
            st->length);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            // try to resend
            txcont = MKCONT(usb_driver_connect_response, st);
            err = st->b->register_send(st->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "failed to register send");
            }
        } else {
            // error
            DEBUG_ERR(err, "error while seniding driver connect response");
            free(st->desc);
            free(st);
        }
    }
}

/**
 * \brief
 */
static void usb_rx_connect_call(struct usb_manager_binding *bind,
        iref_t driver_iref, uint16_t init_config)
{
    struct usb_manager_connect_state *st;

    st = malloc(sizeof(struct usb_manager_connect_state));

    if (st == NULL) {
        USER_PANIC("cannot reply, out of memory!");
    }

    st->b = bind;
    st->driver_iref = driver_iref;


    usb_driver_connected(bind, st->driver, init_config);

    st->error = USB_ERR_OK;

    if (bind->st == NULL) {
        /* error */
        debug_printf("ERROR: no state associated..\n");
        st->error = USB_ERR_IOERROR;
        usb_driver_connect_response(st);
        return;
    }

    struct usb_device *dev = bind->st;
    st->length = sizeof((dev->device_desc)) + dev->config_desc_size;
    st->desc = malloc(st->length);

    memcpy(st->desc, &(dev->device_desc), sizeof((dev->device_desc)));
    memcpy(st->desc + sizeof((dev->device_desc)), dev->config_desc,
            dev->config_desc_size);

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
    .transfer_start_call = usb_rx_transfer_start_call,
    .transfer_stop_call = usb_rx_transfer_stop_call,
    .transfer_status_call = usb_rx_transfer_status_call,
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

//static void* usb_subsystem_base = NULL;

#if !__arm__
#define USB_SUBSYSTEM_L4_OFFSET 0x00062000
//#define USB_OHCI_OFFSET         (0x000A9000-USB_SUBSYSTEM_L4_OFFSET)
#define USB_OHCI_OFFSET         0x00002800
#define USB_EHCI_OFFSET         0x00002C00

#define USB_ARM_EHCI_IRQ 109
/*
 *
 */
static errval_t init_device_range(void)
{
    USB_DEBUG("doing pandaboard related setup...\n");
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
        printf("slot alloc failed. Step 1\n");
        return (err);
    }
    struct capref tiler_cap = NULL_CAP;

    err = slot_alloc(&tiler_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc failed. Step 1\n");
        return (err);
    }

    err = cap_retype(device_range_cap, requested_caps, ObjType_DevFrame, 29);

    struct capref l3_ocm_ram = NULL_CAP;

    err = slot_alloc(&l3_ocm_ram);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc failed. Step 2\n");
        return (err);
    }

    err = cap_retype(l3_ocm_ram, device_range_cap, ObjType_DevFrame, 26);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retype the dev cap. Step 3\n");
        return (err);
    }

    struct capref l3_config_registers_cap;
    err = slot_alloc(&l3_config_registers_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot alloc failed. Step 4\n");
        return (err);
    }

    struct capref l4_domains_cap;
    err = slot_alloc(&l4_domains_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc failed. Step 5\n");
        return (err);
    }

    struct capref emif_registers_cap;
    err = slot_alloc(&emif_registers_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc failed. Step 6\n");
        return (err);
    }

    struct capref gpmc_iss_cap;
    err = slot_alloc(&gpmc_iss_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc failed. Step 7\n");
        return (err);
    }

    struct capref l3_emu_m3_sgx_cap;
    err = slot_alloc(&l3_emu_m3_sgx_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc failed. Step 8\n");
        return (err);
    }

    struct capref display_iva_cap;
    err = slot_alloc(&display_iva_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc failed. Step 9\n");
        return (err);
    }
    struct capref tmp_cap = display_iva_cap;
    tmp_cap.slot++;
    cap_delete(tmp_cap);

    struct capref l4_PER_domain_cap;
    err = slot_alloc(&l4_PER_domain_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc failed. Step 12\n");
        return (err);
    }

    struct capref l4_ABE_domain_cap;
    err = slot_alloc(&l4_ABE_domain_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc failed. Step 11\n");
        return (err);
    }

    struct capref l4_CFG_domain_cap;
    err = slot_alloc(&l4_CFG_domain_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc failed. Step 12\n");
        return (err);
    }

    err = cap_retype(l4_PER_domain_cap, l4_domains_cap, ObjType_DevFrame, 24);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retype the cap. Step 13\n");
        return (err);
    }
    tmp_cap = l4_CFG_domain_cap;
    tmp_cap.slot++;
    cap_delete(tmp_cap);

    struct frame_identity frameid;  // = {        0,        0    };

    err = invoke_frame_identify(l4_CFG_domain_cap, &frameid);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not identify the frame. Step 14\n");
    }

    struct capref dest = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_ARGCN
    };

    err = cap_copy(dest, l4_CFG_domain_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not copy to the argcn slot");
    }

    // get the 32 bit
    uint32_t last = (uint32_t) (0xFFFFFFFF & (frameid.base));
    uint32_t size2 = frameid.bits;

    /* the L4 CFG domain cap must have address 0x4A000000 */
    assert(last == 0x4a000000);

    /* the size of the L4 CFG domain is 16k */
    assert(((1 << size2) / 1024) == (16 * 1024));

    err = inthandler_setup_arm(usb_hc_intr_handler, NULL, USB_ARM_EHCI_IRQ);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to enable interrupt. Step 16.\n");
    }

    USB_DEBUG("pandaboard related setup completed successfully.\n");

    return (SYS_ERR_OK);
}

#endif

static uintptr_t map_device_cap(void)
{
    errval_t err;

    struct capref dev_cap = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_ARGCN
    };

    struct frame_identity frameid;  // = {        0,        0    };

    err = invoke_frame_identify(dev_cap, &frameid);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not identify the frame.\n");
    }

    void *ret_addr = NULL;
    size_t size = (1UL << frameid.bits); /* bytes */

    err = vspace_map_one_frame_attr(&ret_addr, size, dev_cap,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to create a vspace mapping. Step 15\n");
    }

    return ((uintptr_t) ret_addr);
}

#if __arm__
#define USB_SUBSYSTEM_L4_OFFSET 0x00062000
#define USB_EHCI_OFFSET         0x00002C00
#define USB_ARM_EHCI_IRQ 109
static usb_error_t pandaboard_checkup(uintptr_t base, int argc, char *argv[])
{
    USB_DEBUG("performing pandaboard integrity check.\n");

    if (strcmp(argv[0], "ehci")) {
        debug_printf("wrong host controller type: %s\n", argv[0]);
        return (USB_ERR_INVAL);
    }

    if (strtoul(argv[1], NULL, 10)
            != ((uint32_t) USB_EHCI_OFFSET + USB_SUBSYSTEM_L4_OFFSET)) {
        debug_printf("wrong offset!: %x (%s)\n", strtoul(argv[1], NULL, 10),
                argv[1]);
        return (USB_ERR_INVAL);
    }

    if (strtoul(argv[2], NULL, 10) != USB_ARM_EHCI_IRQ) {
        debug_printf("wrong interrupt number: %s, %x", argv[2],
                strtoul(argv[2], NULL, 10));
        return (USB_ERR_INVAL);
    }

    /* the ehci ULPI register of the omap */

    uint32_t tmp = USB_EHCI_OFFSET + USB_SUBSYSTEM_L4_OFFSET + (uint32_t) base;

    /* read the debug register and check line state */
    *((volatile uint32_t*) (tmp + 0x00A4)) = (uint32_t) ((0x15 << 16)
            | (0x3 << 22) | (0x1 << 24) | (0x1 << 31));
    while (*((volatile uint32_t*) (tmp + 0x00A4)) & (1 << 31)) {
        printf("%c", 0xE);

    }

    if (!(*(((volatile uint32_t*) (tmp + 0x00A4))) & 0x1)) {
        return (USB_ERR_INVAL);
    }

    /* read the vendor low register and check if it has the corrent value */
    *((volatile uint32_t*) (tmp + 0x00A4)) = (uint32_t) ((0x00 << 16)
            | (0x3 << 22) | (0x1 << 24) | (0x1 << 31));
    while (*((volatile uint32_t*) (tmp + 0x00A4)) & (1 << 31)) {
        printf("%c", 0xE);
    }

    if (0x24 != ((*((volatile uint32_t*) (tmp + 0x00A4))) & 0xFF)) {
        return (USB_ERR_INVAL);
    }

    USB_DEBUG("pandaboard check passed. All fine.\n");
    return (USB_ERR_OK);
}
#endif

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

    uintptr_t base = map_device_cap();

#if !__arm__
    init_device_range();
    argc = 2;

    /* just settuing up the params for the OMAP ehci controller */

    uint32_t tmp = (uint32_t) USB_EHCI_OFFSET + USB_SUBSYSTEM_L4_OFFSET;
    char ehci_base[4];
    memcpy(ehci_base, &tmp, 4);
    argv = (char *[]) {"ehci", ehci_base};
#endif

    uint8_t arg_tuple_size = 2;

#if __arm__

    if (pandaboard_checkup(base, argc, argv) != USB_ERR_OK) {
        USER_PANIC("Pandaboard checkup failed!\n");
    }

    arg_tuple_size = 3;
    /*
     * parse command line args
     */
    if (argc != 3) {
        debug_printf("Usage: usb_manager [host-controller offset interrupt]\n");
    }

    uint32_t irq = strtoul(argv[2], NULL, 10);

    err = inthandler_setup_arm(usb_hc_intr_handler, NULL, irq);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to enable interrupt. Step 16.\n");
    }

#else
    if (argc == 0 || argc % 2) {
        debug_printf("Usage: usb_manager [host-controller offset]\n");
    }
    uint32_t intr_vector;
    err = inthandler_setup(usb_hc_intr_handler, NULL,
            uint32_t &intr_vector);
    /* TODO: register interrupt routing.. */
#endif

    usb_error_t uerr = USB_ERR_OK;
    for (uint16_t i = 0; i < argc; i += arg_tuple_size) {
        usb_host_controller_t *hc = NULL;
        uintptr_t controller_base = base + strtoul(argv[i + 1], NULL, 10);
        if (strcmp(argv[i], "ehci") == 0) {
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_EHCI, controller_base);
        }

        if (strcmp(argv[i], "ohci") == 0) {
            continue;
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_OHCI, controller_base);
        }
        if (strcmp(argv[i], "uhci") == 0) {
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_UHCI, controller_base);
        }

        if (strcmp(argv[i], "xhci") == 0) {
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_XHCI, controller_base);
        }

        if (uerr != USB_ERR_OK && hc != NULL) {
            free(hc);
            continue;
        }
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
