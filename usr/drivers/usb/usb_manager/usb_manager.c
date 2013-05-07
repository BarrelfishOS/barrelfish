#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <pager/pager.h>

#include <usb/usb.h>
#include <usb/usb_error.h>
#include <usb/usb_device.h>

#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>
#include <if/monitor_blocking_rpcclient_defs.h>

#include "usb_controller.h"
#include "usb_request.h"
#include "usb_device.h"

static usb_host_controller_t *host_controllers = NULL;

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
#define USB_SUBSYSTEM_L4_OFFSET 0x00062000
//#define USB_OHCI_OFFSET         (0x000A9000-USB_SUBSYSTEM_L4_OFFSET)
#define USB_OHCI_OFFSET         0x00002800
#define USB_EHCI_OFFSET         0x00000000
#define USB_UHCI_OFFSET         0x00000000
#define USB_XHCI_OFFSET         0x00000000
/*
 * USBTLLHS_config 0x4A06 2000 2KB
 USBTLLHS_ULPI 0x4A06 2800 2KB
 HSUSBHOST 0x4A06 4000 2KB
 OHCI       0x4A06 4800 1KB
 EHCI 0x4A06 4C00 1KB
 *
 */
#define USB_PANDABOARD_OHCI_BASE 0x4A064800
#define USB_PANDABAORD_OHCI_SIZE 1024

#if __arm__
static void pandaboard_enable_usb(void *l4_cfg_base)
{
    USB_DEBUG("doing pandaboard related setup.\n");

    /*
     * enable static dependencies
     */
    volatile uint32_t *CM_L3INIT_STATICDEP = (uint32_t *) (l4_cfg_base + 0x9304);
    *CM_L3INIT_STATICDEP = 0xFFFF;

    /*
     * enable the HS usb host control
     */
    volatile uint32_t *CM_L3INIT_HSUSBHOST_CLKCTRL = (uint32_t *) (l4_cfg_base + 0x9358);
    *CM_L3INIT_HSUSBHOST_CLKCTRL = (0x2 | 0xFF00);

    /*
     * enable HS USB OTG Clock
     */
    volatile uint32_t *CM_L3INIT_HSUSBOTG_CLKCTRL = (uint32_t *) (l4_cfg_base +0x9360);
    *CM_L3INIT_HSUSBOTG_CLKCTRL = (0x1 << 24) | (0x1 << 8) | 0x1;

    // enable l3 usbphy clock control
    volatile uint32_t *CM_L3INIT_USBPHY_CLKCTRL = (uint32_t *) (l4_cfg_base +0x93E0);
    *CM_L3INIT_USBPHY_CLKCTRL = (0x1 << 8) | 0x1;

    /*
     * set input enable
     */

    volatile uint32_t * USBB1_ULPITLL_CLK = (uint32_t *)(l4_cfg_base+0x1000C0);
    *USBB1_ULPITLL_CLK = (*USBB1_ULPITLL_CLK) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19);

    volatile uint32_t * USBB1_ULPITLL_DIR = (uint32_t *)(l4_cfg_base+0x1000C4);
    *USBB1_ULPITLL_DIR = (*USBB1_ULPITLL_CLK) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19)
    | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * USBB1_ULPITLL_DAT0 = (uint32_t *)(l4_cfg_base+0x1000C8);
    *USBB1_ULPITLL_DAT0 = (*USBB1_ULPITLL_DAT0) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19)
    | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * USBB1_ULPITLL_DAT2 = (uint32_t *)(l4_cfg_base+0x1000CC);
    *USBB1_ULPITLL_DAT2 = (*USBB1_ULPITLL_DAT2) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19)
    | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * USBB1_ULPITLL_DAT4 = (uint32_t *)(l4_cfg_base+0x1000D0);
    *USBB1_ULPITLL_DAT4 = (*USBB1_ULPITLL_DAT4) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19)
    | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * USBB1_ULPITLL_DAT6 = (uint32_t *)(l4_cfg_base+0x1000D4);
    *USBB1_ULPITLL_DAT6 = (*USBB1_ULPITLL_DAT6) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19)
    | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * DAT7_PAD1_USBB1_HSIC_DATA = (uint32_t *)(l4_cfg_base+0x1000D8);
    *DAT7_PAD1_USBB1_HSIC_DATA = (*DAT7_PAD1_USBB1_HSIC_DATA) |
    (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);
    volatile uint32_t * USBC1_ICUSB_DP = (uint32_t *)(l4_cfg_base+0x1000DC);
    *USBC1_ICUSB_DP = (*USBC1_ICUSB_DP) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19);

    volatile uint32_t * USBC1_ICUSB_DM = (uint32_t *)(l4_cfg_base+0x1000E0);
    *USBC1_ICUSB_DM = (*USBC1_ICUSB_DM) | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * USBB2_ULPITLL_STP = (uint32_t *)(l4_cfg_base+0x100160);
    *USBB2_ULPITLL_STP = (*USBB2_ULPITLL_STP) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19)
    | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * PAD1_USBB2_ULPITLL_NXT = (uint32_t *)(l4_cfg_base+0x100164);
    *PAD1_USBB2_ULPITLL_NXT = (*PAD1_USBB2_ULPITLL_NXT) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19)
    | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * USBB2_ULPITLL_DAT1 = (uint32_t *)(l4_cfg_base+0x100168);
    *USBB2_ULPITLL_DAT1 = (*PAD1_USBB2_ULPITLL_NXT) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19)
    | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * USBB2_ULPITLL_DAT3 = (uint32_t *)(l4_cfg_base+0x10016C);
    *USBB2_ULPITLL_DAT3 = (*USBB2_ULPITLL_DAT3) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19)
    | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * USBB2_ULPITLL_DAT5 = (uint32_t *)(l4_cfg_base+0x100170);
    *USBB2_ULPITLL_DAT5 = (*USBB2_ULPITLL_DAT5) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19)
    | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * USBB2_ULPITLL_DAT7 = (uint32_t *)(l4_cfg_base+0x100174);
    *USBB2_ULPITLL_DAT7 = (*USBB2_ULPITLL_DAT7) | (0x1 << 30) |(0x1 << 28) |(0x1 << 24) |(0x1 << 19)
    | (0x1 << 14) |(0x1 << 12) |(0x1 << 8) |(0x1 << 3);

    volatile uint32_t * CONTROL_USB2PHYCORE = (uint32_t *)(l4_cfg_base+0x100620);
    *CONTROL_USB2PHYCORE = (0x3 << 26) | (0x3 << 24) | (0x1 << 29);

    /*
     * enabling usb ttl clock
     */
    volatile uint32_t *CM_L3INIT_HSUSBTLL_CLKCTRL = (uint32_t *) (l4_cfg_base +0x9368);
    *CM_L3INIT_HSUSBTLL_CLKCTRL = (0x1<<8) | (0x1<<9) | 0x1;

    // enable full speed usb
    volatile uint32_t *CM_L3INIT_FSUSB_CLKCTRL = (uint32_t *)(l4_cfg_base + 0x93D0);
    *CM_L3INIT_FSUSB_CLKCTRL = 0x2;

    // enable the usb phy clock
    volatile uint32_t * CM_ALWON_USBPHY_CLKCTRL = (uint32_t *)(l4_cfg_base+0x8640);
    *CM_ALWON_USBPHY_CLKCTRL = (0x1 << 8);

    // wakeup dependencies
    //volatile uint32_t * PM_L3INIT_HSUSBHOST_WKDEP = (uint32_t *)(l4_cfg_base+0x30785);
    //*PM_L3INIT_HSUSBHOST_WKDEP = 0x3;

}

static void pandaboard_usb_global_init(void *usb_base)
{
    /*
     * Reset usb ttl
     */
    volatile uint32_t *USBTLL_SYSCONFIG = (uint32_t *)(usb_base+0x10);
    *USBTLL_SYSCONFIG = ((*USBTLL_SYSCONFIG) | 0x2);

    /*
     * wait till reset is done
     */
    volatile uint32_t *USBTLL_SYSSTATUS = (uint32_t *)(usb_base+0x14);
    while(!((*USBTLL_SYSSTATUS) & 0x1));
    /*
     * enable interrupts on ttl
     */
    volatile uint32_t *USBTLL_IRQENABLE = (uint32_t *)(usb_base+0x1C);
    *USBTLL_IRQENABLE = (*USBTLL_IRQENABLE) | 0x3;

    /*
     * host controller reset
     */
    volatile uint32_t *UHH_SYSCONFIG = (uint32_t *)(usb_base+0x2010);
    *UHH_SYSCONFIG = (*UHH_SYSCONFIG) | 0x1;

    /*
     * wait till reset is done
     */
    volatile uint32_t * UHH_SYSSTATUS = (uint32_t *)(usb_base+0x2014);
    while(((*UHH_SYSSTATUS) & 0x6) != 0x6);

    /*
     * Configuration of the ports
     *
     * EHCI
     *
     * The EHCI controller, based on the Enhanced Host Controller Interface
     * (EHCI) Specification for USB Release 1.1, is responsible for HS traffic
     * (480 Mbps), over ULPI/USB 2.0 transceiver macrocell interface (UTMI).
     *
     * OHCI
     *
     * The OHCI controller, based on the Open Host Controller Interface (OHCI
     * Specification for USB Release 1.0a, is responsible for full-speed
     * (FS)/low-speed (LS) traffic (12/1.5 Mbps, respectively), over a serial
     * interface.
     *
     * PORTS
     *
     * Each of the three external ports of the HS USB host controller module is
     * owned by one of the controllers (EHCI or OHCI) at a given time. Each port
     * can work in several modes:
     *  - When the port is owned by the OHCI (FS) host, the 6-pin internal
     *    serial interface to the TLL is used.
     *  - When the port is owned by the EHCI (HS) host, UTMI internal interface
     *    to the TLL, or ULPI to external PHY is used.
     *
     *  - HS-only (with external HS physical layer [PHY]/HS TLL mode HSIC) on
     *    the EHCI
     *  - FS-/LS-only (with external FS PHY/FS TLL mode) on the OHCI
     */

    volatile uint32_t *UHH_DEBUG_CSR = (uint32_t *)(usb_base + 0x2044);
    *UHH_DEBUG_CSR = 0xFF;

#define UUH_HOSTCONFIG_PORT_1_HS 0x0
#define UUH_HOSTCONFIG_PORT_1_FS (0x1 << 16)
#define UUH_HOSTCONFIG_PORT_2_HS 0x0
#define UUH_HOSTCONFIG_PORT_2_FS (0x1 << 18)

    volatile uint32_t *UHH_HOSTCONFIG = (uint32_t *)(usb_base + 0x2040);
    *UHH_HOSTCONFIG = ( *UHH_HOSTCONFIG) | UUH_HOSTCONFIG_PORT_1_FS | UUH_HOSTCONFIG_PORT_2_FS;

#define TTL_CHAN_MODE_SERIAL 0x3
#define TTL_CHAN_MODE_ULPI 0x1
#define TTL_CHAN_MODE_TRANSPARENT 0x5
#define TTL_CHAN_FSLS_PHY   (0x3<<24)
#define TTL_CHAN_FSLS_TTL   (0x7<<24)
#define TTL_CHAN_FSLS_6_PHY   (0x0<<24)
#define TTL_CHAN_FSLS_6_PHY2   (0x1<<24)

    volatile uint32_t *TLL_CHANNEL_CONF_0 = (uint32_t *)(usb_base + 0x40);
    *TLL_CHANNEL_CONF_0 = TTL_CHAN_MODE_SERIAL | TTL_CHAN_FSLS_6_PHY;
    volatile uint32_t *TLL_CHANNEL_CONF_1 = (uint32_t *)(usb_base + 0x44);
    *TLL_CHANNEL_CONF_1 = TTL_CHAN_MODE_SERIAL | TTL_CHAN_FSLS_6_PHY;
    volatile uint32_t *TLL_SHARED_CONF = (uint32_t *)(usb_base + 0x30);
    *TLL_SHARED_CONF = 0x1;

//    volatile uint32_t *CONFIGFLAG = (uint32_t *)(usb_base + 0x2C50 );
//    *CONFIGFLAG = 0x1;

    volatile uint32_t *PORT_1 = (uint32_t *)(usb_base + 0x2C54 );
    printf("PORT1: %u (%x, %u) ", (*PORT_1)&0x1, *PORT_1, ((*PORT_1) >> 13)&0x1);
    *PORT_1 = 0x1<<13;
    volatile uint32_t *PORT_2 = (uint32_t *)(usb_base + 0x2C58);
    printf("PORT2: %u (%x, %u) ", (*PORT_2)&0x1, *PORT_2, ((*PORT_2) >> 13)&0x1);
    *PORT_2 = 0x1<<13;
    volatile uint32_t *PORT_3 = (uint32_t *)(usb_base + 0x2C5C);
    printf("PORT3: %u (%x, %u)  ", (*PORT_3)&0x1, *PORT_3, ((*PORT_3) >> 13)&0x1);
    *PORT_3 = 0x1<<13;

    /*volatile uint8_t *FUNCTION_CTRL_0 = (uint8_t *)(usb_base + 0x804);
     *FUNCTION_CTRL_0 = (0x1<<6) | (0x1 << 2) | 0x1;
     volatile uint8_t *FUNCTION_CTRL_1 = (uint8_t *)(usb_base + 0x904);
     *FUNCTION_CTRL_1 = (0x1<<6) | (0x1 << 2) | 0x3;
     */
    volatile uint32_t *USBINTR = (uint32_t *)(usb_base + 0x2C18);
    *USBINTR = 0xFF;

    printf("UHH_DEBUG_CSR = %x, CCS = %x", *UHH_DEBUG_CSR, ((*UHH_DEBUG_CSR)>>17)&0x3);
    printf("UHH_HOSTCONFIG=%x, CCS=%x \n", *UHH_HOSTCONFIG, ((*UHH_HOSTCONFIG)>>8)&0x3);

    USB_DEBUG("Pandaboard USB global init done.\n");

}
#endif
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

#if __arm__
    pandaboard_enable_usb(ret_addr);
    pandaboard_usb_global_init(usb_subsystem_base);
#endif

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

#if __arm__
    argc = 2;
    argv = (char *[]) {"ohci", "ehci"};

#endif

    /*
     * parse command line args
     */
    if (argc == 0) {
        debug_printf("Usage: usb_manager [host controller type list]\n");
    }

    usb_error_t uerr = USB_ERR_OK;
    for (uint16_t i = 0; i < argc; i++) {
        usb_host_controller_t *hc = NULL;
        if (strcmp(argv[i], "ohci") == 0) {
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_OHCI,
                    usb_subsystem_base + USB_OHCI_OFFSET);
        }
        if (strcmp(argv[i], "uhci") == 0) {
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_UHCI,
                    usb_subsystem_base + USB_UHCI_OFFSET);
        }
        if (strcmp(argv[i], "ehci") == 0) {
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_EHCI,
                    usb_subsystem_base + USB_EHCI_OFFSET);
        }
        if (strcmp(argv[i], "xhci") == 0) {
            hc = malloc(sizeof(*hc));
            uerr = usb_hc_init(hc, USB_XHCI,
                    usb_subsystem_base + USB_XHCI_OFFSET);
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
