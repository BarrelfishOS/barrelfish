/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_OHCI_H_
#define _USB_OHCI_H_

#include "usb_ohci_descriptors.h"
#include <dev/ohci_dev.h>

// the OHCI controller supports maximum 127 devices
#define	USB_OHCI_MAX_DEVICES 127

/* ENDPOINT TYPE CODES */
#define USB_OHCI_EP_TYPE_ISOC 0
#define USB_OHCI_EP_TYPE_INTR 1
#define USB_OHCI_EP_TYPE_CTRL 2
#define USB_OHCI_EP_TYPE_BULK 3

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Frame Mappings
 * ------------------------------------------------------------------------
 * This data structure contains information about the used physical frames
 * and their corresponding physical and virtual addresses as well as the
 * respective capabilities.
 *
 * Each transfer type has to have at least one associated entry since each
 * has its own list of endpoint- and transfer descriptors, which are
 * managed independelty.
 *
 * Fields:
 *
 */
struct usb_ohci_memory_pages {
    struct usb_page_cache hcca_pc;
    struct usb_page_cache ctrl_start_pc;
    struct usb_page_cache bulk_start_pc;
    struct usb_page_cache isoc_start_pc;
    struct usb_page_cache intr_start_pc[USB_OHCI_NO_EP_DESCRIPTORS];

    struct usb_page hcca_pg;
    struct usb_page ctrl_start_pg;
    struct usb_page bulk_start_pg;
    struct usb_page isoc_start_pg;
    struct usb_page intr_start_pg[USB_OHCI_NO_EP_DESCRIPTORS];
};

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Configuration Descriptor
 * ------------------------------------------------------------------------
 * When there is a GetConfiguration request executed then the reply contains
 * the configuration descriptor, interface descriptor and endpoint
 * descriptor.
 *
 * Fields:
 *  - confg_desc:  configuration descriptor
 *  - iface_desc:  interface descriptor
 *  - ep_desc:     endpoint descriptor
 */
struct usb_ohci_config_desc {
    struct usb_config_descriptor config_desc;
    struct usb_interface_descriptor iface_desc;
    struct usb_endpoint_descriptor ep_desc;
} __packed;

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Configuration Descriptor
 * ------------------------------------------------------------------------
 * When there is a GetConfiguration request executed then the reply contains
 * the configuration descriptor, interface descriptor and endpoint
 * descriptor.
 *
 * TODO: REVISE
 *
 * Fields:
 *  - confg_desc:  configuration descriptor
 *  - iface_desc:  interface descriptor
 *  - ep_desc:     endpoint descriptor
 */
typedef struct usb_ohci_hc {
    ohci_t ohci_base;
    union ohci_hub_desc root_hub_desc;
    uint8_t root_hub_num_ports;
    uint8_t root_hub_intr_data[32];
    uint8_t root_hub_address;
    uint8_t root_hub_config;
    uint32_t enabled_intrs; /* enabled interrupts */

    struct usb_device *devices[USB_OHCI_MAX_DEVICES];

    struct usb_ohci_hcca *hcca;
    struct usb_ohci_ed *qh_ctrl_last;
    struct usb_ohci_ed *qh_bulk_last;
    struct usb_ohci_ed *qh_isoc_last;
    struct usb_ohci_ed *qh_intr_last[USB_OHCI_NO_EP_DESCRIPTORS];

    struct usb_ohci_ed *qh_ed_free;
    struct usb_ohci_td *qh_td_free;
    struct usb_ohci_itd *qh_itd_free;

    uint16_t intr_stats[USB_OHCI_NO_EP_DESCRIPTORS];  //keeps track of the interrupt transfes

    usb_host_controller_t *controller;



    uint16_t id_vendor;

    char vendor[16];



} usb_ohci_hc_t;

/*
 * ------------------------------------------------------------------------
 * OHCI Host Controller Function Prototypes
 * ------------------------------------------------------------------------
 */
usb_error_t usb_ohci_init(usb_ohci_hc_t *sc);
void usb_ohci_detach(struct ohci_host_controller *sc);
void usb_ohci_interrupt(usb_ohci_hc_t *sc);

#endif /* _USB_OHCI_H_ */
