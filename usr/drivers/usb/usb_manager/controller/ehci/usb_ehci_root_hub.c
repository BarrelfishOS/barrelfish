/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include "ehci_device.h"

#include <usb/usb.h>
#include <usb/usb_descriptor.h>
#include <usb/usb_error.h>
#include <usb/usb_request.h>
#include <usb/usb_device.h>
#include <usb/class/usb_hub_descriptor.h>
#include <usb/class/usb_hub_request.h>

#include "../../usb_controller.h"
#include "usb_ehci.h"
#include "usb_ehci_root_hub.h"

#define WAIT(x)

static const struct usb_device_descriptor rh_dev_desc = {
    .bLength = sizeof(struct usb_device_descriptor),
    .bDescriptorType = USB_DESCRIPTOR_TYPE_DEVICE,
    .bcdUSB = 0x0200,
    .bDeviceClass = USB_HUB_CLASS_CODE,
    .bDeviceSubClass = USB_HUB_SUBCLASS_CODE,
    .bDeviceProtocol = USB_HUB_PROTOCOL_HSHUBSTT,
    .bMaxPacketSize0 = 64,
    .idVendor = 0,
    .idProduct = 0,
    .bcdDevice = 0x0100,
    .iManufacturer = 1,
    .iProduct = 2,
    .iSerialNumber = 0,
    .bNumConfigurations = 1,
};

static const struct usb_device_qualifier_descriptor rh_qual_desc = {
    .bLength = sizeof(struct usb_device_qualifier_descriptor),
    .bDescriptorType = USB_DESCRIPTOR_TYPE_DEVICE_QUALIFIER,
    .bcdUSB = 0x0200,
    .bDeviceClass = USB_HUB_CLASS_CODE,
    .bDeviceSubClass = USB_HUB_SUBCLASS_CODE,
    .bDeviceProtocol = USB_HUB_PROTOCOL_FSHUB,
    .bMaxPacketSize0 = 0,
    .bNumConfigurations = 0,
    .bReserved = 0,
};

static const struct usb_ehci_config_descriptor rh_cfg_desc = {
    .config = {
        .bLength = sizeof(struct usb_config_descriptor),
        .bDescriptorType = USB_DESCRIPTOR_TYPE_CONFIG,
        .wTotalLength = sizeof(rh_cfg_desc),
        .bNumInterfaces = 1,
        .bConfigurationValue = 1,
        .iConfiguration = 0,
        .bmAttributes = USB_CONFIG_SELF_POWERED,
        .bMaxPower = 0,
    },
    .iface = {
        .bLength = sizeof(struct usb_interface_descriptor),
        .bDescriptorType = USB_DESCRIPTOR_TYPE_INTERFACE,
        .bNumEndpoints = 1,
        .bInterfaceClass = USB_HUB_IFACE_CLASS_CODE,
        .bInterfaceSubClass = USB_HUB_IFACE_SUBCLASS_CODE,
        .bInterfaceProtocol = 0,
    },
    .endpoint = {
        .bLength = sizeof(struct usb_endpoint_descriptor),
        .bDescriptorType = USB_DESCRIPTOR_TYPE_ENDPOINT,
        .bEndpointAddress = {
            USB_ENDPOINT_DIRECTION_IN,
            0,
            1
        },
        .bmAttributes = {
            0,
            0,
            0,
            USB_ENDPOINT_TYPE_INTR
        },
        .wMaxPacketSize = (8 << 8),
        .bInterval = 255,
    },
};

static const struct usb_hub_class_descriptor rh_desc = {
    .bDesLength = 0,
    .bDescriptorType = USB_DESCRIPTOR_TYPE_HUB,
    .bNbrPorts = 0,
    .wHubCharacteristics = {
        0,
        0,
        0,
        0,
        0,
        0
    },
    .bPwrOn2PwrGood = 0,
    .bHubContrCurrent = 0,
    .bDeviceRemovable = {
        0
    }
};

void usb_ehci_roothub_interrupt(usb_ehci_hc_t *hc)
{
    memset(hc->root_hub_interrupt_data, 0, sizeof(hc->root_hub_interrupt_data));

    uint16_t num_ports = hc->root_hub_num_ports + 1;
    if (num_ports > (8 * sizeof(hc->root_hub_interrupt_data))) {
        num_ports = 8 * sizeof(hc->root_hub_interrupt_data);
    }

    for (uint16_t i = 1; i < num_ports; i++) {
        ehci_portsc_t ps = ehci_portsc_rd(hc->ehci_base, i);
        /* clear out the change bits */
        ps = ehci_portsc_occ_insert(ps, 0);
        ps = ehci_portsc_pec_insert(ps, 0);
        ps = ehci_portsc_csc_insert(ps, 0);
        if (ps) {
            hc->root_hub_interrupt_data[i / 8] |= (1 << (i % 8));
        }
    }
    /*
     * TODO: uhub_root_intr(&sc->sc_bus, sc->sc_hub_idata,
     sizeof(sc->sc_hub_idata));
     */
}

usb_error_t usb_ehci_roothub_exec(struct usb_device *device,
        struct usb_device_request *req, const void **ret_data,
        uint16_t *ret_length)
{

    usb_ehci_hc_t *hc = (usb_ehci_hc_t *) device->controller->hc_control;
    const char *str;
    const void *data = (const void *) &hc->root_hub_descriptor;
    uint16_t data_length = 0;

#define C(req, recipent, dir) ((req) | ((recipent)<<8) | ((dir)<<16))
    switch (C(req->bRequest, req->bType.recipient, req->bType.direction)) {
        case C(USB_REQUEST_CLEAR_FEATURE, USB_REQUEST_RECIPIENT_DEVICE, USB_DIRECTION_H2D):
        case C(USB_REQUEST_CLEAR_FEATURE, USB_REQUEST_RECIPIENT_INTERFACE, USB_DIRECTION_H2D):
        case C(USB_REQUEST_CLEAR_FEATURE, USB_REQUEST_RECIPIENT_ENDPOINT, USB_DIRECTION_H2D):
            /* no-op: don't handle write requests */
            break;
        case C(USB_REQUEST_GET_CONFIG, USB_REQUEST_RECIPIENT_DEVICE, USB_DIRECTION_D2H):
            data_length = 1;
            hc->root_hub_descriptor.temp[0] = hc->root_hub_config;
            break;
        case C(USB_REQUEST_GET_DESCRIPTOR, USB_REQUEST_RECIPIENT_DEVICE, USB_DIRECTION_D2H):
            switch (req->wValue >> 8) {
                /*
                 * the only the string type has an id.. the others
                 * result in an IO error if there is an id set.
                 */
                case USB_DESCRIPTOR_TYPE_DEVICE:
                    if ((req->wValue & 0xFF) != 0) {
                        return (USB_ERR_IOERROR);
                    }
                    if (req->bType.type != USB_REQUEST_TYPE_CLASS) {
                        data_length = sizeof(rh_dev_desc);
                        data = (const void *) &rh_dev_desc;
                        break;
                    }
                    /* handling class specific request  */
                    hc->root_hub_descriptor.hub_desc = rh_desc;
                    hc->root_hub_descriptor.hub_desc.bNbrPorts = hc
                            ->root_hub_num_ports;
                    hc->root_hub_descriptor.hub_desc.wHubCharacteristics
                            .port_indicator = ehci_hcsparams_p_indicator_rdf(
                            hc->ehci_base);
                    hc->root_hub_descriptor.hub_desc.wHubCharacteristics
                            .power_mode = ehci_hcsparams_ppc_rdf(hc->ehci_base);
                    hc->root_hub_descriptor.hub_desc.bPwrOn2PwrGood = 200;
                    hc->root_hub_descriptor.hub_desc.bDesLength = 8
                            + ((hc->root_hub_num_ports + 7) / 8);
                    data_length = hc->root_hub_descriptor.hub_desc.bDesLength;
                    break;
                case USB_DESCRIPTOR_TYPE_DEVICE_QUALIFIER:
                    if ((req->wValue & 0xFF) != 0) {
                        return (USB_ERR_IOERROR);
                    }
                    data_length = sizeof(rh_qual_desc);
                    data = (const void *) &rh_qual_desc;
                    break;

                case USB_DESCRIPTOR_TYPE_CONFIG:
                    if ((req->wValue & 0xFF) != 0) {
                        return (USB_ERR_IOERROR);
                    }
                    data_length = sizeof(rh_cfg_desc);
                    data = (const void *) &rh_cfg_desc;
                    break;

                case USB_DESCRIPTOR_TYPE_STRING:
                    switch (req->wValue & 0xFF) {
                        case 0:
                            str = "\001";
                            break;
                        case 1:
                            str = hc->root_hub_vendor;
                            break;
                        case 2:
                            str = "EHCI root hub";
                            break;
                        default:
                            str = "";
                            break;
                    }
                    /*
                     * TODO: MAKE STRING DESCRIPTOR
                     * len = ...
                     * store in hub_desc.tmp
                     */
                    break;
                default:
                    return (USB_ERR_IOERROR);
                    break;
            }
            break;
        case C(USB_REQUEST_GET_INTERFACE, USB_REQUEST_RECIPIENT_INTERFACE, USB_DIRECTION_D2H):
            /* we don't have an alternative interface */
            data_length = 1;
            hc->root_hub_descriptor.temp[0] = 0;
            break;
        case C(USB_REQUEST_GET_STATUS, USB_REQUEST_RECIPIENT_DEVICE, USB_DIRECTION_D2H):
            if (req->bType.type != USB_REQUEST_TYPE_CLASS) {
                data_length = 2;
                hc->root_hub_descriptor.status.wStatus = USB_STATUS_SELF_POWERED;
                break;
            }
            data_length = 16;
            memset(hc->root_hub_descriptor.temp, 0, 16);

            break;
        case C(USB_REQUEST_GET_STATUS, USB_REQUEST_RECIPIENT_INTERFACE, USB_DIRECTION_D2H):
        case C(USB_REQUEST_GET_STATUS, USB_REQUEST_RECIPIENT_ENDPOINT, USB_DIRECTION_D2H):
            data_length = 2;
            hc->root_hub_descriptor.status.wStatus = 0;
            break;
        case C(USB_REQUEST_SET_ADDRESS, USB_REQUEST_RECIPIENT_DEVICE, USB_DIRECTION_H2D):
            if (req->wValue >= USB_EHCI_MAX_DEVICES) {
                return (USB_ERR_IOERROR);
            }
            hc->root_hub_address = req->wValue;
            break;
        case C(USB_REQUEST_SET_CONFIG, USB_REQUEST_RECIPIENT_DEVICE, USB_DIRECTION_H2D):
            if ((req->wValue != 0) && (req->wValue != 1)) {
                return (USB_ERR_IOERROR);
            }
            hc->root_hub_config = req->wValue;
            break;
        case C(USB_REQUEST_SET_DESCRIPTOR, USB_REQUEST_RECIPIENT_DEVICE, USB_DIRECTION_H2D):
            break;
        case C(USB_REQUEST_SET_FEATURE, USB_REQUEST_RECIPIENT_DEVICE, USB_DIRECTION_H2D):
        case C(USB_REQUEST_SET_FEATURE, USB_REQUEST_RECIPIENT_INTERFACE, USB_DIRECTION_H2D):
        case C(USB_REQUEST_SET_FEATURE, USB_REQUEST_RECIPIENT_ENDPOINT, USB_DIRECTION_H2D):
            return (USB_ERR_IOERROR);
            break;
        case C(USB_REQUEST_SET_INTERFACE, USB_REQUEST_RECIPIENT_INTERFACE, USB_DIRECTION_H2D):
            break;
        case C(USB_REQUEST_SYNCH_FRAME, USB_REQUEST_RECIPIENT_ENDPOINT, USB_DIRECTION_H2D):
            break;

            /*
             * handling hub class specific requests
             */

        case C(USB_HUB_REQ_CLEAR_FEATURE, USB_REQUEST_RECIPIENT_OTHER, USB_DIRECTION_H2D):
            if ((req->wIndex < 1) || (req->wLength > hc->root_hub_num_ports)) {
                /* invalid port nuber  */
                return (USB_ERR_IOERROR);
            }
            switch (req->wValue) {
                case USB_HUB_FEATURE_PORT_ENABLE:
                    ehci_portsc_ped_wrf(hc->ehci_base, req->wIndex, 0);
                    break;
                case USB_HUB_FEATURE_PORT_SUSPEND:
                    if (ehci_portsc_sus_rdf(hc->ehci_base, req->wIndex)
                            && (!ehci_portsc_fpr_rdf(hc->ehci_base, req->wIndex))) {
                        ehci_portsc_fpr_wrf(hc->ehci_base, req->wIndex, 1);
                    }
                    WAIT(20);

                    ehci_portsc_sus_wrf(hc->ehci_base, req->wIndex, 0);
                    ehci_portsc_fpr_wrf(hc->ehci_base, req->wIndex, 0);
                    ehci_portsc_ls_wrf(hc->ehci_base, req->wIndex, 0x3);
                    WAIT(4);
                    break;
                case USB_HUB_FEATURE_PORT_POWER:
                    ehci_portsc_pp_wrf(hc->ehci_base, req->wIndex, 0);
                    break;
                case USB_HUB_FEATURE_PORT_TEST:
                    /* clear port test */
                    break;

                case USB_HUB_FEATURE_PORT_INDICATOR:
                    ehci_portsc_pic_wrf(hc->ehci_base, req->wIndex, 0);
                    break;
                case USB_HUB_FEATURE_C_PORT_CONNECTION:
                    ehci_portsc_csc_wrf(hc->ehci_base, req->wIndex, 1);
                    break;

                case USB_HUB_FEATURE_C_PORT_ENABLE:
                    ehci_portsc_pec_wrf(hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_C_PORT_SUSPEND:
                    ehci_portsc_sus_wrf(hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_C_PORT_OVER_CURRENT:
                    ehci_portsc_occ_wrf(hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_C_PORT_RESET:
                    hc->root_hub_reset = 0;
                    break;
                default:
                    return (USB_ERR_IOERROR);
                    break;
            }
            break;
        case C(USB_HUB_REQ_GET_STATUS, USB_REQUEST_RECIPIENT_OTHER, USB_DIRECTION_D2H):
            if ((req->wIndex < 1) || (req->wLength > hc->root_hub_num_ports)) {
                /* invalid port number  */
                return (USB_ERR_IOERROR);
            }
            break;
        case C(USB_HUB_REQ_SET_FEATURE, USB_REQUEST_RECIPIENT_OTHER, USB_DIRECTION_H2D):
            if ((req->wIndex < 1) || (req->wLength > hc->root_hub_num_ports)) {
                /* invalid port number  */
                return (USB_ERR_IOERROR);
            }
            switch (req->wValue) {
                case USB_HUB_FEATURE_PORT_ENABLE:
                    ehci_portsc_ped_wrf(hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_PORT_SUSPEND:
                    ehci_portsc_sus_wrf(hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_PORT_RESET:
                    if (ehci_portsc_ls_rdf(hc->ehci_base, req->wIndex) == 0x1) {
                        /* low speed device */
                        usb_ehci_roothub_port_disown(hc, req->wIndex, 1);
                        break;
                    }
                    /* initiate reset sequence */
                    ehci_portsc_pr_wrf(hc->ehci_base, req->wIndex, 1);
                    WAIT(200);

                    /* clear the reset */
                    ehci_portsc_pr_wrf(hc->ehci_base, req->wIndex, 0);
                    WAIT(200);

                    if (ehci_portsc_pr_rdf(hc->ehci_base, req->wIndex)) {
                        debug_printf("timeout while resetting port\n");
                        return (USB_ERR_TIMEOUT);
                    }

                    if (!ehci_portsc_ped_rdf(hc->ehci_base, req->wIndex)) {
                        if (hc->flags.tt_present) {
                            usb_ehci_roothub_port_disown(hc, req->wIndex, 0);
                        }
                    }
                    hc->root_hub_reset = 1;

                    break;
                case USB_HUB_FEATURE_PORT_POWER:
                    ehci_portsc_pp_wrf(hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_PORT_TEST:
                    break;
                case USB_HUB_FEATURE_PORT_INDICATOR:
                    ehci_portsc_pic_wrf(hc->ehci_base, req->wIndex, 1);
                    break;
                default:
                    return (USB_ERR_IOERROR);
            }
            break;
        case C(USB_HUB_REQ_CLEAR_TT_BUFFER, USB_REQUEST_RECIPIENT_OTHER, USB_DIRECTION_H2D):
        case C(USB_HUB_REQ_RESET_TT, USB_REQUEST_RECIPIENT_OTHER, USB_DIRECTION_H2D):
        case C(USB_HUB_REQ_STOP_TT, USB_REQUEST_RECIPIENT_OTHER, USB_DIRECTION_H2D):
            break;
        default:
            return (USB_ERR_IOERROR);
    }
    *ret_length = data_length;
    *ret_data = data;

    return (USB_ERR_OK);
}

void usb_ehci_roothub_port_disown(usb_ehci_hc_t *hc, uint16_t portno,
        uint8_t lowspeed)
{
    if (portno > ehci_hcsparams_n_ports_rdf(hc->ehci_base)) {
        debug_printf("ERROR: port does not exist! \n");
        return;
    }

    ehci_portsc_po_wrf(hc->ehci_base, portno, 1);

}

