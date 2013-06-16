/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include <usb/usb.h>
#include <usb/usb_parse.h>

#include <usb_controller.h>
#include <usb_device.h>
#include <usb_request.h>
#include <usb_pipe.h>
#include <usb_interface.h>
#include <usb_hub.h>





static void usb_device_cfg_free(struct usb_device *dev, uint8_t iface)
{
    USB_DEBUG_TR_ENTER;
    if (dev->ifaces != NULL) {
        free(dev->ifaces);
    }

    if (dev->endpoints != NULL) {
        free(dev->endpoints);
    }
    dev->ep_clear_stall = NULL;
    dev->ifaces = NULL;
    dev->iface_max = 0;
    dev->endpoints = NULL;
    dev->ep_max = 0;
}

static void usb_device_init_endpoint(struct usb_device *device,
        uint8_t iface_index, struct usb_endpoint_descriptor *desc,
        struct usb_endpoint *ep)
{
    USB_DEBUG_TR_ENTER;
    struct usb_hcdi_bus_fn *bus_fn = device->controller->hcdi_bus_fn;

    if (bus_fn && bus_fn->endpoint_init) {
        (bus_fn->endpoint_init)(device, desc, ep);
    }

    ep->descriptor = desc;
    ep->iface_index = iface_index;
    ep->endpoint_address = desc->bEndpointAddress.ep_number;
    ep->max_packet_size = desc->wMaxPacketSize;
    (&ep->transfers.head)->first = NULL;
    (&ep->transfers.head)->last_next = &((&ep->transfers.head)->first);

    ep->transfers.command = &usb_pipe_start;

    if (ep->pipe_fn == NULL) {
        return;
    }

    if (bus_fn->clear_stall != NULL) {
        (bus_fn->clear_stall)(device, ep);
    }
}

static usb_error_t usb_device_cfg_process(struct usb_device *dev, uint8_t iface,
        uint8_t init)
{
    USB_DEBUG_TR_ENTER;
    struct usb_iface_parse_state iface_ps;
    struct usb_endpoint *ep;
    uint8_t ep_max, ep_current;
    uint8_t alt_index = 0;
    usb_error_t err = USB_ERR_OK;

    if (iface != USB_INTERFACE_INDEX_ANY) {
        alt_index = init;
        init = 2;
    }

    if (init) {
        ep = dev->endpoints;
        ep_max = dev->ep_max;

        while (ep_max--) {
            if ((iface == USB_INTERFACE_INDEX_ANY)
                    || (iface == ep->iface_index)) {
                if (ep->ref_allocation != 0) {
                    err = USB_ERR_IN_USE;
                } else {
                    /* perform EP reset */
                    memset(ep, 0, sizeof(*ep));
                    ep->iface_index = USB_INTERFACE_INDEX_ANY;
                }
            }
            ep++;
        }
        if (err != USB_ERR_OK) {
            return (err);
        }
    }
    memset(&iface_ps, 0, sizeof(iface_ps));

    ep_current = 0;
    ep_max = 0;
    uint8_t ep_tmp = 0;

    struct usb_interface_descriptor *idesc;
    struct usb_interface *interface;
    struct usb_endpoint_descriptor *edesc;
    uint8_t iface_index = 0;

    while ((idesc = usb_parse_next_iface(dev->config_desc, &iface_ps))) {

        if (iface_ps.iface_index == 32) {
            /* the maximium ifaces */
            break;
        }

        interface = dev->ifaces + iface_ps.iface_index;

        uint8_t do_init = 0;

        if (init) {
            if ((iface_index != USB_INTERFACE_INDEX_ANY)
                    && (iface_index != iface_ps.iface_index)) {
                do_init = 0;
            } else if (alt_index != iface_ps.iface_index_alt) {
                do_init = 0;
            } else {
                do_init = 1;
            }
        }

        if (iface_ps.iface_index_alt == 0) {
            ep_current = ep_max;
        }

        if (do_init) {
            assert(interface != NULL);
            interface->descriptor = idesc;

            interface->parent_iface_index = USB_INTERFACE_INDEX_ANY;

            interface->alt_setting = alt_index;
        }

        edesc = (struct usb_endpoint_descriptor *) idesc;

        ep_tmp = ep_current;

        while ((edesc = usb_parse_next_edesc(dev->config_desc, edesc))) {

            if (ep_tmp == 32) {
                /* maximum endpoints */
                break;
            }
            ep = dev->endpoints + ep_tmp;
            if (do_init) {
                usb_device_init_endpoint(dev, iface_ps.iface_index, edesc, ep);
            }
            ep_tmp++;

            if (ep_max < ep_tmp) {
                ep_max = ep_tmp;
            }

            idesc = (struct usb_interface_descriptor *) edesc;
        }

    }

    if (!init) {
        dev->iface_max = iface_ps.iface_index;
        dev->ifaces = NULL;
        if (dev->iface_max != 0) {
            dev->ifaces = malloc(sizeof(*interface) * dev->iface_max);
            if (dev->ifaces == NULL) {
                usb_device_cfg_free(dev, USB_INTERFACE_INDEX_ANY);
                return (USB_ERR_NOMEM);
            }
        }

        if (ep_max != 0) {
            dev->endpoints = malloc(sizeof(*ep) * ep_max);
            if (dev->endpoints == NULL) {
                usb_device_cfg_free(dev, USB_INTERFACE_INDEX_ANY);
                return (USB_ERR_NOMEM);
            }
            dev->ep_max = ep_max;
            dev->ep_clear_stall = NULL;
        }
    }

    return (USB_ERR_OK);
}

static usb_error_t usb_device_setup_descriptor(struct usb_device *dev)
{
    USB_DEBUG_TR_ENTER;
    usb_error_t err;
    switch (dev->speed) {
        case USB_SPEED_LOW:
        case USB_SPEED_FULL:
            err = usb_req_get_descriptor(dev, NULL, &dev->device_desc, 8, 8, 0,
                    USB_DESCRIPTOR_TYPE_DEVICE, 0, 0);
            if (err != USB_ERR_OK) {
                USB_DEBUG("ERROR: Failed to get device descriptor\n");
                USB_DEBUG_TR_RETURN;
                return (err);
            }
            break;
        default:
            break;
    }
    err = usb_req_get_device_descriptor(dev, &dev->device_desc);

    if (err != USB_ERR_OK) {
        USB_DEBUG("NOTICE: getting descriptor failed. retry.\n");
        err = usb_req_get_device_descriptor(dev, &dev->device_desc);
    }

    return (err);
}

usb_error_t usb_device_set_configuration(struct usb_device *dev,
        uint8_t config)
{
    USB_DEBUG_TR_ENTER;

    usb_device_cfg_free(dev, USB_INTERFACE_INDEX_ANY);

    /* free old config descriptor */
    if (dev->config_desc) {
        free(dev->config_desc);
        dev->config_desc = NULL;
    }

    if (config == 0xFF) {
        dev->config_index = 0xFF;
        dev->config_number = 0;
        if (dev->state == USB_DEVICE_STATE_CONFIGURED) {
            dev->state = USB_DEVICE_STATE_ADDRESSED;
        }
        return (USB_ERR_OK);
    }

    usb_error_t err;
    struct usb_config_descriptor *cdesc;

    err = usb_req_get_config_descriptor(dev, &cdesc, config);

    if (err) {
        USB_DEBUG("ERROR: getting configuration failed.\n");
        USB_DEBUG_TR_RETURN;
        return (err);
    }

    assert(cdesc != NULL);

    dev->config_desc = cdesc;

    /* TODO: Check for power */
    if (dev->flags.usb_mode == USB_MODE_HOST) {
        dev->flags.self_powered = 1;
    }

    dev->config_index = config;
    dev->config_number = cdesc->bConfigurationValue;
    dev->state = USB_DEVICE_STATE_CONFIGURED;

    err = usb_req_set_config(dev, cdesc->bConfigurationValue);

    if (err != USB_ERR_OK) {
        USB_DEBUG("ERROR: usb_req_set_config failed.\n");
        USB_DEBUG_TR_RETURN;
        return (err);
    }

    err = usb_device_cfg_process(dev, USB_INTERFACE_INDEX_ANY, 0);
    if (err) {
        usb_device_cfg_free(dev, USB_INTERFACE_INDEX_ANY);
        USB_DEBUG("ERROR: processing not init, %s\n", __func__);
        return (err);
    }

    err = usb_device_cfg_process(dev, USB_INTERFACE_INDEX_ANY, 1);
    if (err) {
        usb_device_cfg_free(dev, USB_INTERFACE_INDEX_ANY);
        USB_DEBUG("%s, error while processing + init\n", __func__);
        return (err);
    }

    USB_DEBUG_TR_RETURN;

    return (USB_ERR_OK);
}

static void usb_device_setup_strings(struct usb_device *dev)
{
    debug_printf("TODO: GET STRING DESCRIPTORS!\n");
    /// TODO...
}

struct usb_device *usb_device_alloc(struct usb_host_controller *hc,
        struct usb_device *parent_hub, uint8_t depth, uint8_t portindex,
        uint8_t portno, usb_speed_t speed, usb_mode_t mode)
{
    USB_DEBUG_TR_ENTER;
    usb_error_t err;

    /* find a new and empty device index */
    uint8_t device_index = 1; /* root hub address */
    for (device_index = 1;
            (device_index < hc->devices_max)
                    && (hc->devices[device_index] != NULL); device_index++) {
        /* noop */
    }

    if (device_index == hc->devices_max) {
        /* no free space on the bus */
        USB_DEBUG("WARNING: No free device index!\n");
        return (NULL);
    }

    if (depth > 0x10) {
        USB_DEBUG("ERROR: Invalid device depth.\n");
        return (NULL);
    }

    struct usb_device *dev = malloc(sizeof(struct usb_device));
    if (dev == NULL) {
        USB_DEBUG("ERROR: no free mem.\n");
        return (NULL);
    }

    memset(dev, 0, sizeof(struct usb_device));

    dev->xfer_id = 0;



    /* initialize the device */
    dev->parent_hub = parent_hub;
    dev->hub_port_index = portindex;
    dev->hub_port_number = portno;

    dev->depth = depth;
    dev->controller = hc;
    dev->device_address = 0; /* the default start address */

    /* setup default endpont descriptor */
    dev->ctrl_ep_desc.bLength = sizeof(dev->ctrl_ep_desc);
    dev->ctrl_ep_desc.bDescriptorType = USB_DESCRIPTOR_TYPE_ENDPOINT;
    dev->ctrl_ep_desc.wMaxPacketSize = 8;

    /* set the device max packet size */
    dev->device_desc.bMaxPacketSize0 = 8;

    dev->speed = speed;
    dev->flags.usb_mode = mode;

    /* search for high speed USB hub  if any */
    struct usb_device *hub = dev->parent_hub;
    struct usb_device *adev = dev;

    dev->controller = hc;

    while (hub) {
        if (hub->speed == USB_SPEED_HIGH) {
            dev->hs_hub_address = hub->device_address;
            dev->parent_hs_hub = hub;
            dev->hs_hub_port_number = adev->hub_port_number;
            break;
        }
        adev = hub;
        hub = hub->parent_hub;
    }

    usb_device_init_endpoint(dev, 0, &dev->ctrl_ep_desc, &dev->ctrl_ep);

    dev->device_index = device_index;

    dev->state = USB_DEVICE_STATE_POWERED;

    if (dev->flags.usb_mode == USB_MODE_HOST) {
        err = usb_req_set_address(dev, device_index);

        if (err) {
            debug_printf("set address failed (ignored)\n");
            return (NULL);
        }
        if (dev->device_address == 0) {
            USB_DEBUG_DEV("setting device address to %i\n", device_index);
            dev->device_address = device_index;
        }
    } else {
        assert(!"NYI: device mode.\n");
        dev->flags.self_powered = 0;

        /* unconfigured state */
        dev->config_index = 0;
        dev->config_number = 0;
    }

    USB_WAIT(USB_DELAY_SET_ADDRESS*5);

    dev->state = USB_DEVICE_STATE_ADDRESSED;

    err = usb_device_setup_descriptor(dev);

    assert(dev->device_desc.bDescriptorType == USB_DESCRIPTOR_TYPE_DEVICE);

    USB_DEBUG_DEV(
            "bcdUSB=%x, class = %x, subclass = %x\n", dev->device_desc.bcdUSB, dev->device_desc.bDeviceClass, dev->device_desc.bDeviceSubClass);

    USB_DEBUG_DEV(
            "bMaxPacketSize0 = %u, bNumConfigurations=%u\n", dev->device_desc.bMaxPacketSize0, dev->device_desc.bNumConfigurations);

    USB_DEBUG_DEV(
            "idProduct = %x, idVendor = %x, iSerial = %x\n", dev->device_desc.idProduct, dev->device_desc.idVendor, dev->device_desc.iSerialNumber);

    char buf[255];
    memset(buf, 0, sizeof(buf));

    if (dev->device_desc.iManufacturer || dev->device_desc.iProduct
            || dev->device_desc.iSerialNumber) {
        err = usb_req_get_string_desc(dev, buf, 4, 0, 0);
    } else {
        USB_DEBUG("no string descriptors...\n");
        err = USB_ERR_INVAL;
    }

    if (err != USB_ERR_OK || buf[0] < 4) {
        dev->flags.no_strings = 1;
    } else {
        uint16_t langid = 0;

        buf[0] &= ~1;
        uint32_t i;
        for (i = 2; i < buf[0]; i += 2) {
            langid = (uint16_t) *(buf + i);
            if ((langid & 0x00FF) == 0x0009) {
                break;
            }
        }
        if (i >= buf[0]) {
            langid = (uint16_t) *(buf + 2);
        }
        dev->language_id = langid;
    }

    dev->power_needed = USB_POWER_MIN;

    usb_device_setup_strings(dev);

    err = usb_device_set_configuration(dev, 0);

    if (err != USB_ERR_OK) {
        if (dev->device_desc.bNumConfigurations != 0) {
            debug_printf("WARNING: Set configuration failed.\n");

            USB_DEBUG("WARNING: getting descriptor failed. "
            "Try to re-enumerate\n");
            /* TODO: err = usb_req_re_enumerate(dev, NULL); */
            assert(!"NYI: re-enumeration\n");
            err = usb_device_set_configuration(dev, 0);
        }
    }

    if (err != USB_ERR_OK) {
        usb_device_free(dev, 0);
        return (NULL);
    }

    if (parent_hub) {
        (parent_hub->hub->ports + portindex)->device_index = device_index;
    }

    if (device_index != 0) {
        hc->devices[device_index] = dev;
    }


    USB_DEBUG_TR_RETURN;

    return (dev);
}

void usb_device_free(struct usb_device * device, uint8_t flag)
{
    debug_printf("TODO: freeing up the device and it's transfers..\n");

    /*
     * 1) check if it is a hub device... and free up recursively
     *
     * 2) notiry the usb device driver
     *
     * 3) free up the transfers
     *
     * 4) free up the device
     */

    device->controller->devices[device->device_index] = NULL;
}
