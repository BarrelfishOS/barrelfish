/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <barrelfish/barrelfish.h>

#include <usb/usb.h>
#include <usb/usb_descriptor.h>
#include <usb/usb_xfer.h>
#include <usb/usb_device.h>

#include <usb_xfer.h>

#include <usb_endpoint.h>

struct usb_endpoint *usb_endpoint_lookup(struct usb_device *dev, uint8_t iface,
        const struct usb_xfer_config *filter)
{
    uint8_t ep_index = filter->ep_index;
    struct usb_endpoint *ep = dev->endpoints;
    usb_endpoint_address_t *epaddr;
    usb_endpoint_attributes_t *epattr;

    uint8_t any = 1;

    /* loop over all endpoints */
    for (uint8_t ep_current = 0; ep_current < dev->ep_max; ep_current++) {
        if (ep == NULL) {
            /* no endpoint allocated */
            continue;
        }

        if ((ep->descriptor == NULL) || (ep->iface_index != iface)) {
            /* there is no descriptor or the wrong interface */
            continue;
        }

        epaddr = &(ep->descriptor->bEndpointAddress);
        epattr = &(ep->descriptor->bmAttributes);
        /* check for direction */
        if (filter->direction != USB_ENDPOINT_DIRECTION_ANY) {
            any = 0;
            if (epaddr->direction != filter->direction) {
                /* wrong direction, check next next */
                continue;
            }
        }

        if (filter->endpoint != USB_ENDPOINT_ADDRESS_ANY) {
            any = 0;
            if (epaddr->ep_number != filter->endpoint) {
                /* wrong endpoint address */
                continue;
            }
        }

        if (filter->type != USB_ENDPOINT_TYPE_ANY) {
            any = 0;
            if (epattr->xfer_type != filter->type) {
                /* wrong xfer type */
                continue;
            }
        }

        if (!ep_index-- && !any) {
            USB_DEBUG_DEV(
                    "usb_endpoint_lookup(): found iface=0x%x, ep=0x%x\n", iface, ep->endpoint_address);
            return (ep);
        }

        ep++;
    }

    if (dev->ctrl_ep.descriptor && any && !ep_index) {
        USB_DEBUG_DEV("usb_endpoint_lookup(): found default ctrl ep\n");
        return (&dev->ctrl_ep);
    }

    return (NULL);
}
