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
#include <barrelfish/barrelfish.h>

#include "ehci_device.h"

#include <usb/usb.h>
#include <usb/usb_descriptor.h>
#include <usb/class/usb_hub_descriptor.h>
#include <usb/usb_error.h>
#include <usb/usb_device.h>
#include <usb/usb_xfer.h>


#include "../../usb_controller.h"


#include "../../usb_memory.h"

#include "usb_ehci.h"
#include "usb_ehci_root_hub.h"
#include "usb_ehci_bus.h"
#include "usb_ehci_xfer.h"
#include "usb_ehci_pipe.h"
#include "usb_ehci_queue.h"

static void usb_ehci_do_poll(usb_host_controller_t *hostc)
{
    usb_ehci_hc_t *hc = (usb_ehci_hc_t *)hostc->hc_control;

    usb_ehci_poll(hc);
}

static struct usb_hcdi_bus_fn usb_ehci_bus_fn =
{
    .endpoint_init = usb_ehci_endpoint_init,
    .xfer_setup = usb_ehci_xfer_setup,
    .xfer_unsetup = usb_ehci_xfer_unsetup,
    .get_dma_delay = usb_ehci_get_dma_delay,
    .device_resume = usb_ehci_device_resume,
    .device_suspend = usb_ehci_device_suspend,
    .set_hw_power = usb_ehci_set_power,
    .set_hw_power_sleep = usb_ehci_sleep,
    .roothub_exec = usb_ehci_roothub_exec,
    .xfer_poll = usb_ehci_do_poll,
};


void usb_ehci_poll(usb_ehci_hc_t *hc) {

    struct usb_xfer *xfer;
    uint8_t repeat = 0;

    do {
        for (xfer = (&hc->controller->intr_queue.head)->first; xfer;
                xfer = ((xfer))->wait_entry.next) {

            if (usb_ehci_xfer_is_finished(xfer)) {
                repeat = 1;
            }
        }
    } while(repeat);

}


void usb_ehci_sleep(struct usb_host_controller *hc, uint32_t state)
{
    assert(!"NYI: sleep / resume the host controller");
}


void usb_ehci_set_power(struct usb_host_controller *hc)
{
    assert(!"NYI: set power");
}

/**
 * \brief resumes a suspended USB device
 */
void usb_ehci_device_resume(struct usb_device *device)
{
    assert(!"NYI: resuming suspended device");
}

/**
 * \brief suspends a attached USB device
 */
void usb_ehci_device_suspend(struct usb_device *device)
{
    assert(!"NYI: device suspend");
}


struct usb_hcdi_bus_fn *usb_ehci_get_bus_fn(void)
{
    return (&usb_ehci_bus_fn);
}

void usb_ehci_endpoint_init(struct usb_device *device,
        struct usb_endpoint_descriptor *ep_desc, struct usb_endpoint *ep)
{
    usb_ehci_hc_t *hc = (usb_ehci_hc_t *)device->controller->hc_control;

    /* check if the operation is supported */
    if (device->flags.usb_mode != USB_MODE_HOST) {
        return;
    }

    if (device->device_index == hc->root_hub_address) {
        /* creating endpoints is only supported for non root hub devices */
        return;
    }

    if (device->speed != USB_SPEED_HIGH) {
        if ((device->hs_hub_address == 0) ||
                (device->hs_hub_port_number == 0) ||
                (device->parent_hs_hub == NULL) ||
                (device->parent_hs_hub->hub == NULL)){
            /*
             * full or low speed devices need a transaction translator
             * to be set. so we cannot create an endpoint.
             */
            return;
        }
    }

    switch(ep_desc->bmAttributes.xfer_type) {
        case USB_ENDPOINT_XFER_CONTROL:
            ep->pipe_fn = usb_ehci_get_ctrl_pipe_fn();
            break;
        case USB_ENDPOINT_XFER_INTR:
            ep->pipe_fn = usb_ehci_get_intr_pipe_fn();
                    break;
        case USB_ENDPOINT_XFER_ISOCHR:
            if (device->speed == USB_SPEED_HIGH) {
                ep->pipe_fn = usb_ehci_get_hs_isoc_pipe_fn();
            } else if (device->speed == USB_SPEED_FULL) {
                ep->pipe_fn = usb_ehci_get_fs_isoc_pipe_fn();
            }
                    break;
        case USB_ENDPOINT_XFER_BULK:
            ep->pipe_fn = usb_ehci_get_bulk_pipe_fn();
                    break;
        default:
                    /* no-op */
                    break;
    }
}


void usb_ehci_get_dma_delay(struct usb_device *device, uint32_t *ret_delay)
{
    *ret_delay = (188);
}
