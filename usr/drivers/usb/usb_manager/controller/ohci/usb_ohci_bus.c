/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "usb_ohci_bus.h"
#include "usb_ohci.h"
#include "usb_ohci_descriptors.h"
#include <usb/usb_descriptor.h>


#include "dev/ohci_dev.h"

/*
 * ------------------------------------------------------------------------
 * OHCI Bus Functions
 * ------------------------------------------------------------------------
 */

/*
 * \brief   this function polls the interrupt queue and checks for each element
 *          if the transfer has been completed. If so the transfer is
 *          removed from the list
 *
 * \param   hc  pointer to the host controller
 */
static void usb_ohci_do_poll(struct usb_host_controller *hc)
{
    struct usb_xfer *xfer;

    uint8_t repeat = 1;

    while(repeat) {
        repeat = 0;

        for ((xfer) = (((&hc->intr_queue.head))->first); (xfer); (xfer) =
                (((xfer))->wait_entry.next)) {
            /*
             * check if transfer is transferred
             */
            if (usb_ohci_xfer_is_finished(xfer)) {
                /* queue has been modified */
                repeat = 1;
            }
        }
    }
}

/*
 * \brief   this function initializes the endpoint with the correct
 *          pipe functions
 *
 * \param   device  the device the endpoint belongs to
 * \param   ep_desc the description of the endpoint
 * \param   ep      the endpoint
 */
static void usb_ohci_ep_init(struct usb_device *device,
        struct usb_endpoint_descriptor *ep_desc, struct usb_endpoint *ep)
{
    if (device->flags.usb_mode != USB_CONTROLLER_MODE_HOST) {
        /* this usb device mode is not supported */
        return;
    }

    usb_ohci_hc_t *hc = (usb_ohci_hc_t *)device->controller->hc_control;

    /*
     * we can only initialize endpoints for function devices
     */
    if (device->device_index != hc->address) {
        switch(ep_desc->bmAttributes.xfer_type) {
            case USB_ENDPOINT_XFER_CONTROL:
                ep->pipe_fn = usb_ohci_get_ctrl_pipe_fn();
                break;
            case USB_ENDPOINT_XFER_INTR :
                ep->pipe_fn = usb_ohci_get_intr_pipe_fn();
                break;
            case USB_ENDPOINT_XFER_ISOCHR :
                if (device->speed == USB_DEVICE_SPEED_FULL) {
                    ep->pipe_fn = usb_ohci_get_isoc_pipe_fn();
                }
                break;
            case USB_ENDPOINT_XFER_BULK :
                ep->pipe_fn = usb_ohci_get_bulk_pipe_fn();
                break;
            default:
                /* transfer type unkown, do nothing */
                break;
        }
    }

}

static struct usb_hcdi_bus_fn ohci_bus_methods = {    .endpoint_init = usb_ohci_ep_init,
        .xfer_setup = ohci_xfer_setup,
        .xfer_unsetup = ohci_xfer_unsetup,
        .get_dma_delay = ohci_get_dma_delay,
        .device_resume = ohci_device_resume,
        .device_suspend = ohci_device_suspend,
        .set_hw_power = ohci_set_hw_power,
        .set_hw_power_sleep = ohci_set_hw_power_sleep,
        .roothub_exec = ohci_roothub_exec,
        .xfer_poll = usb_ohci_do_poll,
};

struct usb_hcdi_bus_fn *usb_ohci_get_bus_fn()
{
return &ohci_bus_methods;

}

