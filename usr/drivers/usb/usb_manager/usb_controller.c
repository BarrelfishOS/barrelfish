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

#include <usb/usb.h>
#include <usb/usb_error.h>

#include <usb_controller.h>
#include <usb_device.h>

#include "controller/ohci/usb_ohci.h"
#include "controller/uhci/usb_uhci.h"
#include "controller/ehci/usb_ehci.h"
#include "controller/xhci/usb_xhci.h"

static usb_host_controller_t *host_controllers = NULL;

usb_error_t usb_hc_init(usb_host_controller_t *hc, usb_hc_version_t version,
        uintptr_t controller_base)
{
    if (hc == NULL) {
        return (USB_ERR_NOMEM);
    }

    (((&hc->done_queue.head))->first) = NULL;
    (&hc->done_queue.head)->last_next = &(((&hc->done_queue.head))->first);

    (((&hc->intr_queue.head))->first) = NULL;
        (&hc->intr_queue.head)->last_next = &(((&hc->intr_queue.head))->first);

    usb_error_t err = USB_ERR_INVAL;
    void *controller;

    switch (version) {
        case USB_UHCI:
            debug_printf("Failure: UHCI not yet implemented\n");
            break;
        case USB_OHCI:
            controller = malloc(sizeof(usb_ohci_hc_t));
            if (controller == NULL) {
                return (USB_ERR_NOMEM);
            }
            memset(controller, 0, sizeof(usb_ohci_hc_t));
            hc->hc_type = USB_OHCI;
            hc->hc_control = controller;
            ((usb_ohci_hc_t*) controller)->controller = hc;
            err = usb_ohci_init((usb_ohci_hc_t*) controller,
                    (uintptr_t) controller_base);
            break;
        case USB_EHCI:
            controller = malloc(sizeof(usb_ehci_hc_t));
            if (controller == NULL) {
                return (USB_ERR_NOMEM);
            }
            memset(controller, 0, sizeof(usb_ehci_hc_t));
            hc->hc_type = USB_EHCI;
            hc->hc_control = controller;
            hc->handle_intr = usb_ehci_interrupt;
            ((usb_ehci_hc_t*) controller)->controller = hc;
            err = usb_ehci_init((usb_ehci_hc_t*) controller, controller_base);
            break;
        case USB_XHCI:
            debug_printf("Failure: XHCI not yet implemented\n");
            break;
        default:
            return (USB_ERR_INVAL);
            break;

    }

    if (err == USB_ERR_OK) {
        if (host_controllers == NULL) {
            hc->next = NULL;
            hc->prev_next = NULL;
            host_controllers = hc;
        } else {
            hc->next = host_controllers;
            host_controllers->prev_next = &(hc->next);
            host_controllers = hc;
        }

    }

    return (USB_ERR_OK);
}

void usb_hc_intr_handler(void *arg)
{
    usb_host_controller_t *hc = host_controllers;

    while (hc != NULL) {
        if (hc->handle_intr != NULL) {
            (hc->handle_intr)(hc);
        }
        hc = hc->next;
    }

}

/**
 * \brief this function starts the exploration of the devices on the
 *        USB bus starting from the root hub.
 *
 * \param hc
 */
void usb_hc_explore_device_tree(usb_host_controller_t *hc)
{
    if (hc == NULL) {
        return;
    }

    /*TODO
     * if (hc->root_hub && hc->root_hub->hub) {
     usb_hub_explore(hc->root_hub);
     }*/
}
