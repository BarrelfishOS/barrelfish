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

#include <usb/usb.h>
#include <usb/usb_error.h>

#include "usb_controller.h"

#include "controller/ohci/usb_ohci.h"
#include "controller/uhci/usb_uhci.h"
#include "controller/ehci/usb_ehci.h"
#include "controller/xhci/usb_xhci.h"

usb_error_t usb_hc_init(usb_host_controller_t *hc, usb_hc_version_t version,
            void *controller_base)
{
    if (hc == NULL) {
        return USB_ERR_NOMEM;
    }
    void *controller;
    switch(version) {
        case USB_UHCI:
            debug_printf("Failure: UHCI not yet implemented\n");
            break;
        case USB_OHCI:
            controller = malloc(sizeof(usb_ohci_hc_t));
            if (controller == NULL) {
                return USB_ERR_NOMEM;
            }
            hc->hc_type = USB_OHCI;
            hc->hc_control = controller;
            ((usb_ohci_hc_t*)controller)->controller = hc;
            return usb_ohci_init((usb_ohci_hc_t*)controller, (uintptr_t)controller_base);
            break;
        case USB_EHCI:
            debug_printf("Failure: EHCI not yet implemented\n");
            break;
        case USB_XHCI:
            debug_printf("Failure: XHCI not yet implemented\n");
            break;
        default:
            return USB_ERR_INVAL;
            break;

    }
    return USB_ERR_OK;
}
