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
#include <usb/usb_device.h>
#include <usb/usb_xfer.h>

#include "../../usb_controller.h"
#include "../../usb_xfer.h"
#include "usb_ehci.h"
#include "usb_ehci_xfer.h"
#include "usb_ehci_memory.h"
#include "usb_ehci_queue.h"

void usb_ehci_xfer_remove(struct usb_xfer *xfer, usb_error_t error)
{

}

void usb_ehci_xfer_standard_setup(struct usb_xfer *xfer, usb_ehci_qh_t **qh_last)
{

}

uint8_t usb_ehci_xfer_is_finished(struct usb_xfer *xfer)
{
    return 0;
}
