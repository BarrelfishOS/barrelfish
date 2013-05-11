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


struct usb_hcdi_bus_fn *usb_ehci_get_bus_fn(void)
{
    return NULL;
}
