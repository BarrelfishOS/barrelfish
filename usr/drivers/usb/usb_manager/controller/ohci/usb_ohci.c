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

#include "ohci_device.h"

#include <usb/usb_error.h>

#include "usb_ohci.h"


/*
 * our mackerel base
 */
struct ohci_t ohci_base;


 /*
 * ------------------------------------------------------------------------
 * Function Prototypes
 * ------------------------------------------------------------------------
 */


/*
 * ------------------------------------------------------------------------
 * Exported Functions
 * ------------------------------------------------------------------------
 */

usb_error_t usb_ohci_init(usb_ohci_hc_t *hc)
{
    return USB_ERR_OK;
}



void	    usb_ohci_detach(usb_ohci_hc_t *hc)
{

}




void	    usb_ohci_interrupt(usb_ohci_hc_t *sc)
{

}
