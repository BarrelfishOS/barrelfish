/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "usb_ohci.h"
#include "usb_ohci_pipe.h"
#include "usb_ohci_root.h"

#define USB_OHCI_INTERRUPT_ENDPOINT 1


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



void	    ohci_detach(usb_ohci_hc_t *hc)
{

}




void	    ohci_interrupt(usb_ohci_hc_t *sc);
