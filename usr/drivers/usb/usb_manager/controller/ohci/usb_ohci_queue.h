/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_OHCI_QUEUE_H_
#define _USB_OHCI_QUEUE_H_

#include "usb_ohci_descriptors.h"


void usb_ohci_setup_standard_chain(struct usb_xfer *xfer, usb_ohci_ed_t **ed_last);
usb_ohci_ed_t *usb_ohci_remove_qh(usb_ohci_ed_t *ed, usb_ohci_ed_t *last);
usb_ohci_ed_t *usb_ohci_append_qh(usb_ohci_ed_t *ed, usb_ohci_ed_t *last);
#endif
