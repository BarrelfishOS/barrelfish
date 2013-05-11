/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef USB_HUB_H_
#define USB_HUB_H_

#define USB_HUB_MAX_DEPTH   5

struct usb_xfer;

void usb_hub_alloc_hs_bandwidth(struct usb_xfer *xfer);
void usb_hub_free_hs_bandwidth(struct usb_xfer *xfer);


#endif /* USB_HUB_H_ */
