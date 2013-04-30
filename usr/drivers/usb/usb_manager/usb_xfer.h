/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_XFER_H_
#define USB_XFER_H_

void usb_xfer_enqueue(struct usb_xfer_queue *queue, struct usb_xfer *xfer);
void usb_xfer_dequeue(struct usb_xfer *xfer);
void usb_xfer_done(struct usb_xfer *xfer, usb_error_t err);
void usb_xfer_setup_struct(struct usb_xfer_setup_params *param);

#endif  /* USB_XFER_H_ */
