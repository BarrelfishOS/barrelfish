/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef USB_TRANSFER_H_
#define USB_TRANSFER_H_

struct usb_device;
struct usb_xfer;
struct usb_xfer_config;

void usb_transfer_setup_ctrl_default(struct usb_device *device,
        struct usb_request_state *st);
void usb_transfer_start(struct usb_xfer *xfer);
void usb_transfer_stop(struct usb_xfer *xfer);
void usb_transfer_unsetup(struct usb_xfer **xfers, uint16_t xfer_count);
usb_error_t usb_transfer_setup(struct usb_device *device, const uint8_t *ifaces,
        struct usb_xfer **ppxfer, const struct usb_xfer_config *setup_start, uint16_t n_setup);
uint8_t usb_transfer_completed(struct usb_xfer *xfer);

#endif /* USB_TRANSFER_H_ */
