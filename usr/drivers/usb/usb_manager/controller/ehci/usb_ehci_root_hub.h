/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_EHCI_ROOT_HUB_H_
#define USB_EHCI_ROOT_HUB_H_


void usb_ehci_roothub_interrupt(usb_ehci_hc_t *hc);
usb_error_t usb_ehci_roothub_exec(struct usb_device *device,
        struct usb_device_request *req, const void **ret_data,
        uint16_t *ret_length);
void usb_ehci_roothub_port_disown(usb_ehci_hc_t *sc, uint16_t index,
        uint8_t lowspeed);

#endif /* USB_EHCI_ROOT_HUB_H_ */
