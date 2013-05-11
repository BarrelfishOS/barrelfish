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

/**
 *
 */
union usb_ehci_hub_descriptor {
    struct usb_status status;
    struct usb_hub_port_status port_status;
    struct usb_hub_class_descriptor hub_desc;
    uint8_t temp[128];
};

#endif /* USB_EHCI_ROOT_HUB_H_ */
