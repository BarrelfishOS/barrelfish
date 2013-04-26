/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_OHCI_MEMORY_H_
#define _USB_OHCI_MEMORY_H_

#include "usb_ohci_descriptors.h"



struct usb_ohci_td *usb_ohci_td_alloc();
void usb_ohci_td_free(struct usb_ohci_td *td);

struct usb_ohci_ed *usb_ohci_ed_alloc();
void usb_ohci_ed_free(struct usb_ohci_ed *ed);

usb_paddr_t usb_ohci_buffer_alloc(uint32_t size, uint32_t align);
void usb_ohci_buffer_free(usb_paddr_t  buf);






#endif /* _USB_OHCI_MEMORY_H_ */
