/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef USB_EHCI_MEMORY_H_
#define USB_EHCI_MEMORY_H_

struct usb_ehci_qh *usb_ehci_qh_alloc(void);
void usb_ehci_qh_free(struct usb_ehci_qh *qh);

struct usb_ehci_qtd *usb_ehci_qtd_alloc(void);
void usb_ehci_qtd_free(struct usb_ehci_qtd *qtd);

struct usb_ehci_sitd *usb_ehci_sitd_alloc(void);
void usb_ehci_sitd_free(struct usb_ehci_sitd *sitd);

struct usb_ehci_itd *usb_ehci_itd_alloc(void);
void usb_ehci_itd_free(struct usb_ehci_itd *itd);

usb_paddr_t usb_ehci_buffer_page_alloc(void);
void usb_ehci_buffer_page_free(usb_paddr_t buf);

struct usb_ehci_pframes *usb_ehci_pframes_alloc(void);
#endif /* USB_EHCI_MEMORY_H_ */
