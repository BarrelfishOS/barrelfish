/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_PARSE_H_
#define USB_PARSE_H_

struct usb_descriptor *usb_parse_next_descriptor(
        struct usb_config_descriptor *cd, struct usb_descriptor *_desc);

#if 0
struct usb_interface_descriptor *usb_idesc_foreach(
        struct usb_config_descriptor *cd, struct usb_idesc_parse_state *ps);
struct usb_endpoint_descriptor *
usb_edesc_foreach(struct usb_config_descriptor *cd,
        struct usb_endpoint_descriptor *ped);
/*struct usb_endpoint_ss_comp_descriptor *
 usb_ed_comp_foreach(struct usb_config_descriptor *cd,
 struct usb_endpoint_ss_comp_descriptor *ped);*/
uint8_t usbd_get_no_descriptors(struct usb_config_descriptor *cd, uint8_t type);

uint8_t
usbd_get_no_alts(struct usb_config_descriptor *cd,
        struct usb_interface_descriptor *id);
#endif
#endif /* USB_PARSE_H_ */
