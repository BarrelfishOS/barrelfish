/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/**
 *
 *
 *
 *
 */

#ifndef _LIB_USB_HUB_REQUEST_H
#define _LIB_USB_HUB_REQUEST_H

/*
 * USB Hub Class Specific Request Codes
 * (USB Specification, Rev 2.0, Table 11.16)
 */
#define USB_HUB_REQ_GET_STATUS     	 0
#define USB_HUB_REQ_CLEAR_FEATURE  	 1
#define USB_HUB_REQ_SET_FEATURE  	 3
#define USB_HUB_REQ_GET_DESCRIPTOR   6
#define USB_HUB_REQ_SET_DESCRIPTOR   7
#define USB_HUB_REQ_CLEAR_TT_BUFFER  8
#define USB_HUB_REQ_RESET_TT		 9
#define USB_HUB_REQ_GET_TT_STATE  	10
#define USB_HUB_REQ_STOP_TT			11

/*
 * USB Hub Class Specific Request Codes
 * (USB Specification, Rev 2.0, Table 11.17)
 */
#define USB_HUB_FEATURE_C_HUB_LOCAL_POWER	0
#define USB_HUB_FEATURE_C_HUB_OVER_CURRENT	1
#define USB_HUB_FEATURE_PORT_CONNECTION		0
#define USB_HUB_FEATURE_PORT_ENABLE			1
#define USB_HUB_FEATURE_PORT_SUSPEND		2
#define USB_HUB_FEATURE_PORT_OVER_CURRENT	3
#define USB_HUB_FEATURE_PORT_RESET			4
#define USB_HUB_FEATURE_PORT_POWER			8
#define USB_HUB_FEATURE_PORT_LOW_SPEED		9
#define USB_HUB_FEATURE_C_PORT_CONNECTION	16
#define USB_HUB_FEATURE_C_PORT_ENABLE		17
#define USB_HUB_FEATURE_C_PORT_SUSPEND		18
#define USB_HUB_FEATURE_C_PORT_OVER_CURRENT	19
#define USB_HUB_FEATURE_C_PORT_RESET		20
#define USB_HUB_FEATURE_PORT_TEST			21
#define USB_HUB_FEATURE_PORT_INDICATOR		22

usb_error_t
usb_hub_clear_hub_feature(uint16_t feature);
usb_error_t
usb_hub_clear_port_feature(uint16_t feature, uint8_t sel, uint8_t port);
usb_error_t
usb_hub_clear_tt_buffer(uint8_t dev_addr, uint8_t ep_num, uint8_t ep_type,
        uint8_t direction, uint16_t tt_port);
usb_error_t
usb_hub_get_hub_status(struct usb_hub_status *ret_status);
usb_error_t
usb_hub_get_port_status(uint16_t port, struct usb_hub_port_status *ret_status);
usb_error_t
usb_hub_reset_tt(uint16_t port);

usb_error_t
usb_hub_set_hub_feature(uint16_t feature);
usb_error_t
usb_hub_set_port_feature(uint16_t feature, uint8_t selector, uint8_t port);
usb_error_t
usb_hub_get_tt_state(struct usb_hub_tt_flags flags, uint16_t port,
        struct usb_hub_tt_state *ret_state);
usb_error_t
usb_hub_stop_tt(uint16_t port);

usb_error_t
usb_hub_get_hub_descriptor(uint16_t max_length,
        struct usb_hub_class_descriptor *ret_desc);
usb_error_t
usb_hub_set_hub_descriptor(uint16_t desc_length,
        struct usb_hub_class_descriptor *desc);

#endif
