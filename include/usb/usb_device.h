/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBUSB_DEVICE_H
#define LIBUSB_DEVICE_H

/*
 * ------------------------------------------------------------------------
 * USB Endpoint
 * ------------------------------------------------------------------------
 * This data structure defines an USB tendpoint reflecting the state on a
 * physical endpoint on the device.
 *
 * Fields:
 *  - transfers         queue of usb_xfers of this endpoint
 *  - descriptor        the endpoint descriptor
 *  - pipe_fn           pointer to the pipe functions (set by HC driver)
 *  - isoc_next         the next isochronus transfer
 *  - toggle_next       the next data toggle value
 *  - is_stalled        set if the endpoint is stalled
 *  - is_sync           is set if the data structure is in sync
 *  - unused            unused
 *  - iface             pointer to the interface
 *  - iface_index       the index of the interface
 *  - endpoint_address  the address of this endpoint
 *  - ref_allocation    reference count allocation
 *  - ref_bandwidth     reference count bandwidth
 *  - hs_start          high speed resource allocation start mask
 *  - hs_complete       high speed resource allocation complete mask
 *  - hs_uframe;        high speed micro frame
 *  - max_packet_size   the maximum packet size to be used for this endponit
 *  - status            the status uf this endpoint
 */
struct usb_endpoint
{
    uint8_t ep_direction;
    uint8_t ep_number;
    uint8_t ep_type;
    uint8_t ep_usage;
    uint8_t ep_sync;
    struct usb_interface *iface;
    uint8_t iface_index;
};



// endpoint status flag for usb_status_t
#define USB_ENDPOINT_STATUS_HALT 0x0001

// the USB control endpoint
#define USB_ENDPOINT_CONTROL 0

// the maximum number of endpoints
#define USB_ENDPOINT_MAX 32

/*
 * ------------------------------------------------------------------------
 * USB Interface
 * ------------------------------------------------------------------------
 * This data structure defines an USB tendpoint reflecting the state on a
 * physical endpoint on the device.
 *
 * Fields:
 *  - descriptor        pointer to the interface descriptor
 *  - alt_setting       alternative setting for this iface
 *  - iface_index       interface index of this interface
 *  - device            pointer to the device
 *  - num_endpoints     the number of endpoints for this interface
 *  - endpoints         array of pointer to the endpoints of this iface
 */
struct usb_interface
{
    uint8_t alt_setting;
    uint8_t parent_iface_index;
    uint8_t iface_number;
    uint8_t iface_class;
    uint8_t iface_subclass;
    uint8_t iface_protocol;
    uint8_t num_endpoints;
    uint8_t config;
    struct usb_endpoint endpoints[USB_ENDPOINT_MAX];
};

#define USB_INTERFACE_INDEX_ANY 0xFF


struct usb_device {
    struct usb_interface *ifaces;
    struct usb_endpoint *endpoints;
    struct usb_config_descriptor *config_desc;
    uint8_t dev_class;
    uint8_t dev_subclass;
    uint8_t dev_protocol;
    uint16_t vendor;
    uint16_t product;
    uint16_t version;
    uint8_t iface_max;
    uint8_t ep_max;
    uint8_t num_config;
    uint8_t current_config;
};

typedef struct usb_device usb_device_t;

void usb_device_init(void *desc);

uint8_t usb_device_get_num_config(void);
struct usb_interface *usb_device_get_iface(uint8_t iface);
usb_error_t usb_device_get_iface_count(uint8_t *ret_count);
usb_error_t usb_device_get_speed(usb_speed_t *ret_speed);
usb_error_t usb_device_state(void);
struct usb_config_descriptor *usb_device_get_cfg_desc(void);

usb_error_t usb_device_suspend(void);
usb_error_t usb_device_resume(void);
usb_error_t usb_device_powersave(void);

#endif /* USB_DEVICE_H_ */
