/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_DEVICE_H_
#define _USB_DEVICE_H_

/*
 * The USB device speed enumeration describes all possible USB speed
 * settings for a device.
 */
enum usb_device_speed {
    USB_DEVICE_SPEED_VARIABLE,
    USB_DEVICE_SPEED_LOW,
    USB_DEVICE_SPEED_FULL,
    USB_DEVICE_SPEED_HIGH,
    USB_DEVICE_SPEED_SUPER,
};

/*
 * The USB device state enumeration describes all the possible USB states
 * the device can be in.
 */
enum usb_device_state {
    USB_DEVICE_STATE_DETACHED,
    USB_DEVICE_TATE_ATTACHED,
    USB_DEVICE_STATE_POWERED,
    USB_DEVICE_STATE_ADDRESSED,
    USB_DEVICE_STATE_CONFIGURED,
};

#define USB_DEVICE_START_ADDR 0
#define USB_DEVICE_MIN_COUNT  2



 // device status flags
 #define USB_DEVICE_STATUS_SELF_POWERED  0x0001
 #define USB_DEVICE_STATUS_REMOTE_WAKEUP 0x0002

/**
 * ------------------------------------------------------------------------
 * USB Device Flags
 * ------------------------------------------------------------------------
 * This data structures contains device related flags
 *
 * Fields:
 *  - usb_mode          USB mode state, host or device mode
 *  - self_powered      set if the device is self powered
 *  - no_strings        set if the device does not have string support
 *  - remote_wakeup     set if remote wakup is enabled
 *  - peer_suspended    set if the peer (=device) is suspended
 *  - self_suspended    set if the device is self suspended
 */
struct usb_device_flags {
    enum usb_controller_mode usb_mode;  /* host or device mode */
    uint8_t self_powered : 1;
    uint8_t no_strings : 1;
    uint8_t remote_wakeup : 1;
    uint8_t peer_suspended : 1;
    uint8_t self_suspended : 1;
};


struct usb_device {
    struct usb_interface *ifaces;
    struct usb_endpoint ctrl_ep;    /* Control Endpoint 0 */
    struct usb_endpoint *endpoints;
    struct usb_host_controller *controller;
    struct usb_device *parent_hub;
    struct usb_device *parent_hs_hub;

    struct usb_config_descriptor *config_desc;

    enum usb_device_speed speed;
    enum usb_dev_state state;

    struct usb_hub *hub;        /* only if this is a hub */
    struct usb_xfer *ctrl_xfer[2];

    struct usb_endpoint *ep_clear_stall;   /* current clear stall endpoint */

    uint16_t power_needed;
    uint16_t language_id;

    uint8_t device_address;
    uint8_t device_index;
    uint8_t config_number;
    uint8_t config_index;
    uint8_t depth;
    uint8_t hub_port_index;
    uint8_t hub_port_number;
    uint8_t iface_max;
    uint8_t ep_max;

    struct usb_device_flags flags;

    struct usb_endpoint_descriptor ctrl_ep_desc;
    struct usb_device_descriptor device_desc;

    char *serial_number;
    char *manifacturer;
    char *product;

    // needed?
    struct usb_temp_data *usb_template_ptr;
};

#endif /* _USB_DEVICE_H_ */
