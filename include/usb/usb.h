/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBUSB_USB_H_
#define LIBUSB_USB_H_

/**
 *
 */
typedef enum usb_mode  {
    USB_MODE_HOST, /* initiates transfers */
    USB_MODE_DEVICE, /* bus transfer target */
    USB_MODE_DUAL /* can be host or device */
} usb_mode_t;
#define USB_MODE_MAX    (USB_MODE_DUAL+1)


/**
 * The USB device speed enumeration describes all possible USB speed
 * settings for a device.
 */
typedef enum usb_speed {
    USB_SPEED_VARIABLE,
    USB_SPEED_LOW,
    USB_SPEED_FULL,
    USB_SPEED_HIGH,
    USB_SPEED_SUPER,
} usb_speed_t;

typedef enum usb_hc_version {
    USB_UHCI=0x0100,
    USB_OHCI=0x0110,
    USB_EHCI=0x0200,
    USB_XHCI=0x0300
} usb_hc_version_t;

typedef enum usb_revision {
    USB_REV_UNKNOWN,
    USB_REV_PRE_1_0,
    USB_REV_1_0,
    USB_REV_1_1,
    USB_REV_2_0,
    USB_REV_2_5,
    USB_REV_3_0
} usb_revision_t;

/**
 *
 */
typedef enum usb_type {
    USB_TYPE_ISOC = 0,
    USB_TYPE_INTR,
    USB_TYPE_CTRL,
    USB_TYPE_BULK
} usb_type_t;


/*
 * definition of the usb physical address type
 */
typedef volatile uintptr_t usb_paddr_t;

/**
 *
 */
struct usb_status {
    uint16_t wStatus;
};
typedef struct usb_status usb_status_t;

#define USB_STATUS_SELF_POWERED     0x0001;
#define USB_STATUS_REMOTE_WAKEUP    0x0002;
#define USB_STATUS_EP_HALT          0x0001;


#define USB_DEBUG(x...) debug_printf(x)

#endif
