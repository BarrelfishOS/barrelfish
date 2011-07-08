/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Comtains set of basic device related structs
 * which are extracted from chapter 9 of USB 2.0
 * documentation.
 */

#ifndef USB_CH9_H
#define USB_CH9_H

#include <stdio.h>
#include <stdint.h>


#define N_USB_DEVICES 127

// Some macros, which are frequently used to
// construct a request

#define USB_DIR_HOST_TO_DEVICE  0x00
#define USB_DIR_DEVICE_TO_HOST  0x80

#define USB_REQTYPE_STANDARD    0x00
#define USB_REQTYPE_CLASS       0x20
#define USB_REQTYPE_VENDOR      0x40
#define USB_REQTYPE_RSVD        0x60

#define USB_RECP_DEVICE         0
#define USB_RECP_INTERFACE      1
#define USB_RECP_ENDPOINT       2
#define USB_RECP_OTHER          3

// Valid bRequests

#define USB_GET_STATUS         0
#define USB_CLEAR_FEATURE      1
#define USB_SET_FEATURE        3
#define USB_SET_ADDRESS        5
#define USB_GET_DESCRIPTOR     6
#define USB_SET_DESCRIPTOR     7
#define USB_GET_CONFIGURATION  8
#define USB_SET_CONFIGURATION  9
#define USB_GET_INTERFACE      10
#define USB_SET_INTERFACE      11
#define USB_SYNCH_FRAME        12

// Valid descriptor types

#define USB_DEVICE                     1
#define USB_CONFIGURATION              2
#define USB_STRING                     3
#define USB_INTERFACE                  4
#define USB_ENDPOINT                   5
#define USB_DEVICE_QUALIFIER           6
#define USB_OTHER_SPEED_CONFIGURATION  7
#define USB_INTERFACE_POWER            8

// Valid features selectors

#define USB_DEVICE_REMOTE_WAKEUP    1   // Recipient = Device
#define USB_ENDPOINT_HALT           0   // Recipient = Endpoint
#define USB_TEST_MODE               2   // Recipient = Device


/*
 * USB device request format, should be sent in a SETUP packet
 * Copied form the USB sepcification 2.0, page number #248
 */

#pragma pack(push,1)

typedef struct usb_dev_req_wIndex_ep {
    uint8_t ep_num:4;
    uint8_t rsvd:3;
    uint8_t dir:1;
    uint8_t rsvd1;
} __attribute__ ((packed)) usb_dev_req_wIndex_ep;


typedef struct usb_dev_req_wIndex_if {
    uint8_t if_num:8;
    uint8_t rsvd:8;
} __attribute__ ((packed)) usb_dev_req_wIndex_if;


typedef struct usb_device_request {
    uint8_t bmRequestType:8;
    uint8_t bRequest:8;
    uint16_t wValue:16;
    uint16_t wIndex:16;         // Should be either wIndex_ep or wIndex_if, as defined above
    uint16_t wLength:16;
} __attribute__ ((packed)) usb_device_request;


/*
 * USB device descriptor, as described in USB 2.0
 * specification on pages #262-270
 */

// Device Descriptor
typedef struct usb_device_descriptor {
    // Byte offset
    uint8_t bLength:8;          // 0
    uint8_t bDescriptorType:8;  // 1
    uint16_t bcdUSB:16;         // 2
    uint8_t bDeviceClass:8;     // 4
    uint8_t bDeviceSubClass:8;  // 5
    uint8_t bDeviceProtocol:8;  // 6
    uint8_t bMaxPacketSize0:8;  // 7
    uint16_t idVendor:16;       // 8
    uint16_t idProduct:16;      // 10
    uint16_t bcdDevice:16;      // 12
    uint8_t iManufacturer:8;    // 14
    uint8_t iProduct:8;         // 15
    uint8_t iSerialNumber:8;    // 16
    uint8_t bNumConfigurations:8;       // 17
} __attribute__ ((packed)) usb_device_descriptor;

// Configuration descriptor
typedef struct usb_configuration_descriptor {   // Byte offset
    uint8_t bLength:8;          // 0
    uint8_t bDescriptorType:8;  // 1
    uint16_t wTotalLength:16;   // 2
    uint8_t bNumInterfaces:8;   // 4
    uint8_t bConfigurationValue:8;      // 5
    uint8_t iConfiguration:8;   // 6
    uint8_t bmAttributes:8;     // 7
    uint8_t bMaxPower:8;        //8
} __attribute__ ((packed)) usb_configuration_descriptor;


// Interface descriptor
typedef struct usb_interface_descriptor {
    uint8_t bLength:8;
    uint8_t bDescriptorType:8;
    uint8_t bInterfaceNumber:8;
    uint8_t bAlternateSetting:8;
    uint8_t bNumEndpoints:8;
    uint8_t bInterfaceClass:8;
    uint8_t bInterfaceSubClass:8;
    uint8_t bInterfaceProtocol:8;
    uint8_t iInterface:8;
} __attribute__ ((packed)) usb_interface_descriptor;

// Endpoint descriptor
typedef struct usb_endpoint_descriptor {
    uint8_t bLength:8;
    uint8_t bDescriptorType:8;
    uint8_t bEndpointAddress:8;
    uint8_t bmAttributes:8;
    uint16_t wMaxPacketSize:16;
    uint8_t bInterval:8;
} __attribute__ ((packed)) usb_endpoint_descriptor;

typedef struct usb_string_descriptor {
    uint8_t bLength:8;
    uint8_t bDescriptorType:8;
    //FIXME: Assuming that whole stirng fits in one 64
    uint8_t len[62];
} usb_string_descriptor;

#pragma pack()

#endif                          // USB_CH9_H
