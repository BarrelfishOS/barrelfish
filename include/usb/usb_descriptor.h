/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_DESCRIPTOR_H_
#define _USB_DESCRIPTOR_H_

#include <stdint.h>

//USB descriptor codes (USB Specification, Rev 2.0, Table 9.5)
#define USB_DESCRIPTOR_TYPE_DEVICE             1
#define USB_DESCRIPTOR_TYPE_CONFIG             2
#define USB_DESCRIPTOR_TYPE_STRING             3
#define USB_DESCRIPTOR_TYPE_INTERFACE          4
#define USB_DESCRIPTOR_TYPE_ENDPOINT           5
#define USB_DESCRIPTOR_TYPE_DEVICE_QUALIFIER   6
#define USB_DESCRIPTOR_TYPE_OTHER_SPEED_CONFIG 7
#define USB_DESCRIPTOR_TYPE_INTERFACE_POWER    8
#define USB_DESCRIPTOR_TYPE_OTG                9

// USB Specification Release Numbers
#define USB_RELEASE_NUMBER_10 0x0100
#define USB_RELEASE_NUMBER_11 0x0110
#define USB_RELEASE_NUMBER_20 0x0200
#define USB_RELEASE_NUMBER_25 0x0250
#define USB_RELEASE_NUMBER_30 0x0300

// USB release number masks
#define USB_RELEASE_NUMBER_MAJOR 0xFF00
#define USB_RELEASE_NUMBER_MINOR 0x00F0
#define USB_RELEASE_NUMBER_SUB   0x000F

/**
 * ------------------------------------------------------------------------
 * USB Generic Descriptor
 * ------------------------------------------------------------------------
 * Fields:
 *  - bLength:              length of the descriptor in bytes
 *  - bDescriptorType:      type of the descriptor
 *  - bDescriptorSubType:   subtype of the descriptor
 */
struct usb_descriptor
{
  uint8_t bLength;
  uint8_t bDescriptorType;
  uint8_t bDescriptorSubType;
};

typedef struct usb_descriptor usb_descriptor_t;

/**
 * ------------------------------------------------------------------------
 * USB Device Descriptor (USB Specification, Rev 2.0, Section 9.6.1)
 * ------------------------------------------------------------------------
 * General and global information about an USB device. Each USB device
 * has exactly one usb_device_descriptor.
 *
 * Fields:
 *  - bLength:           length of the descriptor in bytes
 *  - bDescriptorType:   always USB_DESCRIPTOR_TYPE_DEVICE
 *  - bcdUSB:            USB specification release number
 *  - bDeviceClass:      USB device class code (defined by USB-IF)
 *  - bDeviceSubClass:   USB Device subclass code (defined by USB-IF)
 *  - bDeviceProtocol:   Class specific protocol to be used by this device
 *  - bMaxPacketSize0:   maximum packet size for endpoint 0 (control)
 *  - idVendor:          Vendor ID (assigned by USB-IF)
 *  - idProduct:         Product ID (assigned by manufacturer)
 *  - bcdDevice:         device release number
 *  - iManufacturer:     index of string descriptor describing manufacturer
 *  - iProduct:          index of string descriptor describing product
 *  - iSerialNumber:     index of string describing device's serial number
 *  - bNumConfigurations:number of possible configurations
 */
struct usb_device_descriptor
{
  uint8_t bLength;
  uint8_t bDescriptorType;
  uint16_t bcdUSB;
  uint8_t bDeviceClass;
  uint8_t bDeviceSubClass;
  uint8_t bDeviceProtocol;
  uint8_t bMaxPacketSize0;
  uint16_t idVendor;
  uint16_t idProduct;
  uint16_t bcdDevice;
  uint8_t iManufacturer;
  uint8_t iProduct;
  uint8_t iSerialNumber;
  uint8_t bNumConfigurations;
};

typedef struct usb_device_descriptor usb_device_descriptor_t;

// size information of the device descriptor
#define USB_DEVICE_DESCRIPTOR_SIZE 18

/*
 * ------------------------------------------------------------------------
 * USB Device Qualifier Descriptor (USB Specification, Rev 2.0, Section 9.6.2)
 * ------------------------------------------------------------------------
 * This descriptor contains information about a high-speed capable device
 * that would change, if the device is operating at other speed:
 *  - device runs at full speed -> descriptor returns values for high speed.
 *
 * Fields:
 *  - bLength:           length of the descriptor in bytes
 *  - bDescriptorType:   always USB_DESCRIPTOR_TYPE_DEVICE_QUALIFIER
 *  - bcdUSB:            USB specification release number
 *  - bDeviceClass:      USB device class code (defined by USB-IF)
 *  - bDeviceSubClass:   USB Device subclass code (defined by USB-IF)
 *  - bDeviceProtocol:   Class specific protocol to be used by this device
 *  - bMaxPacketSize0:   maximum packet size for endpoint 0 (control)
 *  - bNumConfiguration: number of possible configurations
 *  - bReserved:         always ZERO
 */
struct usb_device_qualifier_descriptor
{
  uint8_t bLength;
  uint8_t bDescriptorType;
  uint16_t bcdUSB;
  uint8_t bDeviceClass;
  uint8_t bDeviceSubClass;
  uint8_t bDeviceProtocol;
  uint8_t bMaxPacketSize0;
  uint8_t bNumConfigurations;
  uint8_t bReserved;
};

typedef struct usb_device_qualifier_descriptor usb_device_qualifier_descriptor_t;

// size information of the device qualifier descriptor
#define USB_DEVICE_QUALIFIER_DESCRIPTOR_SIZE 10

/*
 * ------------------------------------------------------------------------
 * USB Configuration Descriptor (USB Specification, Rev 2.0, Section 9.6.3)
 * ------------------------------------------------------------------------
 * This descriptor contains information about a specific device
 * configuration. The bConfigurationValue is used as a parameter to
 * SetConfiguration().
 *
 * Each USB device has one or more configuration descriptors
 *
 * Fields:
 *  - bLength:              length of the descriptor in bytes
 *  - bDescriptorType:      always USB_DESCRIPTOR_TYPE_CONFIG
 *  - wTotalLength:         total length of data returned for this config
 *  						the configuration descriptor is followed by
 *  						the interface and endpoint descriptors
 *  - bNumInterface:        number of supported interface
 *  - bConfigurationValue:  parameter for SetConfiguration()
 *  - iConfiguration:       index of string descriptor describing this config
 *  - bmAttributes:         bit map of configuration characteristics
 *  - bMaxPower:            maximum power consumption in 2mA unit steps
 */
struct usb_config_descriptor
{
  uint8_t bLength;
  uint8_t bDescriptorType;
  uint16_t wTotalLength;
  uint8_t bNumInterfaces;
  uint8_t bConfigurationValue;
  uint8_t iConfiguration;
  uint8_t bmAttributes;
  uint8_t bMaxPower;
};

typedef struct usb_config_descriptor usb_config_descriptor_t;

// size information of the configuration descriptor
#define USB_CONFIG_DESCRIPTOR_SIZE 9

// values for the bit map
#define USB_CONFIG_SELF_POWERED     0x40
#define USB_CONFIG_REMOTE_WAKEUP    0x20
#define USB_CONFIG_BUS_POWERED      0x80

/*
 * ------------------------------------------------------------------------
 * USB Interface Descriptor (USB Specification, Rev 2.0, Section 9.6.5)
 * ------------------------------------------------------------------------
 * This descriptor contains information about a specific interface within
 * an USB configuration. The interface descriptor defines an unique set
 * of endpoints within the configuration.
 *
 * Interface descriptors cannot directly be acesses by Get/SetDescriptor(),
 * they are returned as a part of the configuration descriptor.
 *
 * Fields:
 *  - bLength:              length of the descriptor in bytes
 *  - bDescriptorType:      always USB_DESCRIPTOR_TYPE_INTERFACE
 *  - bInterfaceNumber:     number of this interface within the config
 *  - bAlternateSetting:    the value used to select the alternate setting
 *  - bNumEndpoints:        number of used endpoints by this interface
 *  - bInterfaceClass:      interface class code (assigned by USB-IF)
 *  - bInterfaceSubClass:   interface subclass code (assigned by USB-IF)
 *  - bInterfaceProtocol:   protocol code (qualified by class/subclass)
 *  - iInterface:           index of string descriptor describing this iface
 */
struct usb_interface_descriptor
{
  uint8_t bLength;
  uint8_t bDescriptorType;
  uint8_t bInterfaceNumber;
  uint8_t bAlternateSetting;
  uint8_t bNumEndpoints;
  uint8_t bInterfaceClass;
  uint8_t bInterfaceSubClass;
  uint8_t bInterfaceProtocol;
  uint8_t iInterface;
};

typedef struct usb_interface_descriptor usb_interface_descriptor_t;

// size information about the interface descriptor
#define USB_INTERFACE_DESCRIPTOR_SIZE 9

/*
 * ------------------------------------------------------------------------
 * USB Endpoint Descriptor (USB Specification, Rev 2.0, Section 9.6.6)
 * ------------------------------------------------------------------------
 * This descriptor contains information about one endpoint for an interface.
 * Endpoint descriptors are always returned as part of the configuration
 * inforamation by GetConfiguration().
 *
 * Each endpoint has its own endpoint descriptor.
 *
 * Fields:
 *  - bLength:          length of the descriptor in bytes
 *  - bDescriptorType:  always USB_DESCRIPTOR_TYPE_ENDPOINT
 *  - bEndpointAddress: encoded address of this endpoint
 *    - direction:      direction either IN | OUT
 *    - ep_number:      endpoint number
 *  - bmAttributes:     the endpoint's attributes
 *    - usage_type:     for ISOCHR only, DATA|FEEDBACK|...
 *    - sync_type:      for ISOCHR only, synchronization type
 *    - xfer_type:      transfer type of this endpoint
 *  - wMaxPacketSize:   maximum packet size for this endpoint
 *  - bInterval:        interval for polling in (micro)frames
 */
struct usb_endpoint_address
{
  uint8_t direction :1;
  uint8_t _reserved :3;
  uint8_t ep_number :4;
};

typedef struct usb_endpoint_address usb_endpoint_address_t;

struct usb_endpoint_attributes
{
  uint8_t _unused :2;
  uint8_t usage_type :2;
  uint8_t sync_type :2;
  uint8_t xfer_type :2;
};

typedef struct usb_endpoint_attributes usb_endpoint_attributes_t;

struct usb_endpoint_descriptor
{
  uint8_t bLength;
  uint8_t bDescriptorType;
  usb_endpoint_address_t bEndpointAddress;
  usb_endpoint_attributes_t bmAttributes;
  uint16_t wMaxPacketSize;
  uint8_t bInterval;
};

typedef struct usb_endpoint_descriptor usb_endpoint_descriptor_t;

// size information of endpoint descriptor
#define USB_ENDPOINT_DESCRIPTOR_SIZE 9

#define USB_ENDPOINT_DIRECTION_OUT  0
#define USB_ENDPOINT_DIRECTION_IN   1

#define USB_ENDPOINT_XFER_CONTROL   0x00
#define USB_ENDPOINT_XFER_ISOCHR    0x01
#define USB_ENDPOINT_XFER_BULK      0x02
#define USB_ENDPOINT_XFER_INTR      0x03

#define USB_ENDPOINT_SYNC_NON_ISO   0x00
#define USB_ENDPOINT_SYNC_NONE      0x00
#define USB_ENDPOINT_SYNC_ASYNC     0x04
#define USB_ENDPOINT_SYNC_ADAPT     0x08
#define USB_ENDPOINT_SYNC_SYNC      0x0A

#define USB_ENDPOINT_USAGE_NON_ISO  0x00
#define USB_ENDPOINT_USAGE_DATA     0x00
#define USB_ENDPOINT_USAGE_FEEDBACK 0x10
#define USB_ENDPOINT_USAGE_IMPLICIT 0x20
#define USB_ENDPOINT_USAGE_RESERVED 0x30

/*
 * ------------------------------------------------------------------------
 * USB String Descriptor (USB Specification, Rev 2.0, Section 9.6.7)
 * ------------------------------------------------------------------------
 * String descriptors contains string describing certain elements of other
 * USB descriptors. String descriptors are optional. All references to
 * string descriptors are set to zero if they are not implemented by
 * the device
 *
 * The strings are encoded using UNICODE and are NOT null terminated.
 *
 * Fields:
 *  - bLength:          length of the descriptor in bytes
 *  - bDescriptorType:  always USB_DESCRIPTOR_TYPE_STRING
 *  - bString:          char array containing the string (not null term.)
 *  - wLangID:          language ID code
 */
struct usb_string_descriptor_languages
{
  uint8_t bLength;
  uint8_t bDescriptorType;
  uint16_t wLangID[128];
};

typedef struct usb_string_descriptor_languages usb_string_descriptor_languages_t;

struct usb_string_descriptor
{
  uint8_t bLength;
  uint8_t bDescriptorType;
  char bString[256];
};

typedef struct usb_string_descriptor usb_string_descriptor_t;

#define USB_STRING_GET_ELEMENT_COUNT(sd)    ((sd->bLength -2 )/2)
#define USB_STRING_GET_STRLEN(sd)           ((sd->bLength -2 ))

#endif
