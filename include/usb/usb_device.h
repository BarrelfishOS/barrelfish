/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * This file contains defn of interal data structs
 * used to maintain device related information in
 * USB manager.
 */

#ifndef USB_DEVICE_H
#define USB_DEVICE_H

#include <stdint.h>
#include <stdio.h>

#include <usb/ch9.h>

// Connected to root hub
#define CONN_RH    1

// Disconnected from root hub
#define DISCONN_RH 0



// Related helper macros

#define EP_HI_SPEED    2
#define EP_FULL_SPEED  0
#define EP_LOW_SPEED   1


#define EP_TYPE_IN   1
#define EP_TYPE_OUT  0

#define EP_CONTROL   0
#define EP_ISO       1
#define EP_BULK      2
#define EP_INT       3

#define EP_NO_SYNC    0
#define EP_ASYNC      1
#define EP_ADPV       2
#define EP_SYNC       3

#define EP_DATA       0
#define EP_FB         1
#define EP_IMP_FB     2

// Takes ep.bEndpointAddress
#define GET_EP_DIR(x)      (x >> 7)

#define IS_EP_IN(x)   (GET_EP_DIR(x) & EP_TYPE_IN)
#define IS_EP_OUT(x)  ( !IS_EP_IN(x))

#define GET_EP_NUMBER(x)   (x & 0x0F)

// Takes ep.bmAttributes
#define GET_EP_TYPE(x)     (x & 0x3)

// Takes ep.wMaxPacketSize
#define GET_EP_PSZ(x)      (x & 0x07FF) // 0..10 (11) bits
#define GET_EP_MULTI(x)    ( 1 + (((x>>11) & 0x03)))    // EHCI = USB_20 + 1

/*
 * Generic device structure to save the read parameters from the
 * USB device
 */

#define USB_DEFAULT_ADDRESS 0

//FIXME: alternate setting logic

typedef struct usb_endpoint_t {
    usb_endpoint_descriptor ep;
    int pipe_allocated;
} usb_endpoint_t;

typedef struct usb_interface_t {
    usb_interface_descriptor intf;
    usb_endpoint_t *ep;
    int no_ep;
} usb_interface_t;

typedef struct usb_config_t {
    usb_interface_t *intf;
    int no_intf;
    usb_configuration_descriptor config;
    int curr_intf;

} usb_config_t;

typedef struct usb_device_t {
    int port;
    uint8_t address;
    int no_config;
    int device_state;

    char *device_manufacturer;
    int man_sz;

    char *device_name;
    int dev_sz;

    char *serial_number;
    int sr_sz;

    usb_device_descriptor desc;
    usb_config_t *config;

    int curr_config;

    // For internal use
    int free;
    //uint8_t toggle[2]; // Toggle 0=OUT, 1=IN
    //int status;
} usb_device_t;



//Max device limit is 127. So no harm done, statically allocated
usb_device_t usb_device_arr[N_USB_DEVICES];

/*
 * Address allocator function for USB devices
 */

uint8_t allocate_new_usb_address(void);
void release_address(uint8_t address);

/*
 * Verifies if a given byte is a valid request and does not
 * contain any reserved fields
 */

int verify_usb_request(uint8_t req);

/*
 * Generic printing functions for various realted data strcutures.
 */
void print_usb_device_descriptor(usb_device_descriptor udd);

void print_usb_configuration_descriptor(usb_configuration_descriptor ucd);

void print_usb_interface_descriptor(usb_interface_descriptor uid);

void print_usb_ep_descriptor(usb_endpoint_descriptor uepd);

void print_usb_device_request(usb_device_request udd);

void print_usb_device_info(usb_device_t dev);
/*
 * Periodically checks for the connectitivty in the physcial connection tree
 */
void check_connectivity(int device_id); //check physical connectivity in the tree

/*
 * init function, which init the tree and device array
 */
void usb_device_init(void);
void release_resources(usb_device_t dev);

#endif                          // USB_DEVICE_H
