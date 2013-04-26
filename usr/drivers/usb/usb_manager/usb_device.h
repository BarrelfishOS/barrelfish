/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#define USB_DEVICE_START_ADDR 0
#define USB_DEVICE_MIN_COUNT  2



 // device status flags
 #define USB_DEVICE_STATUS_SELF_POWERED  0x0001
 #define USB_DEVICE_STATUS_REMOTE_WAKEUP 0x0002

struct usb_device {

    /* TODO: Type */
    uint8_t speed;
};
