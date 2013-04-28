/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_USB_H_
#define _USB_USB_H_

struct usb_status {
    uint16_t wStatus;
};

typedef struct usb_status usb_status_t;

#define USB_STATUS_SELF_POWERED 0x0001;
#define USB_STATUS_REMOTE_WAKEUP 0x0002;
#define USB_STATUS_EP_HALT  0x0001;

#endif
