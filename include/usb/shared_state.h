/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains the struct which is shared between EHCI and 
 * USB manager. Contains 
 *                      - device endpoint toggle state
 *                      - connectivity status of the device
 */

#ifndef USB_TOGGLE_STATE_H
#define USB_TOGGLE_STATE_H

#include <barrelfish/barrelfish.h>

typedef struct usb_shared_state_t {
    uint8_t toggle[2];
    int status;
} usb_shared_state_t;


#endif                          // USB_TOGGLE_STATE_H
