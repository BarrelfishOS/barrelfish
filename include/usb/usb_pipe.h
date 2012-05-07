/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains defintion of logical communication channel
 * of USB ..called pipe !!
 */

#ifndef USB_PIPE_H
#define USB_PIPE_H

typedef struct usb_pipe_t {
    uint8_t dev;
    uint8_t ep_number;
    uint8_t ep_address;
    uint8_t ep_dir;
    uint8_t ep_type;
    uint16_t ep_psz;
    uint8_t multi;
    //usb_endpoint_descriptor ep;
    int valid;
} usb_pipe_t;

#endif                          //USB_PIPE_H
