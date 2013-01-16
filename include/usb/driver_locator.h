/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

//FIXME: Old driver registry which was maintained by 
//       USB manager but now this SHOULD be new logic
//       which is to ask SKB to located driver for a 
//       newly enumerated device. 

#ifndef DRIVER_LOCATOR_H
#define DRIVER_LOCATOR_H

#include <barrelfish/barrelfish.h>
#include <stdint.h>

#define MAX_DRIVER 10

typedef void (*driver_init) (int);

typedef struct usb_driver_registry_t {
    uint8_t usb_class_code;
    uint8_t usb_subclass_code;
    uint8_t usb_protocol_code;
    void (*driver_init) (int);
    int valid;
} usb_driver_registry_t;

usb_driver_registry_t driver_register[MAX_DRIVER];

void init_driver_registry(void);

driver_init look_up_driver(uint8_t, uint8_t, uint8_t);

#endif
