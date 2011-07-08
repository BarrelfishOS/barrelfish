/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains code for USB manager <-> driver communication 
 * logic. 
 */

#ifndef USB_DRIVER_MANAGMENT_H
#define USB_DRIVER_MANAGMENT_H

#include <barrelfish/barrelfish.h>

void locate_and_notify_driver(uint8_t dev,
                              uint8_t clss, uint8_t subclass, uint8_t protocol);

#endif                          // USB_DRIVER_MANAGMENT_H
