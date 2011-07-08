/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_TOGGLE_SERVICES_H
#define USB_TOGGLE_SERVICES_H

void dev_toggle_reset(uint8_t add);
void set_dev_status(uint8_t add, uint8_t state);
uint8_t get_dev_status(uint8_t dev);

void map_init(void);

#endif                          // USB_TOGGLE_SERVICES_H
