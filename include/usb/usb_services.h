/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains services exported by USB manager to clients
 */

#ifndef USB_SERVICES_H
#define USB_SERVICES_H

#include <usb/usb_pipe.h>
#include <usb/ch9.h>


void notify_new_device(int port);
void notify_device_removal(int port);

/*---- USB pipe related managment */
void init_pipe(uint8_t dev, uint8_t type, uint8_t dir, usb_pipe_t * pipe);

int get_curr_config(uint8_t dev, usb_configuration_descriptor * config);

int get_curr_config_num(uint8_t dev);

void get_num_config(uint8_t dev, uint8_t * num);

int get_config(uint8_t dev, uint8_t num, usb_configuration_descriptor * config);

int set_config(uint8_t dev, uint8_t num);

int get_curr_intf(uint8_t dev, usb_interface_descriptor * intf);

void get_num_intf(uint8_t dev, uint8_t * num);

int get_intf(uint8_t dev, uint8_t num, usb_interface_descriptor * intf);

int set_intf(uint8_t dev, uint8_t num);

int get_curr_intf_num(uint8_t dev);


#endif                          // USB_SERVICES_H
