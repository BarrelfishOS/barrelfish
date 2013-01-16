/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * USBD = USB Bus Driver 
 * Contains functions used internally by USB manager 
 * to perform device enumeration and management. 
 */

#ifndef USBD_H
#define USBD_H

#include <barrelfish/barrelfish.h>
#include <usb/usb_device.h>
#include <usb/usb_services.h>

#include <usb/usb_pipe.h>

/*
 * These are generic functions 
 * which are used while interacting 
 * with the device.
 *
 * Most of them are deinfed in Chapter 9 
 * of USB 2.0 documentation 
 */

#define EHCI_DEBUG     1
#define EHCI_NO_DEBUG  0

void clear_feature(int device_id);
void get_configuration(int device_id);
void get_descriptor(int device_id);
void get_interface(int device_id);
void get_status(int device_id);
int get_ep_status(usb_device_t dev, uint8_t ep);
int get_device_status(usb_device_t dev);

int clear_ep_halt(usb_device_t dev, uint8_t ep);

int set_ep_halt(usb_device_t dev, uint8_t ep);
void set_address(int port, int add);
void set_configuration(int device_id, int config_index);
void set_descriptor(int device_id, int desc_index);
void set_feature(int device_id);
void set_interface(int device_id);

void usb_manager_init(void);
void release_device(uint8_t idx);
int clear_device_rwakeup(usb_device_t dev);
int set_device_remote_wakeup(usb_device_t dev);

#endif                          // USBD_H
