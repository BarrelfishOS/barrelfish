/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <barrelfish/barrelfish.h>

#include <usb/usb.h>
#include <usb/usb_error.h>
#include <usb/usb_device.h>

#include "usb_device.h"

static struct usb_device *devices[USB_MAX_DEVICES];
static struct usb_device *devices_pending = NULL;
static struct usb_device *device_process = NULL;

void usb_device_insert_pending(struct usb_device *new_device)
{
    new_device->next = devices_pending;
    devices_pending = new_device;
}

void usb_device_config_complete(struct usb_device *device)
{
    assert(device == device_process);

    devices[device->device_address] = device;

    device_process = NULL;
}

struct usb_device * usb_device_get_pending(void)
{
    if (device_process) {
        return device_process;
    }

    device_process = devices_pending;
    devices_pending = device_process->next;

    return devices_pending;
}

struct usb_device *usb_device_get_by_address(uint8_t address)
{
    return devices[address];
}

struct usb_device *usb_device_alloc(struct usb_device *parent_hub, uint8_t depth,
        uint8_t portindex, uint8_t portno, usb_speed_t speed, usb_mode_t mode)
{
    return (NULL);
}

void usb_device_free(struct usb_device * device, uint8_t flag)
{

}
