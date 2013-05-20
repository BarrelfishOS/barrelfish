/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>

#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>

#include <usb/usb.h>
#include <usb/usb_device.h>
#include <usb/usb_descriptor.h>

#include "usb_manager_client.h"


struct usb_generic_descriptor *gen_descriptor = NULL;

const struct usb_generic_descriptor *usb_get_generic_descriptor(void)
{
    return (gen_descriptor);
}


usb_error_t usb_device_get_iface_count(uint8_t *ret_count)
{

    return (USB_ERR_OK);
}


usb_error_t usb_device_get_speed(usb_speed_t *ret_speed)
{
    return (USB_ERR_OK);
}


usb_error_t usb_device_state(void)
{
    return (USB_ERR_OK);
}

usb_error_t usb_device_suspend(void)
{
    return (USB_ERR_OK);
}


usb_error_t usb_device_resume(void)
{
    return (USB_ERR_OK);
}
