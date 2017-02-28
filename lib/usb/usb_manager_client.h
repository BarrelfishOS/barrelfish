/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_MANAGER_CLIENT_H_
#define USB_MANAGER_CLIENT_H_

#include <if/usb_manager_defs.h>
#include <if/usb_driver_defs.h>

extern struct usb_manager_binding *usb_manager;

void usb_driver_rx_detach_notify(struct usb_driver_binding *b);
void usb_driver_rx_done_notify(struct usb_driver_binding *b,
        uint32_t tid, uint32_t error, const uint8_t *data, size_t length);

struct usb_client_st {
    lib_usb_callback callback;
    void *st;
    uint16_t init_config;

    /*
     * --------------------------------------------------------------------------
     * Variables for connection management to the USB manager service
     * --------------------------------------------------------------------------
     */

    /// the iref of the usb manager for connections
    iref_t usb_manager_iref;

    /// ower own iref
    iref_t usb_driver_iref;

    /// our own driver binding with the usb manager
    struct usb_driver_binding *driver_binding;

    /// state variables for the ensuring that the exportation process is completed
    volatile uint8_t manager_connected;

};

#endif /* USB_MANAGER_CLIENT_H_ */
