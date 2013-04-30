/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_USB_DRIVER_H_
#define LIB_USB_DRIVER_H_

#include <usb/usb.h>
#include <usb/usb_error.h>
#include <usb/usb_request.h>

usb_error_t usb_init(void);

usb_error_t usb_do_request(struct usb_device_request *req);
usb_error_t usb_do_request_write(struct usb_device_request *req,
        uint16_t length, void *data);
usb_error_t usb_do_request_read(struct usb_device_request *req,
        uint16_t *ret_length, void *ret_data);

#endif /* LIB_USB_DRIVER_H_ */
