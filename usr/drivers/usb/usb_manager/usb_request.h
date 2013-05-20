/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_REQUEST_H_
#define USB_REQUEST_H_

#include <usb/usb_error.h>

struct usb_manager_binding;
struct usb_device;
struct usb_device_request;

/// struct for managing the request state
struct usb_request_state {
    struct usb_manager_binding *bind;
    void (*callback)(void *a);
    void *data;
    uint16_t data_length;
    usb_error_t error;
    struct usb_xfer *xfer;
};


/*
 * Flounder Callback Functions
 */
void usb_rx_request_read_call(struct usb_manager_binding *_binding,
        uint8_t *request, size_t req_length);
void usb_rx_request_write_call(struct usb_manager_binding *_binding,
        uint8_t *request, size_t req_length, uint8_t *data, size_t data_length);
void usb_rx_request_call(struct usb_manager_binding *_binding,
        uint8_t *request, size_t req_length);

/*
 * Handle the requests
 */
usb_error_t usb_handle_request(struct usb_device *device, uint16_t flags,
        struct usb_device_request *req, struct usb_request_state *req_state,
        void *data, uint16_t *ret_length);


#endif /* USB_REQUEST_H_ */
