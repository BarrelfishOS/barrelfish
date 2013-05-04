/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <usb/usb.h>
#include <usb/usb_error.h>
#include <usb/usb_driver.h>

#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>

/*
 * variables for tracking the state
 */

static iref_t usb_manager_iref;
static struct usb_manager_rpc_client usb_manager;


static volatile uint8_t bind_complete = 0;

/**
 *
 */
const char *usb_manager_name = "usb_manager_service";

static void usb_bind_complete(void *st, errval_t err,
        struct usb_manager_binding *b)
{
    debug_printf("libusb: bind callback complete\n");

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "USB manager binding failed");
    }

    usb_manager_rpc_client_init(&usb_manager, b);

    uint16_t ret_status;
    err=usb_manager.vtbl.connect(&usb_manager, 0, &ret_status);

    bind_complete = 1;

}

/**
 * \brief   does the initialization of the USB library and the binding to the
 *          USB manager service
 *
 */
usb_error_t usb_driver_init(void)
{
    errval_t err;

    debug_printf("libusb: initialization.\n");

    err = nameservice_blocking_lookup(usb_manager_name, &usb_manager_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "USB manager service lookup failed");
    }

    err = usb_manager_bind(usb_manager_iref, usb_bind_complete,
            NULL /* state for bind_cb */, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "USB manager binding failed");
    }

    return USB_ERR_OK;
}

/**
 * \brief
 *
 * \return
 */
usb_error_t usb_do_request(struct usb_device_request *req)
{
    errval_t err;
    uint16_t *ret_status  = 0;
    usb_error_t ret;

    debug_printf("libusb: usb_do_request()");

    err = usb_manager.vtbl.request(&usb_manager, (uint8_t*) req, sizeof(req),
            ret_status);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "libusb: do_request rpc failed");
        return USB_ERR_IDC;
    }

    ret = (usb_error_t) *ret_status;
    free(ret_status);

    debug_printf("libusb: usb_do_request() succeeded");

    return ret;
}

/**
 * \brief
 *
 * \return
 */
usb_error_t usb_do_request_write(struct usb_device_request *req,
        uint16_t length, void *data)
{
    errval_t err;
    usb_error_t ret;
    uint16_t *ret_status = 0;

    debug_printf("libusb: usb_do_request_write()");

    err = usb_manager.vtbl.request_write(&usb_manager, (uint8_t*) req,
            sizeof(req), (uint8_t *) data, length, ret_status);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "libusb: do_request_write rpc failed");
        return USB_ERR_IDC;
    }

    debug_printf("libusb: usb_do_request_write() succeeded");

    ret = (usb_error_t) *ret_status;
    free(ret_status);

    return ret;
}

/**
 * \brief
 *
 * \return
 */
usb_error_t usb_do_request_read(struct usb_device_request *req,
        uint16_t *ret_length, void **ret_data)
{
    errval_t err;
    uint16_t *ret_status  = 0;
    uint8_t *data  = 0;
    size_t *length  = 0;
    usb_error_t ret;

    debug_printf("libusb: usb_do_request_read()");

    err = usb_manager.vtbl.request_read(&usb_manager, (uint8_t*) req,
            sizeof(req), (uint8_t **) &data, length, ret_status);

    *ret_length = *length;
    free(length);
    ret = (usb_error_t) *ret_status;
    free(ret_status);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "libusb: do_request_write rpc failed");
        *ret_length = 0;
        *ret_data = NULL;
        return USB_ERR_IDC;
    }

    debug_printf("libusb: usb_do_request_read() got data (len=%i)", *ret_length);

    *ret_data = (void *) data;

    return ret;
}
