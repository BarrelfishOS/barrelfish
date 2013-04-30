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

/**
 *
 */
const char *usb_manager_name = "usb_manager_service";

static void usb_bind_complete(void *st, errval_t err, struct usb_manager_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "USB manager binding failed");
    }

    usb_manager_rpc_client_init(&usb_manager, b);
}

/**
 * \brief   does the initialization of the USB library and the binding to the
 *          USB manager service
 *
 */
usb_error_t usb_init(void)
{
    errval_t err;

    printf("USB library initialization.\n");

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


    return USB_ERR_OK;
}


/**
 * \brief
 *
 * \return
 */
usb_error_t usb_do_request_write(struct usb_device_request *req,
        uint16_t length, void *data)
{
    return USB_ERR_OK;
}


/**
 * \brief
 *
 * \return
 */
usb_error_t usb_do_request_read(struct usb_device_request *req,
        uint16_t *ret_length, void *ret_data)
{
    return USB_ERR_OK;
}
