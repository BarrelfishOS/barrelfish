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
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <usb/usb.h>

#include "usb_keyboard_driver.h"
#include "usb_keyboard_service.h"


static void keyboard_cb(void* st, usb_error_t uerr) {
    if (uerr != USB_ERR_OK) {
        debug_printf("ERROR: Could not initialize the USB driver library: %d\n", uerr);
        exit(EXIT_FAILURE);
    }

    uerr = usb_keyboard_init();

    if (uerr != USB_ERR_OK) {
        debug_printf("ERROR: Could not initialize the USB keyboard driver library\n");
        exit(EXIT_FAILURE);
    }

    errval_t err = usb_keyboard_service_init();

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed initializing the keyboard service");
        exit(EXIT_FAILURE);
    }

    uerr = usb_keyboard_start_transfers();

    if (uerr != USB_ERR_OK) {
        exit(EXIT_FAILURE);
    }

    debug_printf("Keyboard initialized.\n");
}

int main(int argc, char *argv[])
{
    USB_DEBUG("####### usb keyboard driver start #######\n");

    usb_error_t uerr = usb_lib_init(USB_CONFIGURATION_DEFAULT, keyboard_cb, NULL);


    if (uerr != USB_ERR_OK) {
        debug_printf("ERROR: Could not initialize the USB driver library\n");
        return (EXIT_FAILURE);
    }

    messages_handler_loop();

    USB_DEBUG("####### usb keyboard driver terminated #######\n");
}
