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
#include <usb/usb_device.h>
#include <usb/usb_request.h>
#include <usb/usb_transfer.h>
#include <usb/class/usb_hid.h>

#include "usb_keyboard_driver.h"

static struct usb_keyboard keyboard;

/*
 * setup information for the two transfer types needed
 */

static usb_transfer_setup_t keyboard_tconf[USB_KEYBOARD_NUM_TRANSFERS] = {
    [USB_KEYBOARD_DATA] = {
        .type = USB_TYPE_INTR,
        .interface = 0x00,
        .endpoint = 0x01, /* any address */
        .direction = USB_ENDPOINT_DIRECTION_IN,
        .max_bytes = 0, /* use wMaxPacketSize */
        .flags = {
            .short_xfer_ok = 1,
            .pipe_on_falure = 1
        },
    },

    [USB_KEYBOARD_LED_CTRL]= {
        .type = USB_TYPE_CTRL,
        .interface = 0x00,
        .endpoint = USB_ENDPOINT_CONTROL,
        .max_bytes = sizeof(struct usb_device_request) + USB_KEYBOARD_BUFSIZE,
        .timeout = 1000,
        .direction = USB_ENDPOINT_DIRECTION_ANY
    },
};

/**
 *
 */
static void usb_keyboard_put_key(uint32_t key)
{
    if (keyboard.input_size < USB_KEYBOARD_IN_BUFSIZE) {
        keyboard.input[keyboard.input_tail] = key;
        ++(keyboard.input_size);
        ++(keyboard.input_tail);
        if (keyboard.input_tail >= USB_KEYBOARD_IN_BUFSIZE) {
            keyboard.input_tail = 0;
        }
    } else {
        debug_printf("WARNING: input buffer is full\n");
    }
}

/**
 *
 */
static int32_t usb_keyboard_get_key(void)
{
    int32_t c;

    if (keyboard.input_size == 0) {
        /* start transfer, if not already started */
        usb_transfer_start(keyboard.xferids[USB_KEYBOARD_DATA]);
    }

    if (keyboard.input_size == 0) {
        c = -1;
    } else {
        c = keyboard.input[keyboard.input_head];
        --(keyboard.input_size);
        ++(keyboard.input_head);
        if (keyboard.input_head >= USB_KEYBOARD_IN_BUFSIZE) {
            keyboard.input_head = 0;
        }
    }
    return (c);
}

/**
 *
 */
static void usb_keyboard_parse_hid(const uint8_t *ptr, uint32_t len)
{
    uint32_t flags;

    /* check if there is an ID byte */
    keyboard.keyboard_size = usb_hid_report_size(ptr, len,
        USB_HID_KIND_INPUT, &keyboard.keyboard_id);

    /* figure out some keys */
    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE0),
        USB_HID_KIND_INPUT, 0, &keyboard.ctrl_l.loc, &flags,
        &keyboard.ctrl_l.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.ctrl_l.valid = 1;
        USB_DEBUG("Found left control\n");
    }
    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE4),
        USB_HID_KIND_INPUT, 0, &keyboard.ctrl_r.loc, &flags,
        &keyboard.ctrl_r.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.ctrl_r.valid = 1;
        USB_DEBUG("Found right control\n");
    }
    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE1),
        USB_HID_KIND_INPUT, 0, &keyboard.shift_l.loc, &flags,
        &keyboard.shift_l.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.shift_l.valid = 1;
        USB_DEBUG("Found left shift\n");
    }
    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE5),
        USB_HID_KIND_INPUT, 0, &keyboard.shift_r.loc, &flags,
        &keyboard.shift_r.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.shift_r.valid = 1;
        USB_DEBUG("Found right shift\n");
    }
    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE2),
        USB_HID_KIND_INPUT, 0, &keyboard.alt_l.loc, &flags,
        &keyboard.alt_l.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.alt_l.valid = 1;
        USB_DEBUG("Found left alt\n");
    }
    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE6),
        USB_HID_KIND_INPUT, 0, &keyboard.alt_r.loc, &flags,
        &keyboard.alt_r.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.alt_r.valid = 1;
        USB_DEBUG("Found right alt\n");
    }
    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE3),
        USB_HID_KIND_INPUT, 0, &keyboard.win_l.loc, &flags,
        &keyboard.win_l.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.win_l.valid = 1;
        USB_DEBUG("Found left GUI\n");
    }
    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE7),
        USB_HID_KIND_INPUT, 0, &keyboard.win_r.loc, &flags,
        &keyboard.win_r.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.win_r.valid = 1;
        USB_DEBUG("Found right GUI\n");
    }
    /* figure out event buffer */
    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0x00),
        USB_HID_KIND_INPUT, 0, &keyboard.events.loc, &flags,
        &keyboard.events.report_id)) {
        keyboard.events.valid = 1;
        USB_DEBUG("Found keyboard events\n");
    }

    /* figure out leds on keyboard */
    keyboard.keyboard_led_size = usb_hid_report_size(ptr, len,
            USB_HID_KIND_OUTPUT, NULL);

    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_LEDS, 0x01),
        USB_HID_KIND_OUTPUT, 0, &keyboard.led_numlock.loc, &flags,
        &keyboard.led_numlock.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.led_numlock.valid = 1;
        USB_DEBUG("Found keyboard numlock\n");
    }
    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_LEDS, 0x02),
        USB_HID_KIND_OUTPUT, 0, &keyboard.led_capslock.loc, &flags,
        &keyboard.led_capslock.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.led_capslock.valid = 1;
        USB_DEBUG("Found keyboard capslock\n");
    }
    if (usb_hid_locate(ptr, len,
        USB_HID_USAGE_COMBINE(USB_HID_USAGE_LEDS, 0x03),
        USB_HID_KIND_OUTPUT, 0, &keyboard.led_scrolllock.loc, &flags,
        &keyboard.led_scrolllock.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.led_scrolllock.valid = 1;
        USB_DEBUG("Found keyboard scrolllock\n");
    }
}


/**
 * \brief   this function gets called if the data transfer is completed
 *
 * \param   err     outcome of the transfer
 * \param   data    raw data buffer
 * \param   length  number of bytes in the data buffer
 *
 */
static void usb_transfer_cb(usb_error_t err, void *data, uint32_t length)
{
    USB_DEBUG("usb_transfer_cb()\n");
}


/*
 * --------------------------------------------------------------------------
 * USB Keyboard initialization and de-initialization functions
 * --------------------------------------------------------------------------
 */



/**
 * \brief   initializes the USB keyboard
 */
usb_error_t usb_keyboard_init(void)
{
    USB_DEBUG("usb_keyboard_init()\n");

    memset(&keyboard, 0, sizeof(struct usb_keyboard));

    /*
     * The HID class uses the standard request Get_Descriptor as described in
     * the USB Specification. When a Get_Descriptor(Configuration) request is
     * issued, it returns the Configuration descriptor, all Interface
     * descriptors, all Endpoint descriptors, and the HID descriptor for each
     * interface.
     */
    const struct usb_generic_descriptor *gen_desc = usb_get_generic_descriptor();

    assert(gen_desc != NULL);

    keyboard.num_config = gen_desc->device.bNumConfigurations;

    if (gen_desc->iface->bInterfaceClass != USB_HID_IFCLASS_CODE) {
        debug_printf("ERROR: Not a HID class device \n");
        return (USB_ERR_BAD_CONTEXT);
    }

    /* todo: check if it is really a key board */

    /* TODO: Check for collection devices i.e. mouse/keyboard combinations */

    /*
     * setting up the USB transfers
     */
    usb_error_t err = usb_transfer_setup_intr(&keyboard_tconf[USB_KEYBOARD_DATA],
            usb_transfer_cb, &keyboard.xferids[USB_KEYBOARD_DATA]);

    if (err != USB_ERR_OK) {
        debug_printf("Failed to setup USB transfer");
        return (err);
    }

    struct usb_hid_descriptor *hid_ptr;
    uint16_t hid_length;
    err = usb_req_get_hid_descriptor(&hid_ptr, &hid_length, 0);

    if (err == USB_ERR_OK) {
        USB_DEBUG("Parsing HID descriptor of %d bytes\n", (int16_t)hid_length);
        usb_keyboard_parse_hid((void *)hid_ptr, hid_length);

        free(hid_ptr);
    }

    /* TODO: do set idle request usbd_req_set_idle(sc->sc_udev, NULL, sc->sc_iface_index, 0, 0); */

    /* start the interrupt transfer */
    usb_transfer_start(keyboard.xferids[USB_KEYBOARD_DATA]);

    if (0) {
        usb_transfer_cb(0, NULL, 0);
        usb_keyboard_put_key(0);
        usb_keyboard_get_key();
    }
    return (USB_ERR_OK);
}

/**
 * \brief deinitializes the transfers upon shutting down the keyboard
 *        driver
 */
void usb_keyboard_deinit(void)
{
    for (uint32_t i = 0; i < USB_KEYBOARD_NUM_TRANSFERS; i++) {
        usb_transfer_unsetup(keyboard.xferids[i]);
    }

}
