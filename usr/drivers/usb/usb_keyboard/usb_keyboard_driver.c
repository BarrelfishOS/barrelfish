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
        .interface = 0,
        .endpoint = USB_ENDPOINT_ADDRESS_ANY, /* any address */
        .direction = USB_ENDPOINT_DIRECTION_IN,
        .max_bytes = 0, /* use wMaxPacketSize */
        .flags = {
            .short_xfer_ok = 1,
            .pipe_on_falure = 1
        },
    },

    [USB_KEYBOARD_LED_CTRL]= {
        .type = USB_TYPE_CTRL,
        .interface = 0,
        .endpoint = USB_ENDPOINT_CONTROL,
        .max_bytes = sizeof(struct usb_device_request) + USB_KEYBOARD_BUFSIZE,
        .timeout = 1000,
        .direction = USB_ENDPOINT_DIRECTION_ANY
    },
};

#define USB_KEYBOARD_KEY_INDEX(c)      ((c) & 0xFF)
#define NN 0                /* no translation */

static const uint8_t usb_keyboard_keycodes[256] = {
    0,
    0,
    0,
    0,
    30,
    48,
    46,
    32, /* 00 - 07 */
    18,
    33,
    34,
    35,
    23,
    36,
    37,
    38, /* 08 - 0F */
    50,
    49,
    24,
    25,
    16,
    19,
    31,
    20, /* 10 - 17 */
    22,
    47,
    17,
    45,
    21,
    44,
    2,
    3, /* 18 - 1F */
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11, /* 20 - 27 */
    28,
    1,
    14,
    15,
    57,
    12,
    13,
    26, /* 28 - 2F */
    27,
    43,
    43,
    39,
    40,
    41,
    51,
    52, /* 30 - 37 */
    53,
    58,
    59,
    60,
    61,
    62,
    63,
    64, /* 38 - 3F */
    65,
    66,
    67,
    68,
    87,
    88,
    92,
    70, /* 40 - 47 */
    104,
    102,
    94,
    96,
    103,
    99,
    101,
    98, /* 48 - 4F */
    97,
    100,
    95,
    69,
    91,
    55,
    74,
    78,/* 50 - 57 */
    89,
    79,
    80,
    81,
    75,
    76,
    77,
    71, /* 58 - 5F */
    72,
    73,
    82,
    83,
    86,
    107,
    122,
    NN, /* 60 - 67 */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* 68 - 6F */
    NN,
    NN,
    NN,
    NN,
    115,
    108,
    111,
    113, /* 70 - 77 */
    109,
    110,
    112,
    118,
    114,
    116,
    117,
    119, /* 78 - 7F */
    121,
    120,
    NN,
    NN,
    NN,
    NN,
    NN,
    123, /* 80 - 87 */
    124,
    125,
    126,
    127,
    128,
    NN,
    NN,
    NN, /* 88 - 8F */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* 90 - 97 */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* 98 - 9F */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* A0 - A7 */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* A8 - AF */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* B0 - B7 */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* B8 - BF */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* C0 - C7 */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* C8 - CF */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* D0 - D7 */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* D8 - DF */
    29,
    42,
    56,
    105,
    90,
    54,
    93,
    106, /* E0 - E7 */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* E8 - EF */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN, /* F0 - F7 */
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
    NN,
/* F8 - FF */
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
 * \brief
 */
static void usb_keyboard_set_leds(void)
{

    struct usb_device_request req;

    req.bRequest = USB_HID_REQUEST_SET_REPORT;
    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.recipient = USB_REQUEST_RECIPIENT_INTERFACE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;
    req.wValue = USB_HID_REPORT_OUTPUT;

    req.wIndex = keyboard.usb_iface_number;

    memset(keyboard.buffer, 0, USB_KEYBOARD_BUFSIZE);

    uint8_t report_id = 0;

    if (keyboard.numlock.valid) {
        if (keyboard.keyboard_led_state.numlock) {
            usb_hid_put_data_unsigned(keyboard.buffer + 1,
                    USB_KEYBOARD_BUFSIZE - 1, &keyboard.numlock.loc, 1);
        }
        report_id = keyboard.numlock.report_id;
    }

    if (keyboard.scrolllock.valid) {
        if (keyboard.keyboard_led_state.scrolllock) {
            usb_hid_put_data_unsigned(keyboard.buffer + 1,
                    USB_KEYBOARD_BUFSIZE - 1, &keyboard.scrolllock.loc, 1);
        }
        report_id = keyboard.scrolllock.report_id;
    }

    if (keyboard.capslock.valid) {
        if (keyboard.keyboard_led_state.capslock) {
            usb_hid_put_data_unsigned(keyboard.buffer + 1,
                    USB_KEYBOARD_BUFSIZE - 1, &keyboard.capslock.loc, 1);
        }
        report_id = keyboard.capslock.report_id;
    }

    uint16_t len = keyboard.keyboard_led_size;
    /* check if we don't have too much LEDs set on */
    if (len > (USB_KEYBOARD_BUFSIZE - 1)) {
        len = (USB_KEYBOARD_BUFSIZE - 1);
    }

    keyboard.buffer[0] = report_id;
    void *data;
    if (report_id != 0) {
        len++;
        data = (void *) (keyboard.buffer);
    } else {
        data = (void *) (&keyboard.buffer[1]);
    }

    usb_error_t err = usb_do_request_write(&req, len, data);

    if (err != USB_ERR_OK) {
        debug_printf("WARNING: set LED request not executed propperly\n");
    }

}

static void usb_keyboard_transfer_start(void)
{
    if (keyboard.input_size < USB_KEYBOARD_IN_BUFSIZE) {
        usb_transfer_start(keyboard.xferids[USB_KEYBOARD_DATA]);
    }
}

static uint32_t usb_keyboard_read_char(void)
{
    uint32_t keycode;
    uint32_t action;
    int32_t usbcode;

    uint8_t next_code = 1;

    while (next_code) {
        /* return composed char */
        if (keyboard.composed_char > 0 && keyboard.composed_done) {
            action = keyboard.composed_char;
            keyboard.composed_char = 0;
            if (action > 0xFF) {
                /* invalid char */
                return (USB_KEYBOARD_KEY_ERROR);
            }
            return (action);
        }

        usbcode = usb_keyboard_get_key();
        if (usbcode == -1) {
            return (USB_KEYBOARD_KEY_NOKEY);
        }

        /* TODO: lookup keycode */
        keycode = usb_keyboard_keycodes[USB_KEYBOARD_KEY_INDEX(usbcode)];
        if (keycode == NN) {
            return (USB_KEYBOARD_KEY_NOKEY);
        }

        switch (keycode) {
            case 0x38:
                /* alt_l */
                if (usbcode & USB_KEYBOARD_KEY_RELEASE) {
                    if (!keyboard.composed_done) {
                        keyboard.composed_done = 1;
                    }
                    if (keyboard.composed_char > 0xFF) {
                        keyboard.composed_char = 0;
                    }
                } else {
                    if (keyboard.composed_done) {
                        keyboard.composed_done = 0;
                        keyboard.composed_char = 0;
                    }
                }
                break;

            case 0x5C:
                /* print screen */
                keycode = 0x54;
                break;

            case 0x68:
                /* pause / break */
                keycode = 0x6c;
                break;
        }

        if (usbcode & USB_KEYBOARD_KEY_RELEASE) {
            keycode |= USB_KEYBOARD_SCAN_RELEASE;
        }

        if (!keyboard.composed_done) {
            switch (keycode) {
                case 0x47:
                case 0x48:
                case 0x49:
                    /* keypad 7,8,9 */
                    keyboard.composed_char *= 10;
                    keyboard.composed_char += keycode - 0x40;
                    if (keyboard.composed_char <= 0xFF) {
                        continue;
                    }
                    break;
                case 0x4B:
                case 0x4C:
                case 0x4D:
                    /* keypad 4,5,6 */
                    keyboard.composed_char *= 10;
                    keyboard.composed_char += keycode - 0x47;
                    if (keyboard.composed_char <= 0xFF) {
                        continue;
                    }
                    break;
                case 0x4F:
                case 0x50:
                case 0x51:
                    /* keypad 1,2,3 */
                    keyboard.composed_char *= 10;
                    keyboard.composed_char += keycode - 0x4E;
                    if (keyboard.composed_char <= 0xFF) {
                        continue;
                    }
                    break;
                case 0x52:
                    keyboard.composed_char *= 10;
                    if (keyboard.composed_char <= 0xFF) {
                        continue;
                    }
                    break;
                    /* key released, no interest here */
                case USB_KEYBOARD_SCAN_RELEASE | 0x47:
                case USB_KEYBOARD_SCAN_RELEASE | 0x48:
                case USB_KEYBOARD_SCAN_RELEASE | 0x49: /* keypad 7,8,9 */
                case USB_KEYBOARD_SCAN_RELEASE | 0x4B:
                case USB_KEYBOARD_SCAN_RELEASE | 0x4C:
                case USB_KEYBOARD_SCAN_RELEASE | 0x4D: /* keypad 4,5,6 */
                case USB_KEYBOARD_SCAN_RELEASE | 0x4F:
                case USB_KEYBOARD_SCAN_RELEASE | 0x50:
                case USB_KEYBOARD_SCAN_RELEASE | 0x51: /* keypad 1,2,3 */
                case USB_KEYBOARD_SCAN_RELEASE | 0x52: /* keypad 0 */
                    continue;
                    break;
                case 0x38:
                    /* alt_l */
                    break;
                default:
                    if (keyboard.composed_char > 0) {
                        keyboard.composed_done = 1;
                        keyboard.composed_char = 0;
                        return (USB_KEYBOARD_KEY_ERROR);
                    }
                    break;
            }
        }
    }

    return (USB_KEYBOARD_KEY_NOKEY);
}

static uint32_t usb_keyboard_read(void)
{
    return (0);
}

static void usb_keyboard_process_data(void)
{
    if (0) {
        usb_keyboard_read();
    } else {
        uint32_t c;
        do {
            c = usb_keyboard_read_char();
            debug_printf("GOT CHAR: %c", c);
        } while (c != USB_KEYBOARD_KEY_NOKEY);
    }
    usb_keyboard_set_leds();
}

/**
 * \brief   this function gets called if the data transfer is completed
 *
 * \param   err     outcome of the transfer
 * \param   data    raw data buffer
 * \param   length  number of bytes in the data buffer
 *
 */
static void usb_keyboard_transfer_cb(usb_error_t err, void *data,
        uint32_t length)
{
    USB_DEBUG("usb_transfer_cb()\n");

    if (err != USB_ERR_OK) {
        debug_printf("WARNING: transfer not completed propperly.\n");
        return;
    }

    if (length == 0) {
        usb_keyboard_transfer_start();
        return;
    }

    uint8_t rid = 0;

    /*
     * if a keyboard ID is set, then the data contains a HID ID byte which we
     * have to remove first
     */
    if (keyboard.keyboard_id) {
        rid = *((uint8_t *) data);
        data++;
        length--;
        if (length == 0) {
            usb_keyboard_transfer_start();
            return;
        }
    }

    memset(&keyboard.new_data, 0, sizeof(keyboard.old_data));
    keyboard.modifiers.generic = 0x0000;

    USB_KEYBOARD_MODIFIER_CHECK(ctrl_l);
    USB_KEYBOARD_MODIFIER_CHECK(ctrl_r);
    USB_KEYBOARD_MODIFIER_CHECK(shift_l);
    USB_KEYBOARD_MODIFIER_CHECK(shift_l);
    USB_KEYBOARD_MODIFIER_CHECK(alt_l);
    USB_KEYBOARD_MODIFIER_CHECK(alt_l);
    USB_KEYBOARD_MODIFIER_CHECK(win_l);
    USB_KEYBOARD_MODIFIER_CHECK(win_l);

    keyboard.old_data.modifiers = keyboard.modifiers;

    if (keyboard.events.valid && (rid == keyboard.events.report_id)) {
        uint32_t i = keyboard.events.loc.count;
        if (i > USB_KEYBOARD_KEYCODES) {
            i = USB_KEYBOARD_KEYCODES;
        }
        if (i > length) {
            i = length;
        }

        while (i--) {
            keyboard.new_data.keycode[i] = usb_hid_get_data(data, length - i,
                    &keyboard.events.loc);
        }
    }

    if (keyboard.new_data.keycode[0] == USB_KEYBOARD_KEY_ERROR) {
        return;
    }

    union usb_keyboard_modifiers *old_mod = &keyboard.old_data.modifiers;
    union usb_keyboard_modifiers *new_mod = &keyboard.new_data.modifiers;

    /* check for changed modifiers */
    if (old_mod->generic != new_mod->generic) {
        USB_KEYBOARD_KEY_RELEASE_CHECK(ctrl_l, 0xe0);
        USB_KEYBOARD_KEY_RELEASE_CHECK(ctrl_r, 0xe4);
        USB_KEYBOARD_KEY_RELEASE_CHECK(shift_r, 0xe5);
        USB_KEYBOARD_KEY_RELEASE_CHECK(shift_l, 0xe1);
        USB_KEYBOARD_KEY_RELEASE_CHECK(alt_l, 0xe2);
        USB_KEYBOARD_KEY_RELEASE_CHECK(alt_r, 0xe6);
        USB_KEYBOARD_KEY_RELEASE_CHECK(win_r, 0xe7);
        USB_KEYBOARD_KEY_RELEASE_CHECK(win_l, 0xe3);
    }

    uint8_t key;
    uint8_t found = 0;

    /* check for released keys */
    for (uint32_t i = 0; i < USB_KEYBOARD_KEYCODES; i++) {
        key = keyboard.old_data.keycode[i];
        found = 0;
        if (key == 0) {
            continue;
        }
        for (uint32_t j = 0; j < USB_KEYBOARD_KEYCODES; j++) {
            if (keyboard.new_data.keycode[j] == 0) {
                continue;
            }
            if (key == keyboard.new_data.keycode[j]) {
                break;
                found = 1;
            }
        }
        if (!found) {
            usb_keyboard_put_key(key | USB_KEYBOARD_KEY_RELEASE);
        }
    }

    /* check for pressed keys */
    for (uint32_t i = 0; i < USB_KEYBOARD_KEYCODES; i++) {
        key = keyboard.new_data.keycode[i];
        if (key == 0) {
            continue;
        }
        for (uint32_t j = 0; j < USB_KEYBOARD_KEYCODES; j++) {
            if (keyboard.old_data.keycode[j] == 0) {
                continue;
            }
            if (key == keyboard.old_data.keycode[j]) {
                break;
            }
        }
        usb_keyboard_put_key(key | USB_KEYBOARD_KEY_PRESS);

    }

    keyboard.old_data = keyboard.new_data;

    usb_keyboard_process_data();
}

/*
 * --------------------------------------------------------------------------
 * USB Keyboard initialization and de-initialization functions
 * --------------------------------------------------------------------------
 */

/**
 * \brief   this function parses the HID descriptor and sets some special
 *          key location values in the keyboard struct
 *
 * \param ptr pointer to the hid descriptor
 * \param len the length of the HID descriptor
 */
static void usb_keyboard_parse_hid(const uint8_t *ptr, uint32_t len)
{
    uint32_t flags;

    /* check if there is an ID byte */
    keyboard.keyboard_size = usb_hid_report_size(ptr, len, USB_HID_KIND_INPUT,
            &keyboard.keyboard_id);

    /* figure out some keys */
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE0),
            USB_HID_KIND_INPUT, 0, &keyboard.ctrl_l.loc, &flags,
            &keyboard.ctrl_l.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.ctrl_l.valid = 1;
    }
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE4),
            USB_HID_KIND_INPUT, 0, &keyboard.ctrl_r.loc, &flags,
            &keyboard.ctrl_r.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.ctrl_r.valid = 1;
    }
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE1),
            USB_HID_KIND_INPUT, 0, &keyboard.shift_l.loc, &flags,
            &keyboard.shift_l.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.shift_l.valid = 1;
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
    }
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE6),
            USB_HID_KIND_INPUT, 0, &keyboard.alt_r.loc, &flags,
            &keyboard.alt_r.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.alt_r.valid = 1;
    }
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE3),
            USB_HID_KIND_INPUT, 0, &keyboard.win_l.loc, &flags,
            &keyboard.win_l.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.win_l.valid = 1;
    }
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0xE7),
            USB_HID_KIND_INPUT, 0, &keyboard.win_r.loc, &flags,
            &keyboard.win_r.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.win_r.valid = 1;
    }
    /* figure out event buffer */
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_KEYBOARD, 0x00),
            USB_HID_KIND_INPUT, 0, &keyboard.events.loc, &flags,
            &keyboard.events.report_id)) {
        keyboard.events.valid = 1;
    }

    /* figure out leds on keyboard */
    keyboard.keyboard_led_size = usb_hid_report_size(ptr, len,
            USB_HID_KIND_OUTPUT, NULL);

    USB_DEBUG("LED SIZE = %u\n", keyboard.keyboard_led_size);

    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_LEDS, 0x01),
            USB_HID_KIND_OUTPUT, 0, &keyboard.numlock.loc, &flags,
            &keyboard.numlock.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.numlock.valid = 1;
    }
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_LEDS, 0x02),
            USB_HID_KIND_OUTPUT, 0, &keyboard.capslock.loc, &flags,
            &keyboard.capslock.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.capslock.valid = 1;
    }
    if (usb_hid_locate(ptr, len,
            USB_HID_USAGE_COMBINE(USB_HID_USAGE_LEDS, 0x03),
            USB_HID_KIND_OUTPUT, 0, &keyboard.scrolllock.loc, &flags,
            &keyboard.scrolllock.report_id)) {
        if (flags & USB_HID_IO_VARIABLE)
            keyboard.scrolllock.valid = 1;
    }
}

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



    keyboard.num_config = usb_device_get_num_config();

    struct usb_interface *iface = usb_device_get_iface(0);

    if (iface != NULL && iface->iface_class != USB_HID_CLASS_CODE) {
        debug_printf("ERROR: device is not HID class..\n");
       return (USB_ERR_INVAL);
    }

    if (iface->iface_protocol != USB_HID_PROTOCOL_KEYBOARD) {
        debug_printf("ERROR: device is not a keyboard");
        return (USB_ERR_INVAL);
    }



    /*
     * setting up the USB transfers
     */
    usb_error_t err = usb_transfer_setup_intr(
            &keyboard_tconf[USB_KEYBOARD_DATA], usb_keyboard_transfer_cb,
            &keyboard.xferids[USB_KEYBOARD_DATA]);

    if (err != USB_ERR_OK) {
        debug_printf("Failed to setup USB transfer: %s\n", usb_get_error_string(err));
        return (err);
    }

    struct usb_hid_descriptor *hid_ptr;
    uint16_t hid_length;
    err = usb_hid_get_hid_descriptor(&hid_ptr, &hid_length, 0);
    if (err != USB_ERR_OK) {
        debug_printf("could not get the HID descriptor: %s\n", usb_get_error_string(err));
    }


    if (err == USB_ERR_OK) {
        USB_DEBUG("Parsing HID descriptor of %d bytes\n", (int16_t)hid_length);
        usb_keyboard_parse_hid((void *) hid_ptr, hid_length);
        free(hid_ptr);
    }




    /* TODO: do set idle request usbd_req_set_idle(sc->sc_udev, NULL, sc->sc_iface_index, 0, 0); */

    /* start the interrupt transfer */
    err = usb_transfer_start(keyboard.xferids[USB_KEYBOARD_DATA]);
    if (err != USB_ERR_OK) {
        USB_DEBUG("Failed to start the transfer: %s\n", usb_get_error_string(err));
    }

    USB_DEBUG("all ok sofar....\n");
        while(1);

    if (0) {
        usb_keyboard_transfer_cb(0, NULL, 0);
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
