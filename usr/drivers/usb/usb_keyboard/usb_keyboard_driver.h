/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_KEYBOARD_DRIVER_H_
#define USB_KEYBOARD_DRIVER_H_

#include <usb/usb_transfer.h>
#include <usb/class/usb_hid.h>


#define USB_KEYBOARD_BUFSIZE 64
#define USB_KEYBOARD_MODIFIIERS 8
#define USB_KEYBOARD_KEYCODES 6
#define USB_KEYBOARD_IN_BUFSIZE \
    (2*(USB_KEYBOARD_MODIFIIERS + (2*USB_KEYBOARD_KEYCODES)))


/// enumeration of USB keyboard transfers
enum {
    USB_KEYBOARD_DATA,          ///< used for data transfers to get key events
    USB_KEYBOARD_LED_CTRL,      ///< used for led control
    USB_KEYBOARD_NUM_TRANSFERS, ///< number of transfers
};

/**
 * this union represents the possible modifiers for the pressed key code
 */
union usb_keyboard_modifiers {
        struct {
            uint8_t ctrl_l : 1;
            uint8_t shift_l : 1;
            uint8_t alt_l : 1;
            uint8_t win_l : 1;
            uint8_t ctrl_r : 1;
            uint8_t shift_r : 1;
            uint8_t alt_r : 1;
            uint8_t win_r : 1;
            uint8_t eject : 1;
            uint8_t fn : 1;
            uint8_t _unused : 6;
        };
        uint16_t generic;
    };


/**
 * this structure represents a pressed key event with the corresponding
 * modifiers such as alt/shift/ctrl...
 */
struct usb_keyboard_data {
    union usb_keyboard_modifiers modifiers; ///< the activated modifiers
    uint8_t keycode[USB_KEYBOARD_KEYCODES]; ///> the extracted keycode
};


struct usb_keyboard_key {
    uint8_t report_id;
    struct usb_hid_location loc;
    uint8_t valid;
};

struct usb_keyboard {
    /* location of special keys */
    struct usb_keyboard_key ctrl_l;
    struct usb_keyboard_key ctrl_r;
    struct usb_keyboard_key shift_l;
    struct usb_keyboard_key shift_r;
    struct usb_keyboard_key alt_l;
    struct usb_keyboard_key alt_r;
    struct usb_keyboard_key win_l;
    struct usb_keyboard_key win_r;
    struct usb_keyboard_key events;
    struct usb_keyboard_key led_numlock;
    struct usb_keyboard_key led_capslock;
    struct usb_keyboard_key led_scrolllock;

    uint8_t num_config; ///< the number of configurations
    usb_xfer_id_t xferids[USB_KEYBOARD_NUM_TRANSFERS];
    uint8_t buffer[USB_KEYBOARD_BUFSIZE];

    uint8_t keyboard_id;
    int32_t keyboard_size;
    int32_t keyboard_led_size;
    uint8_t keyboard_led_state;

    /* input buffers */
    uint32_t input[USB_KEYBOARD_IN_BUFSIZE];
    uint16_t input_head;
    uint16_t input_tail;
    uint16_t input_size;
};

typedef struct usb_keyboard usb_keyboard_t;


void usb_keyboard_set_leds(void);

void usb_keyboard_deinit(void);
usb_error_t usb_keyboard_init(void);

#endif /* USB_KEYBOARD_DRIVER_H_ */
