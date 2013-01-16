/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef EHCI_TOGGLE_STATE_H
#define EHCI_TOGGLE_STATE_H

#include <usb/shared_state.h>

void map_dev_arr(struct capref cap, uint32_t sz);

usb_shared_state_t *get_dev_arr_add(void);


// Logic copied from Linux usb/core/hcd.h 
// Data toggle maintainiance 
#define USB_GET_TOGGLE(dev, ep, out) ((dev_arr[dev].toggle[out] >> (ep)) & 1)
#define USB_DO_TOGGLE(dev, ep, out)  (dev_arr[dev].toggle[out] ^= (1 << (ep)))
#define USB_SET_TOGGLE(dev, ep, out, bit) \
                (dev_arr[dev].toggle[out] = (dev_arr[dev].toggle[out] & ~(1 << (ep))) | \
                 ((bit) << (ep)))

void external_toggle(uint8_t dev, uint8_t num, uint8_t dir);

#endif                          // EHCI_TOGGLE_STATE_H
