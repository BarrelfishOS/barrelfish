/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains defn of constructors of various 'valid' USB 2.0
 * device requests. These are invoked by USB managet to 
 * construct a valid request. 
 */


#ifndef DEVICE_COMMANDS_H
#define DEVICE_COMMANDS_H

#include <barrelfish/barrelfish.h>

#include "usb_device.h"
/*
 * This files contains various USB device control
 * requests. 
 * These all are defined in Chapter 9, USB 2.0 documentation 
 */

// For reading usb device descriptor 
usb_device_request get_usb_device_descriptor_request(void);

// For reading the string descriptor 
usb_device_request get_usb_device_string_desc_request(uint8_t index);

// For reading a specific configuration descriptor 
usb_device_request get_usb_device_configuration_descriptor_request(uint8_t
                                                                   index);

// For seting device address 
usb_device_request get_usb_device_set_address_request(uint8_t add);

// For seting device address 
usb_device_request get_usb_device_set_config_request(uint8_t config_idx);

// For reading the current configuration value 
usb_device_request get_usb_device_current_configuration_request(void);

// For getting current ep status 
usb_device_request get_usb_device_ep_status_request(uint8_t ep);

// Clear halt 
usb_device_request get_usb_device_clear_halt_ep_request(uint8_t ep);

// Setting halt bit via software 
usb_device_request get_usb_device_set_halt_ep_request(uint8_t ep);

// For getting current device status 
usb_device_request get_usb_device_status_request(void);

// For setting rwakeup on device 
usb_device_request get_usb_device_set_rwakeup_request(void);

// For clearing remote wakeup
usb_device_request get_usb_device_clear_rwakeup_request(void);
/*---------------------------------------*/

/* 
 * couple of utility functions 
 */

// Returns the expected data direction 
//uint8_t get_setup_data_direction(usb_device_request req);

// Returns the expected data IN/OUT size for a given request 
//uint8_t get_expected_data_size(usb_device_request req);

#endif                          // DEVICE_COMMANDS_H
