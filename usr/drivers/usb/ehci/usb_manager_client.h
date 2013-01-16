/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef  USB_MANAGER_CLIENT_H
#define  USB_MANAGER_CLIENT_H

#include <barrelfish/barrelfish.h>
#include <usb/usb_services.h>

/* This file act as wrapper to the functions provided by USB manager
 * server. This is implemented to have a better and cleaner interface to
 * hide server client communication logic from actual logic.
 *
 * The calls from logic will look like normal calls but here they are 
 * converted to the IDCs. Also sometimes synchronous interface is 
 * required. This can be achieved here by coordination. 
 */


// Main client thread which connects to USB manager server 
int connect_to_usb_manager(void *args);

#endif                          // USB_MANGER_CLIENT_H
