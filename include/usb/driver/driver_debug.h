/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains printing logic specific to a particular 
 * sub system, here USB drivers. 
 */

#ifndef __DRIVER_DEBUG_H
#define __DRIVER_DEBUG_H

#include <usb/debug.h>

#if defined(DRIVER_LOCAL_DEBUG) || defined(DRIVER_GLOBAL_DEBUG)
#define dprintf(x...) printf("USB_DRIVER: " x)
#else
#define dprintf(x...) ((void)0)
#endif


#endif                          // __DRIVER_DEBUG_H
