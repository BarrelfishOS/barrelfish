/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains macros used to debug local USB manager sub system 
 */

#ifndef __USB_DEBUG_H
#define __USB_DEBUG_H

#include <usb/debug.h>

#if defined(USB_LOCAL_DEBUG) || defined(USB_GLOBAL_DEBUG)
#define dprintf(x...) printf("\nUSB_MANAGER: " x)
#else
#define dprintf(x...) ((void)0)
#endif


#endif                          // __USB_DEBUG_H
