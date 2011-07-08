/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Global debugging macros
 * Enable them to debug any subsystem
 *
 * Alternatively you can also enable
 * printfs in a particular file by setting 
 * *_DEBUG_LOCAL ...added to each C source 
 * file. 
 */

#ifndef __USB_DEBUG_H
#define __USB_DEBUG_H


/* Enable it for debugging EHCI module */
//#define EHCI_GLOBAL_DEBUG

/* Enable it for debugging USB manager module */
//#define USB_GLOBAL_DEBUG


/* Enable it for debugging mass storage driver module */
//#define DRIVER_GLOBAL_DEBUG

#endif                          //__USB_DEBUG_H
