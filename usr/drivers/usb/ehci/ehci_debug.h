/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Debugging routines for local EHCI sub system
 */

#ifndef __EHCI_DEBUG_H
#define __EHCI_DEBUG_H

#include <usb/debug.h>

#if defined(EHCI_LOCAL_DEBUG) || defined(EHCI_GLOBAL_DEBUG)
#define dprintf(x...) printf("EHCI: " x)
#else
#define dprintf(x...) ((void)0)
#endif


#endif                          // __EHCI_DEBUG_H
