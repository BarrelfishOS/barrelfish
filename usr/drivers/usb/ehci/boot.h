/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * File to share hardware booting entry point 
 * to the device server. Nothing special  
 */

#include <barrelfish/barrelfish.h>

#ifndef EHCI_BOOT_H
#define EHCI_BOOT_H

int ehci_boot(void *args);

#endif                          // EHCI_BOOT_H
