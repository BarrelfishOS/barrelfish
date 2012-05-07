/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains proxy implementation of 
 * function as described in ehci_services.h
 */

#ifndef EHCI_CLIENT_H
#define EHCI_CLIENT_H

#include <barrelfish/barrelfish.h>
#include "../ehci/ehci_services.h"

int connect_to_ehci_manager(void *args);

#endif                          // EHCI_CLIENT_H
