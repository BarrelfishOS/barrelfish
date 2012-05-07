/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Contains all servies exported by EHCI driver server 
 */

#ifndef EHCI_SERVICES_H
#define EHCI_SERVICES_H

#include <barrelfish/barrelfish.h>

// contain core I/O services 
#include "ehci_core.h"

// additional mapping service
void map_dev_page(struct capref cap, uint32_t sz);

// get core id on which EHCI is running 
uint64_t get_ehci_core_id(void);

#endif                          // EHCI_SERVICES_H
