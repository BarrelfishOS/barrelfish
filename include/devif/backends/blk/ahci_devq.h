/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _AHCI_DEVQ_H
#define _AHCI_DEVQ_H

#include <barrelfish/barrelfish.h>

#define MAX_BUFFERS 256

struct ahci_queue;

errval_t ahci_create(struct ahci_queue** q, void* st, uint64_t flags);
void ahci_interrupt_handler(void* q);


#endif // _AHCI_DEVQ_H
