/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
 
#ifndef E1000_DEVIF_H
#define E1000_DEVIF_H

struct e1000_queue;

// interrupt_mode: 0 - none, 1 - normal, 2 - interrupt test
errval_t e1000_queue_create(struct e1000_queue ** q, struct capref* ep, uint32_t vendor, 
    uint32_t deviceid, uint32_t bus, uint32_t pci_device, uint32_t function, 
    unsigned interrupt_mode, void (*isr)(void *));
#endif
