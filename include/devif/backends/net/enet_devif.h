/*
 * Copyright (c) 2020 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
 
#ifndef ENET_DEVIF_H
#define ENET_DEVIF_H

struct enet_queue;

errval_t enet_rx_queue_create(struct enet_queue ** q, struct capref* ep, struct capref* regs,
                              void (*isr)(void *));

errval_t enet_tx_queue_create(struct enet_queue ** q, struct capref* ep, struct capref* regs,
                              void (*isr)(void *));

#endif
