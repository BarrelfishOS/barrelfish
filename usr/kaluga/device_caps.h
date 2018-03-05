/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DEVICE_CAPS_H
#define DEVICE_CAPS_H

errval_t device_id_cap_create(struct capref dest, uint16_t bus, uint16_t device,
                              uint16_t function, uint16_t flags);
errval_t get_device_cap(lpaddr_t address, size_t size, struct capref* devframe);
errval_t init_device_caps_manager(void);
 
#endif // DEVICE_CAPS_H
