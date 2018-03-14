/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitätestrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef E10K_VF_RESOURCES_H_
#define E10K_VF_RESOURCES_H_

/* Dump bytes of memory region to stdout */

void add_vf_resources(struct capref devid, struct capref regs, struct capref irq);
bool get_vf_resources(uint8_t vfn, struct capref* devid, struct capref* regs, struct capref* irq);
uint32_t num_vfs(void);
#endif

