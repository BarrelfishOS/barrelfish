/* Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitätstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBQDRIVER_H_
#define LIBQDRIVER_H_

void write_queue_tails(void);
size_t check_queue_0(void);
errval_t terminate_queue_0(void);
errval_t init_queue_0(char* cname, uint64_t mac_addr, void* device, 
                      bool interrupts, bool userspace, struct capref* ev,
                      struct capref* tx, struct capref* rx);

#endif
