/*
 * Copyright (c) 2007-11 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ETHERSRV_LOCAL_H_
#define ETHERSRV_LOCAL_H_
#include <ethersrv/ethersrv.h>

// registered buffers:
struct buffer_descriptor *buffers_list;

/* NETD connections */
#define NETD_BUF_NR 2
struct ether_binding *netd[NETD_BUF_NR];

// Measurement purpose, counting interrupt numbers
extern uint64_t interrupt_counter;
extern uint64_t interrupt_loop_counter;

// Function prototypes for ether services
struct buffer_descriptor *find_buffer(uint64_t buffer_id);

// Function prototypes for ether_control service
void init_ether_control_service(char *service_name);

#endif // ETHERSRV_LOCAL_H_
