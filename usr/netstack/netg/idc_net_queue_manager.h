/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _IDC_NET_QUEUE_MANAGER_H_
#define _IDC_NET_QUEUE_MANAGER_H_

#include <stdlib.h>

#include <barrelfish/barrelfish.h>

#include "netg_common.h"

void net_rx_done(struct netg_queue* queue, size_t idx, size_t len);
void net_tx_done(struct netg_queue* queue, size_t idx);

void net_if_init(struct netg_interface* netif, uint64_t qid);
void net_if_terminate(struct netg_queue* queue);
void buffer_tx_add(struct netg_queue* queue, size_t idx, size_t offset, size_t len);
void buffer_rx_add(struct netg_queue* queue, size_t idx);

struct netg_queue* get_queue_from_id(uint64_t queue_id);

#endif

