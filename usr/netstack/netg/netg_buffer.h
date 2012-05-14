/** \file
 *  \brief Example lwip socket application
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _NETG_BUFFER_H_
#define _NETG_BUFFER_H_

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>

#include "netg_common.h"

void netg_init_buffers(struct netg_queue* queue);
struct netg_buffer* netg_get_tx_buffer(uint64_t queue_id);
struct netg_buffer* netg_get_rx_buffer(struct netg_queue* queue, uint64_t idx);

#endif
