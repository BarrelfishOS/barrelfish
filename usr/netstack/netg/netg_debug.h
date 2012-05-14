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

#ifndef _NETG_DEBUG_H_
#define _NETG_DEBUG_H_

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>

#include "netg_common.h"

struct netg_node* netg_node_create_dummy(struct netg_node* next);
struct netg_node* netg_node_create_print(struct netg_node* next);
struct netg_node* netg_node_create_debug_buffer(struct netg_node* next);
struct netg_node* netg_node_create_dump_buffer(struct netg_node* next);

struct netg_node* netg_node_create_loopback(struct netg_node* next);
struct netg_node* netg_node_create_packet_loss(struct netg_node* next, uint8_t count);

#endif
