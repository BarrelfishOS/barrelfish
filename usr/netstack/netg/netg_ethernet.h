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

#ifndef _NETG_ETHERNET_H_
#define _NETG_ETHERNET_H_

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "netg_common.h"

#define HEADER_LENGTH_ETHERNET 14

#define ETHERNET_TYPE_IP (0x0008)

struct ethernet_header {
	uint64_t destination_mac : 48;
	uint64_t source_mac : 48;
	uint16_t type;
} __attribute__((packed));

struct netg_node* netg_node_create_ethernet(struct netg_node* next);

struct netg_node* netg_node_create_ARP_lookup(struct netg_node* next);

struct netg_node* netg_node_create_check_ethernet(struct netg_node* next);

#endif
