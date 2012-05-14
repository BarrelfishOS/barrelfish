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

#ifndef _NETG_UDP_H_
#define _NETG_UDP_H_

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>

#include "netg_common.h"
#include "netg_ipv4.h"
#include "netg_ethernet.h"


#define HEADER_LENGTH_UDP 8

#define HEADER_LENGTH_UDP_COMPLETE (HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP + HEADER_LENGTH_UDP	)

struct udp_header {
	uint16_t source_port;
	uint16_t dest_port;
	uint16_t udp_length;
	uint16_t udp_checksum;
} __attribute__((packed));

void debug_print_udp_header(struct udp_header* header);

struct netg_node* netg_node_create_tx_udp(struct netg_node* next);
struct netg_node* netg_node_create_rx_udp(struct netg_node* next);

struct netg_node* netg_node_create_tx_udp_checksum(struct netg_node* next);
struct netg_node* netg_node_create_rx_udp_checksum(struct netg_node* next);

struct netg_node* netg_node_create_udp_recv(struct netg_node* next);

struct netg_node* netg_node_create_udp_endpoint_classifier(struct netg_node* next);

#endif
