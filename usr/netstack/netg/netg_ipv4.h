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

#ifndef _NETG_IPV4_H_
#define _NETG_IPV4_H_

#include <stdio.h>
#include <string.h>
#include <errno.h>
//#include <unistd.h>
//#include <barrelfish/barrelfish.h>

#include "netg_common.h"

#define HEADER_LENGTH_IP 20

struct ip_header {
	uint8_t  version_header_length;
	uint8_t  tos;
	uint16_t total_length;
	uint16_t identification;
	uint16_t flags_fragment_offset;
	uint8_t  ttl;
	uint8_t  protocol;
	uint16_t header_checksum;
	uint32_t source_ip_addr;
	uint32_t dest_ip_addr;
} __attribute__((packed));

#define IP_VERSION(version_header_length) (version_header_length >> 4)
#define IP_HEADER_LENGTH(version_header_length) (version_header_length % 16)
#define IP_VERSION_HEADER_LENGTH(version, header_length) ((version << 4) + header_length)

#define IP_PROTOCOL_TCP 6
#define IP_PROTOCOL_UDP 17

struct netg_node* netg_node_create_ip(struct netg_node* next);
struct netg_node* netg_node_create_rx_ip(struct netg_node* next);
struct netg_node* netg_node_create_ip_checksum(struct netg_node* next);
struct netg_node* netg_node_create_check_ip_checksum(struct netg_node* next);

#endif
