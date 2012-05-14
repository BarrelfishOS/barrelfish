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

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "netg_ipv4.h"
#include "netg_ethernet.h"

static errval_t netg_node_process_ip(struct netg_buffer* buffer, struct netg_node* this)
{
	struct ip_header* hdr = buffer->data + HEADER_LENGTH_ETHERNET;
	hdr->version_header_length = IP_VERSION_HEADER_LENGTH(4, 5);
	hdr->tos = 0;
	// total length does not include the ethernet header
	hdr->total_length = ng_htons(buffer->header_length + buffer->length - HEADER_LENGTH_ETHERNET);
	hdr->identification = 0;
	hdr->flags_fragment_offset = ng_htons(0x4000);
	hdr->ttl = 0x40;
	hdr->protocol = (buffer->local_endpoint->type == NETG_ENDPOINT_TCP ? IP_PROTOCOL_TCP : IP_PROTOCOL_UDP);
	hdr->header_checksum = 0;
	hdr->source_ip_addr = buffer->local_endpoint->ip_addr;
	hdr->dest_ip_addr = buffer->remote_endpoint->ip_addr;

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_ip(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_ip;

	return node;
}

static errval_t netg_node_process_rx_ip(struct netg_buffer* buffer, struct netg_node* this)
{
	struct ip_header* hdr = buffer->payload;
	buffer->header_length += IP_HEADER_LENGTH(hdr->version_header_length)*4;
	// have to add ethernet header since it is not in the calculation of the IP length
	buffer->length = ng_ntohs(hdr->total_length) + HEADER_LENGTH_ETHERNET - buffer->header_length;

	buffer->payload = buffer->data + buffer->header_length;

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_rx_ip(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_rx_ip;

	return node;
}

static errval_t netg_node_process_ip_checksum(struct netg_buffer* buffer, struct netg_node* this)
{
	struct ip_header* hdr = buffer->data + HEADER_LENGTH_ETHERNET;
	hdr->header_checksum = ~calc_standard_chksum(hdr, 0, HEADER_LENGTH_IP);

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_ip_checksum(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_ip_checksum;

	return node;
}

static errval_t netg_node_process_check_ip_checksum(struct netg_buffer* buffer, struct netg_node* this)
{
	struct ip_header* hdr = buffer->payload;
	uint16_t cksm = ~calc_standard_chksum(hdr, 0, HEADER_LENGTH_IP);

	if (cksm == 0) {
		//debug_printf("ip checksum does match, continue\n");

		if(this->next!=NULL) {
			return this->next->process(buffer, this->next);
		}
	} else {
		debug_printf("ip header checksum failure\n");

		if (discard != NULL) {
			return discard->process(buffer, discard);
		}
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_check_ip_checksum(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_check_ip_checksum;

	return node;
}
