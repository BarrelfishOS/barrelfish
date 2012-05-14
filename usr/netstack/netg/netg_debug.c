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

#include "netg_common.h"
#include "netg_udp.h"
#include "netg_ipv4.h"
#include "netg_ethernet.h"
#include "netg_debug.h"


static errval_t netg_node_process_dummy(struct netg_buffer* buffer, struct netg_node* this)
{
	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

__attribute__((used))
struct netg_node* netg_node_create_dummy(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_dummy;

	return node;
}

static errval_t netg_node_process_print(struct netg_buffer* buffer, struct netg_node* this)
{
	debug_printf("buffer for endpoint %s %hu with length %hu passing through\n",
					buffer->local_endpoint->type == NETG_ENDPOINT_TCP ? "TCP": "UDP",
					ng_ntohs(buffer->local_endpoint->port),
					buffer->length);

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_print(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_print;

	return node;
}

static errval_t netg_node_process_debug_buffer(struct netg_buffer* buffer, struct netg_node* this)
{
	debug_printf("debuging buffer for endpoint %s %hu with length %hu\n",
					buffer->local_endpoint->type == NETG_ENDPOINT_TCP ? "TCP": "UDP",
					buffer->local_endpoint->port,
					buffer->length);
	struct ethernet_header* eth_hdr = buffer->data;
	struct ip_header* ip_hdr = buffer->data + HEADER_LENGTH_ETHERNET;

	debug_printf("ethernet from %lx to %lx\n", (uint64_t)eth_hdr->source_mac, (uint64_t)eth_hdr->destination_mac);
	debug_printf("ip header length %hu total length %hu\n", IP_HEADER_LENGTH(ip_hdr->version_header_length), ng_ntohs(ip_hdr->total_length));
	debug_printf("from %x to %x\n", ip_hdr->source_ip_addr, ip_hdr->dest_ip_addr);

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_debug_buffer(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_debug_buffer;

	return node;
}

static errval_t netg_node_process_dump_buffer(struct netg_buffer* buffer, struct netg_node* this)
{
	debug_printf("hex dump of the buffer\n");
	for(int i=0;i<(buffer->header_length + buffer->length); i += 4) {
		printf("%02x %02x %02x %02x\n",  *(((uint8_t*)buffer->data) +i),
				*(((uint8_t*)buffer->data) +i +1),
				*(((uint8_t*)buffer->data) +i +2),
				*(((uint8_t*)buffer->data) +i +3));
	}

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_dump_buffer(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_dump_buffer;

	return node;
}

static errval_t netg_node_process_loopback(struct netg_buffer* buffer, struct netg_node* this) {
	//debug_printf("loopback a buffer for debugging\n");

	struct netg_buffer* lpbuf = netg_get_buffer(buffer->remote_endpoint);

	lpbuf->local_endpoint = NULL;
	lpbuf->remote_endpoint = NULL;
	lpbuf->header_length = 0;
	lpbuf->length = 0;

	//debug_printf("creating new buffer with header length %hu and data length %hu\n", buffer->header_length, buffer->length);
	memcpy(lpbuf->data, buffer->data, buffer->header_length + buffer->length);
	lpbuf->payload = lpbuf->data;

	netg_free_buffer(buffer);

	if (this->next!=NULL) {
		return this->next->process(lpbuf, this->next);
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_loopback(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_loopback;

	return node;
}

struct packet_loss_state {
	uint8_t count;
	uint8_t drop_every;
};

static errval_t process_packet_loss(struct netg_buffer* buffer, struct netg_node* this) {
	struct packet_loss_state* state = (struct packet_loss_state*) this->node_state;

	state->count++;

	if(state->count % state->drop_every != 0 || state->drop_every == 1) {
		if (this->next!=NULL) {
			return this->next->process(buffer, this->next);
		}
	} else {
		debug_printf("DEBUG: DROPPING PACKET..................\n");
		return discard->process(buffer, discard);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_packet_loss(struct netg_node* next, uint8_t count) {
	struct netg_node* node = create_node(next);
	node->process = process_packet_loss;

	struct packet_loss_state* state = malloc(sizeof(struct packet_loss_state));
	state->count = 0;
	state->drop_every = count;
	node->node_state = state;

	return node;
}
