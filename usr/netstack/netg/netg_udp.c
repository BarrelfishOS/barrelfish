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

#include "netg_udp.h"


void debug_print_udp_header(struct udp_header* header) {
	debug_printf("udp_header: source port %hu dest port %hu length %hu checksum %hx\n", header->source_port, header->dest_port, header->udp_length, header->udp_checksum);
}

static errval_t netg_node_process_udp(struct netg_buffer* buffer, struct netg_node* this)
{
	//debug_printf("packet payload %lx data %lx\n", (uint64_t)buffer->payload, (uint64_t)buffer->data);

	struct udp_header* hdr = buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP;
	hdr->dest_port = buffer->remote_endpoint->port;
	hdr->source_port = buffer->local_endpoint->port;
	hdr->udp_length = ng_htons(buffer->length + HEADER_LENGTH_UDP);
	hdr->udp_checksum = 0;

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tx_udp(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_udp;

	return node;
}

static errval_t netg_node_process_rx_udp(struct netg_buffer* buffer, struct netg_node* this)
{
	//debug_printf("packet payload %lx data %lx\n", (uint64_t)buffer->payload, (uint64_t)buffer->data);

	struct udp_header* hdr = buffer->payload;

	buffer->header_length += HEADER_LENGTH_UDP;
	buffer->length = ng_ntohs(hdr->udp_length) - HEADER_LENGTH_UDP;

	buffer->payload = buffer->data + buffer->header_length;

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_rx_udp(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_rx_udp;

	return node;
}

static errval_t netg_node_process_udp_checksum(struct netg_buffer* buffer, struct netg_node* this)
{
	struct udp_header* hdr = buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP;

	hdr->udp_checksum = inet_chksum_pseudo(buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP,
											buffer->local_endpoint->ip_addr,
											buffer->remote_endpoint->ip_addr,
											IP_PROTOCOL_UDP,
											HEADER_LENGTH_UDP + buffer->length);

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tx_udp_checksum(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_udp_checksum;

	return node;
}

static errval_t netg_node_process_check_udp_checksum(struct netg_buffer* buffer, struct netg_node* this)
{
	struct udp_header* hdr = buffer->payload;
	if (hdr->udp_checksum != 0) {
		struct ip_header* ip_hdr = buffer->data + HEADER_LENGTH_ETHERNET;

		uint16_t chks = inet_chksum_pseudo(buffer->payload,
													ip_hdr->source_ip_addr,
													ip_hdr->dest_ip_addr,
													IP_PROTOCOL_UDP,
													buffer->length);

		if ( chks!=0 ) {
			debug_printf("UDP: checksum MISMATCHES, discarding packet\n");
			return discard->process(buffer, discard);
		}
	} else {
		debug_printf("UDP: checksum disabled for this packet\n");
	}

	if (this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_rx_udp_checksum(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_check_udp_checksum;

	return node;
}

static errval_t netg_node_process_udp_recv(struct netg_buffer* buffer, struct netg_node* this)
{
	//debug_printf("UDP: local port %hu remote port %hu\n", ng_ntohs(buffer->local_endpoint->port), ng_ntohs(buffer->remote_endpoint->port));

	if(buffer->local_endpoint->recv != NULL) {
		buffer->local_endpoint->recv(buffer->local_endpoint->recv_arg, buffer->local_endpoint, buffer, ng_ntohl(buffer->remote_endpoint->ip_addr), ng_ntohs(buffer->remote_endpoint->port));
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_udp_recv(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_udp_recv;

	return node;
}

static errval_t netg_node_process_udp_endpoint_classifier(struct netg_buffer* buffer, struct netg_node* this) {
	struct ip_header* ip_hdr = buffer->data + HEADER_LENGTH_ETHERNET;
	struct netg_endpoint* it_local = active_local_endpoints;
	struct netg_endpoint* it_remote = active_remote_endpoints;
	struct netg_endpoint* nept = NULL;
	struct netg_endpoint* source = NULL;

	struct udp_header* udp_hdr = buffer->payload;

	while (it_local != NULL) {
		if (it_local->type == NETG_ENDPOINT_UDP) {
			if (it_local->ip_addr == ip_hdr->dest_ip_addr && it_local->port == udp_hdr->dest_port) {
				nept = it_local;
				break;
			} else {
				debug_printf("UDP: packet %x %hu endpoint %x %hu \n", ip_hdr->dest_ip_addr, udp_hdr->dest_port, it_local->ip_addr, it_local->port);
			}
		}
		it_local = it_local->next;
	}

	if (nept != NULL && nept->status == NETG_ENDPOINT_CONNECTED) {
		source = (struct netg_endpoint*)nept->transport;

		if (source->ip_addr == ip_hdr->source_ip_addr && source->port == udp_hdr->source_port) {
			debug_printf("UDP: receive from connected udp endpoint, continue\n");
		} else {
			debug_printf("UDP: receive from unknown endpoint on a connected udp endpoint, dropping\n");
			nept = NULL;
			source = NULL;
		}
	}

	if (nept != NULL && source == NULL) {
		//debug_printf("UDP: local endpoint found, looking for remote\n");
		while (it_remote != NULL && source == NULL) {
			if (it_remote->type == NETG_ENDPOINT_UDP) {
				if (it_remote->ip_addr == ip_hdr->source_ip_addr && it_remote->port == udp_hdr->source_port) {
					source = it_remote;
				} else {
					//debug_printf("UDP: packet %x %hu endpoint %x %hu \n", ip_hdr->dest_ip_addr, udp_hdr->dest_port, it_remote->ip_addr, it_remote->port);
				}
			}
			it_remote = it_remote->next;
		}

		if (source == NULL) {
			debug_printf("UDP: creating new remote endpoint in the classifier\n");
			source = malloc(sizeof(struct netg_endpoint));
			source->type = NETG_ENDPOINT_UDP;
			source->next = NULL;
			source->net_interface = nept->net_interface;

			source->mac_addr = 0;
			source->ip_addr = ip_hdr->source_ip_addr;
			source->port = udp_hdr->source_port;

			source->status = NETG_ENDPOINT_CLOSED;
			source->recv = NULL;
			source->recv_arg = NULL;

			// insert into active endpoints list
			netg_endpoint_insert(&active_remote_endpoints, source);
		}
	}

	if(nept != NULL) {
		buffer->local_endpoint = nept;
		buffer->remote_endpoint = source;
		if(this->next!=NULL) {
			return this->next->process(buffer, this->next);
		}
	} else {
		debug_printf("UDP: could not find a matching endpoint\n");
		debug_printf("UDP: dest ip %u.%u.%u.%u\n", IP4_ADDR1(ip_hdr->dest_ip_addr), IP4_ADDR2(ip_hdr->dest_ip_addr), IP4_ADDR3(ip_hdr->dest_ip_addr), IP4_ADDR4(ip_hdr->dest_ip_addr));
		debug_printf("UDP: source ip %u.%u.%u.%u\n", IP4_ADDR1(ip_hdr->source_ip_addr), IP4_ADDR2(ip_hdr->source_ip_addr), IP4_ADDR3(ip_hdr->source_ip_addr), IP4_ADDR4(ip_hdr->source_ip_addr));
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_udp_endpoint_classifier(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_udp_endpoint_classifier;

	return node;
}
