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

#include "netg_ethernet.h"
#include "netg_common.h"
#include "ARP_lookup_client.h"
#include "idc_net_queue_manager.h"


static errval_t process_ethernet(struct netg_buffer* buffer, struct netg_node* this)
{
	//debug_printf("adding ethernet header\n");
	struct ethernet_header* hdr = buffer->data;
	hdr->destination_mac = buffer->remote_endpoint->mac_addr;
	hdr->source_mac      = buffer->local_endpoint->mac_addr;
	hdr->type            = ETHERNET_TYPE_IP;

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_ethernet(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_ethernet;

	return node;
}

static errval_t process_ARP_lookup(struct netg_buffer* buffer, struct netg_node* this)
{
	if (buffer->remote_endpoint->mac_addr <= 1) {
		debug_printf("next MAC address unknown, lookup\n");

		if (buffer->remote_endpoint->mac_addr == 0) {
			struct netg_interface* netif = get_queue_from_id(buffer->queue_id)->netif;
			ip_addr_t remote_ip = buffer->remote_endpoint->ip_addr;
			ip_addr_t nm = netif->net_mask;
			ip_addr_t local_ip = netif->ip;

			if (remote_ip == ng_htonl(IP4_ADDR_LOOPBACK)) {
				// LOOPBACK
				debug_printf("ETH: loopback\n");
				buffer->remote_endpoint->mac_addr =	buffer->local_endpoint->mac_addr;
			} else if ((local_ip&nm) == (remote_ip&nm)) {
				debug_printf("ETH: local address\n");
				buffer->remote_endpoint->mac_addr = idc_ARP_lookup(netif, buffer->remote_endpoint->ip_addr);
			} else {
				debug_printf("ETH: not local address, send it to gateway\n");
				buffer->remote_endpoint->mac_addr = idc_ARP_lookup(netif, netif->gateway);
			}

		}
	}

	if (buffer->remote_endpoint->mac_addr >= 1) {
		if (this->next!=NULL) {
			return this->next->process(buffer, this->next);
		}
	} else {
		// discard buffer, could not find MAC
		discard->process(buffer, discard);
		return NETG_ERR_MAC_NOT_FOUND;
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_ARP_lookup(struct netg_node* next)
{
	struct netg_node* node = create_node(next);
	node->process = process_ARP_lookup;

	return node;
}

static errval_t process_check_ethernet(struct netg_buffer* buffer, struct netg_node* this)
{
	struct ethernet_header* eth_hdr = buffer->payload;

	debug_printf("packet payload %lx data %lx\n", (uint64_t)buffer->payload, (uint64_t)buffer->data);

	if (eth_hdr->type == ETHERNET_TYPE_IP ) {
		buffer->payload = buffer->data + HEADER_LENGTH_ETHERNET;
		buffer->header_length = HEADER_LENGTH_ETHERNET;

		if (this->next!=NULL) {
			return this->next->process(buffer, this->next);
		}
	} else {
		debug_printf("ethernet frame is not an IP packet\n");
	}
	return SYS_ERR_OK;
}


struct netg_node* netg_node_create_check_ethernet(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_check_ethernet;

	return node;
}
