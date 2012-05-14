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

#ifndef _NETG_COMMON_H_
#define _NETG_COMMON_H_

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>

#include "netg.h"

//FIXME somehow refactor the netif out
#include <if/net_ports_rpcclient_defs.h>
#include <if/net_ARP_rpcclient_defs.h>


#include <barrelfish/net_constants.h>

struct netg_node
{
	errval_t (*process)(struct netg_buffer*, struct netg_node*);
	void* node_state;
	uint8_t next_count;
	struct netg_node* next;
};

struct netg_node_state_queue
{
	struct netg_buffer** queue;
	struct netg_node* node;
	uint16_t start;
	uint16_t end;
	uint16_t size;
	bool queue_runnable;
	errval_t (*process)(uint16_t, struct netg_node_state_queue*);

	struct netg_node_state_queue* next;
};



// structure for NIC specific data
struct netg_interface
{
	ip_addr_t ip;
	ip_addr_t gateway;
	ip_addr_t net_mask;
	mac_addr_t mac;

	net_interface_t net_interface;
	char card_name[MAX_NET_SERVICE_NAME_LEN];

	struct net_ports_rpc_client net_ports_rpc;
	bool net_ports_service_connected;
	struct net_ARP_rpc_client net_ARP_rpc;
	bool net_ARP_service_connected;

	struct netg_queue* queues;

	struct netg_interface* next;
};

struct netg_queue_binding
{
	struct net_queue_manager_binding* binding;
	uint64_t bufid;
	void* cont_queue;

	struct netg_queue* queue;
};

struct netg_queue
{
	uint64_t queue_id;
	struct netg_interface* netif;

	struct netg_queue_binding rx_binding;
	struct netg_queue_binding tx_binding;

	struct capref buffer_frame;
	void *buffer_base;
	size_t buffer_size;
	size_t buffer_count;

	struct netg_buffer* buffer_free_list;
	struct netg_buffer** buffers;

	struct netg_queue* next;
};

extern struct netg_endpoint* active_local_endpoints;
extern struct netg_endpoint* active_remote_endpoints;

extern struct netg_endpoint* timewait_endpoints;

extern struct netg_node_state_queue* runnable_queues;

extern struct netg_interface* active_interfaces;

extern struct netg_node* discard;

#define IP4_ADDR1(ipaddr) ((uint8_t)(ng_ntohl(ipaddr) >> 24) & 0xff)
#define IP4_ADDR2(ipaddr) ((uint8_t)(ng_ntohl(ipaddr) >> 16) & 0xff)
#define IP4_ADDR3(ipaddr) ((uint8_t)(ng_ntohl(ipaddr) >> 8) & 0xff)
#define IP4_ADDR4(ipaddr) ((uint8_t)(ng_ntohl(ipaddr)) & 0xff)

#define IP4_ADDR_PRINTF(ipaddr) IP4_ADDR1(ipaddr), IP4_ADDR2(ipaddr), IP4_ADDR3(ipaddr), IP4_ADDR4(ipaddr)


uint16_t calc_standard_chksum(void *dataptr, uint32_t start, uint16_t len);
uint16_t inet_chksum_pseudo(void *dataptr, ip_addr_t src, ip_addr_t dest, uint8_t proto, uint16_t proto_len);

struct netg_node* create_node(struct netg_node* parent);

// netg_endpoint helpers
struct netg_endpoint* netg_endpoint_lookup(struct netg_endpoint** list, uint16_t net_interface, enum netg_endpoint_type type, ip_addr_t ip_addr, uint16_t port);
void netg_endpoint_insert(struct netg_endpoint** list, struct netg_endpoint* endpoint);
void netg_endpoint_remove(struct netg_endpoint** list, struct netg_endpoint* endpoint);

// netg_queue_state helpers
struct netg_node_state_queue* netg_queue_state_init(uint16_t size,
				bool queue_runnable,
				struct netg_node* node,
				errval_t (*process)(uint16_t, struct netg_node_state_queue*));

void netg_queue_push(struct netg_node_state_queue* qst, struct netg_buffer* buffer);
void netg_queue_prepend(struct netg_node_state_queue* qst, struct netg_buffer* buffer);
struct netg_buffer* netg_queue_pop(struct netg_node_state_queue* qst);
struct netg_buffer* netg_queue_pop_last(struct netg_node_state_queue* qst);
struct netg_buffer* netg_queue_peek(struct netg_node_state_queue* qst);
struct netg_buffer* netg_queue_peek_last(struct netg_node_state_queue* qst);
uint16_t netg_queue_length(struct netg_node_state_queue* qst);


void netg_enqueue_runnable_queue(struct netg_node_state_queue* runnable);



struct netg_node* netg_node_create_classifier(struct netg_node* tcp, struct netg_node* udp);
struct netg_node* netg_node_create_discard_buffer(struct netg_node* next);

struct netg_node* netg_node_create_runnable_queue(struct netg_node* next);

struct netg_node* netg_node_create_timestamp(struct netg_node* next);
struct netg_node* netg_node_create_evaluate_timestamp(struct netg_node* next);

struct netg_node* netg_node_create_sendto_queue(uint64_t queue_id);
struct netg_node* netg_node_create_recvfrom_queue(struct netg_node* next, uint64_t queue_id);

void netg_schedule(void);

#endif
