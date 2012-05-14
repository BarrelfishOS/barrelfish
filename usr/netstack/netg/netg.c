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

#include "netg.h"

#include <assert.h>
#include <timer/timer.h>

#include "netg_tcp.h"
#include "netg_common.h"
#include "netg_udp.h"
#include "netg_ipv4.h"
#include "netg_ethernet.h"
#include "netg_debug.h"
#include "netg_buffer.h"

#include "ARP_lookup_client.h"
#include "idc_net_control.h"
#include "idc_net_queue_manager.h"

/**
 * Convert an u16_t from host- to network byte order.
 *
 * @param n u16_t in host byte order
 * @return n in network byte order
 */
uint16_t ng_htons(uint16_t n)
{
    return ((n & 0xff) << 8) | ((n & 0xff00) >> 8);
}

/**
 * Convert an u16_t from network- to host byte order.
 *
 * @param n u16_t in network byte order
 * @return n in host byte order
 */
uint16_t ng_ntohs(uint16_t n)
{
    return ng_htons(n);
}

/**
 * Convert an u32_t from host- to network byte order.
 *
 * @param n u32_t in host byte order
 * @return n in network byte order
 */
uint32_t ng_htonl(uint32_t n)
{
    return ((n & 0xff) << 24) |
      ((n & 0xff00) << 8) |
      ((n & 0xff0000UL) >> 8) | ((n & 0xff000000UL) >> 24);
}

/**
 * Convert an u32_t from network- to host byte order.
 *
 * @param n u32_t in network byte order
 * @return n in host byte order
 */
uint32_t ng_ntohl(uint32_t n)
{
    return ng_htonl(n);
}

struct netg_buffer* netg_get_buffer(struct netg_endpoint* endpoint)
{
	debug_printf("NETG: getting a buffer\n");
	struct netg_buffer* buf = netg_get_tx_buffer(active_interfaces->queues->queue_id);

	//FIXME remove
	assert(buf!=NULL);

	if (buf != NULL) {
		buf->local_endpoint = endpoint;

		if (endpoint->type == NETG_ENDPOINT_UDP) {
			buf->header_length = HEADER_LENGTH_UDP_COMPLETE;
		} else if (endpoint->type == NETG_ENDPOINT_TCP) {
			buf->header_length = HEADER_LENGTH_TCP_COMPLETE;
		}

		buf->length = 0;
		buf->next = NULL;
		buf->payload = buf->data + buf->header_length;
		buf->transport_flags = 0;
		buf->ref_count = 1;
		buf->timestamp = 0;
	}

	return buf;
}

void netg_free_buffer(struct netg_buffer* buffer)
{
	if (--buffer->ref_count == 0) {

	}
}


void netg_claim_buffer(struct netg_buffer* buffer)
{
	buffer->ref_count++;

	debug_printf("NETG: %u are referencing this buffer now\n", buffer->ref_count);
}

static struct netg_node* udp_start = NULL;

static struct netg_node* tcp_start = NULL;
static struct netg_node* tcp_connect_start = NULL;

static struct timer* slow_timer = NULL;

static void slow_timer_callback(struct timer *timer, void *data)
{
	debug_printf("slow_timer...\n");
	netg_tcp_timer();
}


static void init_tree(void)
{
	discard = netg_node_create_discard_buffer(NULL);

	// SEND
	struct netg_node* end = netg_node_create_sendto_queue(0);
	//struct netg_node* dump_send = netg_node_create_dump_buffer(end);
	struct netg_node* eth = netg_node_create_ethernet(end);
	struct netg_node* arp    = netg_node_create_ARP_lookup(eth);
	struct netg_node* ip_chks = netg_node_create_ip_checksum(arp);
	struct netg_node* ip = netg_node_create_ip(ip_chks);

	struct netg_node* udp_chks = netg_node_create_tx_udp_checksum(ip);
	struct netg_node* udp = netg_node_create_tx_udp(udp_chks);

	struct netg_node* tcp_chks = netg_node_create_tcp_tx_checksum(ip);
	struct netg_node* tcp_tx_update = netg_node_create_tcp_tx_update_state(tcp_chks);
	struct netg_node* tcp_retrans = netg_node_create_tcp_tx_retransmit(tcp_tx_update);
	struct netg_node* tcp_tx_check = netg_node_create_tcp_tx_check_window(tcp_retrans);
	struct netg_node* tcp_tx = netg_node_create_tcp_tx(tcp_tx_check);

	tcp_start = tcp_tx;
	udp_start = udp;

	tcp_connect_start = tcp_retrans;

	// RECV
	struct netg_node* recv = netg_node_create_udp_recv(NULL);
	struct netg_node* udp_tx = netg_node_create_evaluate_timestamp(recv);
	//struct netg_node* print = netg_node_create_print(udp_tx);
	struct netg_node* rx_udp = netg_node_create_rx_udp(udp_tx);
	struct netg_node* udp_class = netg_node_create_udp_endpoint_classifier(rx_udp);
	struct netg_node* check_udp = netg_node_create_rx_udp_checksum(udp_class);


	struct netg_node* tcp_rcv = netg_node_create_tcp_recv(NULL);
	struct netg_node* tcp_ts = netg_node_create_evaluate_timestamp(tcp_rcv);
	struct netg_node* send_ack = netg_node_create_tcp_rx_ack_new_data(tcp_ts, tcp_start);
	struct netg_node* tcp_reorder = netg_node_create_tcp_rx_reorder(send_ack);
	struct netg_node* tcp_update_con = netg_node_create_tcp_rx_update_connection_info(tcp_reorder, tcp_start);
	struct netg_node* tcp_update = netg_node_create_tcp_rx_update_state(tcp_update_con, tcp_start);
	struct netg_node* listen = netg_node_create_tcp_rx_listen(tcp_connect_start);
	struct netg_node* tcp_class = netg_node_create_tcp_rx_classifier(listen, tcp_update);
	struct netg_node* tcp_rx_cks = netg_node_create_tcp_rx_checksum(tcp_class);


	struct netg_node* class = netg_node_create_classifier(tcp_rx_cks, check_udp);
	struct netg_node* rx_ip = netg_node_create_rx_ip(class);
	struct netg_node* check_ip = netg_node_create_check_ip_checksum(rx_ip);
	struct netg_node* eth_r = netg_node_create_check_ethernet(check_ip);
	//struct netg_node* dump = netg_node_create_dump_buffer(eth_r);
	struct netg_node* ts = netg_node_create_timestamp(eth_r);
	netg_node_create_recvfrom_queue(ts, 0);


	// LOOPBACK
	//struct netg_node* q_1 = netg_node_create_runnable_queue(ts);
	//struct netg_node* dump = netg_node_create_dump_buffer(q_1);
	//struct netg_node* drop = netg_node_create_packet_loss(q_1, 1);
	//struct netg_node* lo = netg_node_create_loopback(drop);
	//eth->next = ts;
}

static net_interface_t netif_id = 0;

static errval_t init_netif(const char* card_name) {
	errval_t err = SYS_ERR_OK;

	struct netg_interface* netif = malloc(sizeof(struct netg_interface));
	snprintf(netif->card_name, sizeof(netif->card_name), "%s", card_name);
	netif->next = active_interfaces;
	active_interfaces = netif;

	char port_manager_name[MAX_NET_SERVICE_NAME_LEN];
	snprintf(port_manager_name, sizeof(port_manager_name), "%s%s", card_name, NET_PORTS_MNG_SUFFIX);
	idc_connect_port_manager_service(netif, port_manager_name);

	char ARP_service_name[MAX_NET_SERVICE_NAME_LEN];
	snprintf(ARP_service_name, sizeof(ARP_service_name), "%s%s", card_name, NET_ARP_LOOKUP_SUFFIX);
	//idc_connect_ARP_lookup_service(netif, ARP_service_name);

	//FIXME which QUEUE is it?
	net_if_init(netif, 0);

	ip_addr_t ip, gw, nm;
	/*
	err = idc_get_ip_from_ARP_lookup(netif, &ip, &gw, &nm);
	if (err_is_fail(err)) {
		printf("idc_get_ip_from_ARP_lookup failed\n");
		USER_PANIC_ERR(err, "idc_get_ip_from_ARP_lookup failed");
	}*/
	ip = gw = ng_htonl(IP4_ADDR_LOOPBACK);
	nm = 0xFFFFFF;

	debug_printf("got ip %u.%u.%u.%u\n", IP4_ADDR_PRINTF(ip));
	debug_printf("got gw %u.%u.%u.%u\n", IP4_ADDR1(gw), IP4_ADDR2(gw), IP4_ADDR3(gw), IP4_ADDR4(gw));
	debug_printf("got nm %u.%u.%u.%u\n", IP4_ADDR1(nm), IP4_ADDR2(nm), IP4_ADDR3(nm), IP4_ADDR4(nm));

	netg_init_buffers(netif->queues);

	netif->ip = ip;
	netif->gateway = gw;
	netif->net_mask = nm;
	netif->net_interface = netif_id;
	netif_id++;

	return err;
}

static bool basic_initialized = false;

void netg_init(const char* card_name)
{
	if (!basic_initialized) {
		debug_printf("netg_init for the FIRST TIME...\n");

		errval_t err = timer_init();
		if (err_is_fail(err)) {
			printf("timer_init failed\n");
			USER_PANIC_ERR(err, "timer_init failed");
		}

		slow_timer = NULL;

		init_tree();

		basic_initialized = true;
	}

	init_netif(card_name);

	debug_printf("netg_init finished initializing netgraph...\n");
}

static struct netg_interface* get_netif_from_ip(ip_addr_t ip_addr) {
	struct netg_interface* it = active_interfaces;
	while (it != NULL) {
		debug_printf("got %u.%u.%u.%u looking for %u.%u.%u.%u\n", IP4_ADDR_PRINTF(it->ip), IP4_ADDR_PRINTF(ip_addr));
		if(it->ip == ip_addr) {
			return it;
		}
		it = it->next;
	}
	return NULL;
}

struct netg_endpoint *udp_new(void)
{
	struct netg_endpoint* ept = malloc(sizeof(struct netg_endpoint));
	ept->type = NETG_ENDPOINT_UDP;
	ept->next = NULL;

	ept->mac_addr = 0;

	ept->status = NETG_ENDPOINT_CLOSED;
	ept->recv = NULL;
	ept->recv_arg = NULL;

	// insert into active endpoints list
	netg_endpoint_insert(&active_local_endpoints, ept);

	return ept;
}

void udp_remove(struct netg_endpoint* endpoint)
{
	assert(endpoint->type == NETG_ENDPOINT_UDP);
	struct netg_endpoint* remote = (struct netg_endpoint*)endpoint->transport;

	if(remote != NULL) {
		netg_endpoint_remove(&active_remote_endpoints, remote);
	}
	netg_endpoint_remove(&active_local_endpoints, endpoint);
}

errval_t udp_bind(struct netg_endpoint* endpoint, ip_addr_t ipaddr, uint16_t port) {
	endpoint->port    = ng_htons(port);
	struct netg_interface* iface = NULL;

	if (ipaddr > 0) {
		iface = get_netif_from_ip(ng_htonl(ipaddr));
	} else {
		//FIXME register on all netifs
		iface = active_interfaces;
	}

	if (iface == NULL) {
		return NETG_ERR_NOT_BOUND;
	}

	endpoint->mac_addr = iface->mac;
	endpoint->net_interface   = iface->net_interface;
	endpoint->ip_addr = iface->ip;

	idc_bind_udp_port(active_interfaces, port, iface->queues->queue_id);

	return SYS_ERR_OK;
}

errval_t udp_connect(struct netg_endpoint* endpoint, ip_addr_t ipaddr, uint16_t port)
{
	struct netg_endpoint* ept = netg_endpoint_lookup(&active_remote_endpoints, endpoint->net_interface, NETG_ENDPOINT_UDP, ng_htonl(ipaddr), ng_htons(port));
	if (ept == NULL) {
		debug_printf("creating new remote endpoint in the udp_connect\n");
		ept = malloc(sizeof(struct netg_endpoint));
		ept->type = NETG_ENDPOINT_UDP;
		ept->next = NULL;
		ept->net_interface = endpoint->net_interface;

		ept->mac_addr = 0;
		ept->ip_addr = ng_htonl(ipaddr);
		ept->port = ng_htons(port);

		ept->status = NETG_ENDPOINT_CONNECTED;
		ept->recv = NULL;
		ept->recv_arg = NULL;

		// insert into active endpoints list
		netg_endpoint_insert(&active_remote_endpoints, ept);
	}


	endpoint->transport = ept;
	ept->transport = endpoint;

	endpoint->status = NETG_ENDPOINT_CONNECTED;
	return SYS_ERR_OK;
}

void udp_disconnect(struct netg_endpoint* endpoint)
{
	endpoint->status = NETG_ENDPOINT_CLOSED;

	struct netg_endpoint* remote = (struct netg_endpoint*)endpoint->transport;
	remote->transport = NULL;
	endpoint->transport = NULL;
}

void udp_recv(struct netg_endpoint* endpoint,
			  void (*recv) (void *arg, struct netg_endpoint* uendpoint, struct netg_buffer* p, ip_addr_t addr, uint16_t port),
			  void *recv_arg)
{
	endpoint->recv = recv;
	endpoint->recv_arg = recv_arg;
}

errval_t udp_sendto(struct netg_endpoint* endpoint, struct netg_buffer *p, ip_addr_t dst_ip, uint16_t dst_port)
{
	p->local_endpoint = endpoint;
	p->remote_endpoint = netg_endpoint_lookup(&active_remote_endpoints, endpoint->net_interface, NETG_ENDPOINT_UDP, ng_htonl(dst_ip), ng_htons(dst_port));

	if (p->remote_endpoint == NULL) {
		debug_printf("creating new remote endpoint in the udp_sendto\n");
		p->remote_endpoint = malloc(sizeof(struct netg_endpoint));
		p->remote_endpoint->type = NETG_ENDPOINT_UDP;
		p->remote_endpoint->next = NULL;
		p->remote_endpoint->net_interface = endpoint->net_interface;

		p->remote_endpoint->mac_addr = 0;
		p->remote_endpoint->ip_addr = ng_htonl(dst_ip);
		p->remote_endpoint->port = ng_htons(dst_port);

		p->remote_endpoint->status = NETG_ENDPOINT_CLOSED;
		p->remote_endpoint->recv = NULL;
		p->remote_endpoint->recv_arg = NULL;

			// insert into active endpoints list
		netg_endpoint_insert(&active_remote_endpoints, p->remote_endpoint);
	}

	return udp_start->process(p, udp_start);

}

errval_t udp_send(struct netg_endpoint* endpoint, struct netg_buffer *p)
{
	if (endpoint->status == NETG_ENDPOINT_CONNECTED) {
		p->local_endpoint = endpoint;
		p->remote_endpoint = (struct netg_endpoint*)endpoint->transport;
		return udp_start->process(p, udp_start);
	} else {
		return NETG_ERR_NOT_CONNECTED;
	}
}


struct netg_endpoint *tcp_new(void) {
	if(slow_timer == NULL) {
		slow_timer = timer_create(500000, true, slow_timer_callback, NULL);
		timer_start(slow_timer);
	}

	struct netg_endpoint* ept = malloc(sizeof(struct netg_endpoint));
	ept->type = NETG_ENDPOINT_TCP;
	ept->next = NULL;

	ept->mac_addr = 0;


	struct netg_tcp_state* tcp_state = netg_tcp_create_state(ept);
	ept->transport = tcp_state;
	if (ept->transport == NULL) {
		return NULL;
	}

	ept->status = NETG_ENDPOINT_CLOSED;
	ept->recv = NULL;
	ept->recv_arg = NULL;

	// insert into active endpoints list
	netg_endpoint_insert(&active_local_endpoints, ept);

	return ept;
}

errval_t tcp_bind(struct netg_endpoint* endpoint, ip_addr_t ipaddr, uint16_t port)
{
	endpoint->port    = ng_htons(port);
	struct netg_interface* iface = NULL;

	if (ipaddr > 0) {
		iface = get_netif_from_ip(ng_htonl(ipaddr));
	} else {
		//FIXME register on all netifs
		iface = active_interfaces;
	}

	if (iface == NULL) {
		return NETG_ERR_NOT_BOUND;
	}

	endpoint->mac_addr = iface->mac;
	endpoint->net_interface   = iface->net_interface;
	endpoint->ip_addr = iface->ip;

	idc_bind_tcp_port(active_interfaces, port, iface->queues->queue_id);


	return SYS_ERR_OK;
}

errval_t tcp_listen(struct netg_endpoint* endpoint)
{
	// endpoint bound?
	if (endpoint->mac_addr == 0) {
		return NETG_ERR_NOT_BOUND;
	}

	endpoint->status = NETG_ENDPOINT_LISTEN;

	struct netg_tcp_state* ts = (struct netg_tcp_state*) endpoint->transport;

	assert(ts != NULL);

	ts->state = NETG_TCP_LISTEN;
	ts->remote = NULL;

	return SYS_ERR_OK;
}


void tcp_accept(struct netg_endpoint* endpoint, errval_t(*accept) (void *arg, struct netg_endpoint* endpoint, errval_t err))
{
	struct netg_tcp_state* state = (struct netg_tcp_state*) endpoint->transport;

	assert(state != NULL);

	state->accept = accept;
}

errval_t tcp_connect(struct netg_endpoint* endpoint, ip_addr_t ipaddr, uint16_t port,
		errval_t(*connected) (void *arg, struct netg_endpoint* endpoint, errval_t err))
{

	// endpoint bound?
	if (endpoint->mac_addr == 0) {
		return NETG_ERR_NOT_BOUND;
	}

	struct netg_endpoint* ept = malloc(sizeof(struct netg_endpoint));
	ept->type = NETG_ENDPOINT_TCP;
	ept->next = NULL;

	ept->mac_addr = 0;

	ept->status = NETG_ENDPOINT_CLOSED;
	ept->recv = NULL;
	ept->recv_arg = NULL;

	ept->ip_addr = ng_htonl(ipaddr);
	ept->port    = ng_htons(port);

	ept->mac_addr = 0;
	ept->net_interface   = endpoint->net_interface;

	struct netg_tcp_state* state = (struct netg_tcp_state*) endpoint->transport;
	assert(state != NULL);

	state->remote = ept;
	state->connected = connected;
	ept->transport = state;

	// insert into active endpoints list
	netg_endpoint_insert(&active_remote_endpoints, ept);

	return netg_tcp_connect(endpoint, ept, tcp_connect_start);
}

void tcp_abandon(struct netg_endpoint* endpoint, int reset)
{

	/*if (active_local_endpoints != NULL) {
		struct netg_endpoint* it = active_local_endpoints;
		struct netg_endpoint** old = &active_local_endpoints;

		while (it != NULL) {
			if (it == endpoint) {
				debug_printf("removing endpoint\n");
				*old = it->next;
			} else {
				old = &it->next;
			}

			it = it->next;
		}
	}*/
}

errval_t tcp_close(struct netg_endpoint* endpoint)
{
	debug_printf("TCP: tcp_close called\n");
	return netg_tcp_close(endpoint, tcp_start);
}

errval_t tcp_write(struct netg_endpoint* endpoint, const void *dataptr, uint16_t len, uint8_t apiflags)
{
	debug_printf("TCP: tcp_write called\n");
	return netg_tcp_enqueue(endpoint, dataptr, len, tcp_start);
}

void tcp_recv(struct netg_endpoint* endpoint,
		errval_t(*recv) (void *arg, struct netg_endpoint* endpoint, struct netg_buffer * p, errval_t err))
{
	struct netg_tcp_state* state = (struct netg_tcp_state*) endpoint->transport;

	assert(state != NULL);

	state->recv = recv;
}

void netg_tick(void)
{
	netg_schedule();
}


void print_tcp_status(struct netg_endpoint* endpoint)
{
	struct netg_tcp_state* tstate = endpoint->transport;
	assert(tstate!=NULL);

	debug_printf("TCP STATUS: %hu, check_window %hu retransmit %hu reorder %hu\n",
			ng_htons(endpoint->port),
			netg_queue_length(tstate->unsent_queue),
			netg_queue_length(tstate->retransmit_queue),
			netg_queue_length(tstate->reorder_queue));
}
