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

#ifndef _NETG_H_
#define _NETG_H_

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>

#define IP4_ADDR_LOOPBACK 0x7f000001

enum netg_endpoint_type {
	NETG_ENDPOINT_UDP,
	NETG_ENDPOINT_TCP
};

enum netg_endpoint_status {
	NETG_ENDPOINT_CLOSED,
	NETG_ENDPOINT_LISTEN,
	NETG_ENDPOINT_CONNECTED
};

typedef uint32_t ip_addr_t;
typedef uint64_t mac_addr_t;
typedef uint32_t net_interface_t;

struct netg_endpoint;

struct netg_buffer
{
	struct netg_endpoint* local_endpoint;
	struct netg_endpoint* remote_endpoint;
	uint64_t queue_id;
	void* data;
	void* payload;
	uint16_t length;
	uint16_t header_length;
	uint8_t transport_flags;
	uint8_t ref_count;
	uint64_t timestamp;

	struct netg_buffer* next;
};

struct netg_endpoint {
	enum netg_endpoint_type type;

	uint16_t port;
	ip_addr_t ip_addr;
	mac_addr_t mac_addr;

	uint16_t net_interface;

	enum netg_endpoint_status status;

	void* transport;

	// callbacks
	void (*recv) (void *arg, struct netg_endpoint* uendpoint, struct netg_buffer* p, ip_addr_t addr, uint16_t port);
	void* recv_arg;

	struct netg_endpoint* next;
};

uint16_t ng_htons(uint16_t n);
uint16_t ng_ntohs(uint16_t n);
uint32_t ng_htonl(uint32_t n);
uint32_t ng_ntohl(uint32_t n);


struct netg_buffer* netg_get_buffer(struct netg_endpoint* endpoint);
void netg_free_buffer(struct netg_buffer* buffer);
void netg_claim_buffer(struct netg_buffer* buffer);

void netg_init(const char* card_name);

void netg_tick(void);


/* ------------------- UDP ------------------- */
struct netg_endpoint *udp_new(void);

void udp_remove(struct netg_endpoint* endpoint);

errval_t udp_bind(struct netg_endpoint* endpoint, ip_addr_t ipaddr, uint16_t port);

errval_t udp_connect(struct netg_endpoint* endpoint, ip_addr_t ipaddr, uint16_t port);

void udp_disconnect(struct netg_endpoint*);

void udp_recv(struct netg_endpoint* endpoint,
			  void (*recv) (void *arg, struct netg_endpoint* uendpoint, struct netg_buffer* p, ip_addr_t addr, uint16_t port),
			  void *recv_arg);

errval_t udp_sendto(struct netg_endpoint* endpoint, struct netg_buffer *p, ip_addr_t dst_ip, uint16_t dst_port);
errval_t udp_send(struct netg_endpoint* endpoint, struct netg_buffer *p);


/* ------------------- TCP ------------------- */
struct netg_endpoint* tcp_new(void);

errval_t tcp_bind(struct netg_endpoint* endpoint, ip_addr_t ipaddr, uint16_t port);

errval_t tcp_listen(struct netg_endpoint* endpoint);

errval_t tcp_connect(struct netg_endpoint* endpoint, ip_addr_t ipaddr, uint16_t port,
		errval_t(*connected) (void *arg, struct netg_endpoint* endpoint, errval_t err));

void tcp_abandon(struct netg_endpoint* endpoint, int reset);

errval_t tcp_close(struct netg_endpoint* endpoint);

errval_t tcp_write(struct netg_endpoint* endpoint, const void *dataptr, uint16_t len, uint8_t apiflags);

// callbacks
void tcp_accept(struct netg_endpoint* endpoint, errval_t(*accept) (void *arg, struct netg_endpoint* endpoint, errval_t err));

void tcp_recv(struct netg_endpoint* endpoint, errval_t(*recv) (void *arg, struct netg_endpoint* endpoint, struct netg_buffer * p, errval_t err));

void tcp_sent(struct netg_endpoint* endpoint, errval_t(*sent) (void *arg, struct netg_endpoint* endpoint, uint16_t len));

void tcp_err(struct netg_endpoint* endpoint, void (*err) (void *arg, errval_t err));

void tcp_arg(struct netg_endpoint* endpoint, void *arg);

/* ---------------------- DEBUG ---------------- */
void print_tcp_status(struct netg_endpoint* endpoint);
#endif

