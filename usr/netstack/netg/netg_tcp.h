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

#ifndef _NETG_TCP_H_
#define _NETG_TCP_H_

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>

#include "netg_common.h"
#include "netg_ipv4.h"
#include "netg_ethernet.h"

typedef uint32_t tcp_seq;

enum NETG_TCP_STATE {
	NETG_TCP_CLOSED,
	NETG_TCP_LISTEN,
	NETG_TCP_SYN_SENT,
	NETG_TCP_SYN_RECV,
	NETG_TCP_ESTABLISHED,
	NETG_TCP_CLOSE_WAIT,
	NETG_TCP_LAST_ACK,
	NETG_TCP_FIN_WAIT_1,
	NETG_TCP_FIN_WAIT_2,
	NETG_TCP_CLOSING,
	NETG_TCP_TIME_WAIT
};

struct netg_tcp_state {
	struct netg_endpoint* remote;
	struct netg_endpoint* local;

	uint8_t state;

	uint16_t maxseg;

	// send window
	tcp_seq snd_unack;
	tcp_seq snd_next;
	tcp_seq iss;
	uint32_t snd_window;

	// recv window
	tcp_seq rcv_next;
	tcp_seq rcv_acked;
	tcp_seq irs;
	uint32_t rcv_window;

	// retransmission
	int16_t rtx_timer;  // retransmission timer
	int16_t rtx_timeout;    // retransmission timeout
	uint8_t rtx_count;

	// fast retransmission
	tcp_seq last_ack;
	uint8_t dup_acks;

	// time_wait timer
	int16_t two_msl_timer;

	// connection specific queues
	struct netg_node_state_queue* reorder_queue;
	struct netg_node_state_queue* retransmit_queue;
	struct netg_node_state_queue* unsent_queue;

	// callbacks
	void* arg;
	errval_t(*accept) (void *arg, struct netg_endpoint* endpoint, errval_t err);
	errval_t(*connected) (void *arg, struct netg_endpoint* endpoint, errval_t err);
	errval_t(*recv) (void *arg, struct netg_endpoint* endpoint, struct netg_buffer * p, errval_t err);
	//errval_t(*sent) (void *arg, struct netg_endpoint* endpoint, uint16_t len);
	//void (*err) (void *arg, errval_t err);
};


#define HEADER_LENGTH_TCP 20

#define HEADER_LENGTH_TCP_COMPLETE (HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP + HEADER_LENGTH_TCP)

#define NETG_DUPACKS_THRESH 3

#define NETG_TIMER_2MSL 60
#define NETG_TIMER_MAX_FW2 1200

struct tcp_header {
	uint16_t source_port;
	uint16_t dest_port;
	uint32_t seq_number;
	uint32_t ack_number;
	uint8_t  header_length;
	uint8_t  flags;
	uint16_t window_size;
	uint16_t tcp_checksum;
	uint16_t urgent;
} __attribute__((packed));

#define TCP_FLAG_URG 0x20
#define TCP_FLAG_ACK 0x10
#define TCP_FLAG_PSH 0x8
#define TCP_FLAG_RST 0x4
#define TCP_FLAG_SYN 0x2
#define TCP_FLAG_FIN 0x1

#define TCP_GET_HEADER_LENGTH(header_length) (header_length >> 4)
#define TCP_SET_HEADER_LENGTH(header_length) (header_length << 4)
#define TCP_FLAG_IS_FIN(flags) (flags & TCP_FLAG_FIN)
#define TCP_FLAG_IS_SYN(flags) (flags & TCP_FLAG_SYN)
#define TCP_FLAG_IS_RST(flags) (flags & TCP_FLAG_RST)
#define TCP_FLAG_IS_ACK(flags) (flags & TCP_FLAG_ACK)

void debug_print_tcp_header(struct tcp_header* header);

errval_t netg_tcp_connect(struct netg_endpoint* endpoint, struct netg_endpoint* remote, struct netg_node* start);
errval_t netg_tcp_enqueue(struct netg_endpoint* local, const void *dataptr, uint16_t len, struct netg_node* start);
errval_t netg_tcp_timer(void);
errval_t netg_tcp_close(struct netg_endpoint* local, struct netg_node* start);
errval_t netg_tcp_reset(struct netg_endpoint* local, struct netg_node* start);

struct netg_tcp_state* netg_tcp_create_state(struct netg_endpoint* local);

struct netg_node* netg_node_create_tcp_tx_check_window(struct netg_node* next);
struct netg_node* netg_node_create_tcp_tx(struct netg_node* next);
struct netg_node* netg_node_create_tcp_tx_update_state(struct netg_node* next);
struct netg_node* netg_node_create_tcp_tx_checksum(struct netg_node* next);
struct netg_node* netg_node_create_tcp_tx_retransmit(struct netg_node* next);


struct netg_node* netg_node_create_tcp_rx_checksum(struct netg_node* next);

/**
 * classifies the type of packet
 */
struct netg_node* netg_node_create_tcp_rx_classifier(struct netg_node* listen,
													struct netg_node* normal);

/**
 * handles incoming connection attempts for listen endpoints
 */
struct netg_node* netg_node_create_tcp_rx_listen(struct netg_node* next);

/**
 * TCP state machine
 */
struct netg_node* netg_node_create_tcp_rx_update_connection_info(struct netg_node* next, struct netg_node* ctrl);

/**
 * updates TCP connection stuff like window size and acks
 */
struct netg_node* netg_node_create_tcp_rx_update_state(struct netg_node* next, struct netg_node* ctrl);

/**
 * Reorders tcp segments and checks if they fit the receive window
 */
struct netg_node* netg_node_create_tcp_rx_reorder(struct netg_node* next);

/**
 * Sends immediate ACK
 */
struct netg_node* netg_node_create_tcp_rx_ack_new_data(struct netg_node* next, struct netg_node* send_ack);

/**
 * upcall node to tcp_recv callback
 */
struct netg_node* netg_node_create_tcp_recv(struct netg_node* next);

#endif
