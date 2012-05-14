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

#include "netg.h"
#include "netg_tcp.h"

#define TCP_SEQ_LT(a,b)     ((int32_t)((a)-(b)) < 0)
#define TCP_SEQ_LEQ(a,b)    ((int32_t)((a)-(b)) <= 0)
#define TCP_SEQ_GT(a,b)     ((int32_t)((a)-(b)) > 0)
#define TCP_SEQ_GEQ(a,b)    ((int32_t)((a)-(b)) >= 0)

#define TCP_SEQ_BETWEEN(a,b,c) (TCP_SEQ_GEQ(a,b) && TCP_SEQ_LEQ(a,c))

static tcp_seq iss = 0x13131313;

void debug_print_tcp_header(struct tcp_header* header) {
	debug_printf("tcp_header: source port %hu dest port %hu header length %hu checksum %hx\n",
			ng_ntohs(header->source_port),
			ng_ntohs(header->dest_port),
			TCP_GET_HEADER_LENGTH(header->header_length)*4,
			header->tcp_checksum);
}

errval_t netg_tcp_connect(struct netg_endpoint* endpoint, struct netg_endpoint* remote, struct netg_node* start) {
	debug_printf("TCP: trying to connect to an %x on %hu\n", ng_ntohl(remote->ip_addr), ng_ntohs(remote->port));
	struct netg_tcp_state* state = (struct netg_tcp_state*)endpoint->transport;
	assert(state!=NULL);

	state->iss = iss;
	state->snd_next = iss + 1;
	state->snd_unack = state->snd_next;
	state->state = NETG_TCP_SYN_SENT;
	iss += 200;

	struct netg_buffer* buffer = netg_get_buffer(endpoint);

	struct tcp_header* hdr = buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP;
	hdr->source_port = endpoint->port;
	hdr->dest_port   = remote->port;
	hdr->seq_number  = state->iss;
	hdr->ack_number  = 0;
	hdr->header_length = TCP_SET_HEADER_LENGTH(5);
	hdr->flags = TCP_FLAG_SYN;
	hdr->window_size = state->rcv_window;
	hdr->tcp_checksum = 0;
	hdr->urgent = 0;

	buffer->local_endpoint = endpoint;
	buffer->remote_endpoint = remote;
	buffer->length = 0;

	return start->process(buffer, start);
}

errval_t netg_tcp_enqueue(struct netg_endpoint* local, const void *dataptr, uint16_t len, struct netg_node* start) {
	assert(len > 0);
	assert(dataptr != NULL);

	debug_printf("TCP: netg_tcp_enqueue called, len %hu\n", len);

	struct netg_tcp_state* tstate = (struct netg_tcp_state*)local->transport;
	assert(tstate != NULL);

	if(tstate->state == NETG_TCP_ESTABLISHED || tstate->state == NETG_TCP_CLOSE_WAIT) {
		if ( len > tstate->maxseg ) {
			assert("segment not yet implemented");
		} else {
			struct netg_buffer* buffer = netg_get_buffer(local);

			if (buffer == NULL) {
				return NETG_ERR_MEM;
			}

			//debug_printf("TCP: copying data into buffer\n");
			memcpy(buffer->payload, dataptr, len);
			buffer->length = len;
			buffer->local_endpoint = local;
			buffer->remote_endpoint = tstate->remote;
			buffer->next = NULL;

			return start->process(buffer, start);
		}
	} else {
		return NETG_ERR_NOT_CONNECTED;
	}

	return SYS_ERR_OK;
}

/**
 * Helper function for sending acks
 */
static errval_t netg_tcp_tx_ctrl(struct netg_endpoint* local, uint8_t flags, struct netg_node* start) {
	struct netg_buffer* ack_buffer = netg_get_buffer(local);

	if (ack_buffer == NULL) {
		return NETG_ERR_MEM;
	}

	ack_buffer->length = 0;
	ack_buffer->local_endpoint = local;
	ack_buffer->remote_endpoint = ((struct netg_tcp_state*)local->transport)->remote;
	ack_buffer->transport_flags = flags;

	if( start != NULL ) {
		return start->process(ack_buffer, start);
	}

	return SYS_ERR_OK;
}

errval_t netg_tcp_close(struct netg_endpoint* local, struct netg_node* start){
	debug_printf("TCP: trying to close the connection\n");

	struct netg_tcp_state* tstate = (struct netg_tcp_state*)local->transport;
	assert(tstate != NULL);

	if (tstate->state == NETG_TCP_ESTABLISHED) {
		tstate->state = NETG_TCP_FIN_WAIT_1;
		debug_printf("TCP STATE[FIN_WAIT_1] for %hu\n", ng_htons(tstate->local->port));
	} else if (tstate->state == NETG_TCP_CLOSE_WAIT) {
		tstate->state = NETG_TCP_LAST_ACK;
		debug_printf("TCP STATE[LAST_ACK] for %hu\n", ng_htons(tstate->local->port));
	} else {
		return NETG_ERR_NOT_CONNECTED;
	}

	return netg_tcp_tx_ctrl(local, TCP_FLAG_FIN, start);
}

errval_t netg_tcp_reset(struct netg_endpoint* local, struct netg_node* start){
	debug_printf("TCP: trying to reset a connection\n");
	return netg_tcp_tx_ctrl(local, TCP_FLAG_RST, start);
}


static errval_t process_tcp_tx_check_window_queue(uint16_t batch_size, struct netg_node_state_queue* qst) {
	debug_printf("TCP: running on check window queue, length %hu\n", netg_queue_length(qst));

	struct netg_node* this = qst->node;
	assert(this != NULL);

	struct netg_buffer* buffer = netg_queue_peek(qst);
	struct tcp_header* hdr = buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP;

	if (buffer!= NULL) {
		struct netg_tcp_state* tstate = (struct netg_tcp_state*)buffer->local_endpoint->transport;
		//debug_printf("got state\n");
		while ((buffer != NULL) &&
			TCP_SEQ_BETWEEN(ng_htonl(hdr->seq_number) , tstate->snd_unack, tstate->snd_unack + tstate->snd_window) &&
			TCP_SEQ_BETWEEN(ng_htonl(hdr->seq_number) + buffer->length, tstate->snd_unack, tstate->snd_unack + tstate->snd_window)) {

			//debug_printf("passing a buffer along\n");

			netg_queue_pop(qst);
			this->next->process(buffer, this->next);

			buffer = netg_queue_peek(qst);
			if(buffer != NULL) {
				hdr = buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP;
			}
		}
	}

	qst->queue_runnable = false;

	return SYS_ERR_OK;
}

static errval_t process_tcp_tx_check_window(struct netg_buffer* buffer, struct netg_node* this) {
	struct netg_tcp_state* tstate = (struct netg_tcp_state*)buffer->local_endpoint->transport;
	struct tcp_header* hdr = buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP;

	//FIXME should not be set evertime
	tstate->unsent_queue->node = this;

	if (TCP_SEQ_BETWEEN(ng_htonl(hdr->seq_number), tstate->snd_unack, tstate->snd_unack + tstate->snd_window) &&
		TCP_SEQ_BETWEEN(ng_htonl(hdr->seq_number) + buffer->length, tstate->snd_unack, tstate->snd_unack + tstate->snd_window)) {
		//debug_printf("TCP: segment is in send window\n");

		if(this->next!=NULL) {
			return this->next->process(buffer, this->next);
		}
	} else {
		debug_printf("TCP: segment is outside of the send window, enqueue for later sending\n");
		netg_queue_push(tstate->unsent_queue, buffer);
		//tstate->unsent_queue->queue_runnable = true;
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_tx_check_window(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_tcp_tx_check_window;

	return node;
}

static errval_t process_tcp_tx(struct netg_buffer* buffer, struct netg_node* this) {
	struct tcp_header* hdr = buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP;
	struct netg_tcp_state* tstate = (struct netg_tcp_state*)buffer->local_endpoint->transport;
	assert(tstate!=NULL);

	//debug_printf("TCP: process_tcp_tx\n");

	hdr->tcp_checksum = 0;

	hdr->source_port = buffer->local_endpoint->port;
	hdr->dest_port = buffer->remote_endpoint->port;
	hdr->seq_number = ng_htonl(tstate->snd_next);
	tstate->snd_next += buffer->length;
	hdr->header_length = TCP_SET_HEADER_LENGTH(5);
	if (buffer->transport_flags == 0) {
		hdr->flags = TCP_FLAG_ACK;
	} else {
		//debug_printf("TCP: header flags are %u\n", buffer->transport_flags);
		hdr->flags = buffer->transport_flags;
		if( TCP_FLAG_IS_FIN(buffer->transport_flags)) {
			tstate->snd_next++;
		}
	}
	hdr->tcp_checksum = 0;
	hdr->urgent = 0;

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_tx(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_tcp_tx;

	return node;
}

static errval_t process_tcp_tx_update_state(struct netg_buffer* buffer, struct netg_node* this) {
	struct tcp_header* hdr = buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP;
	struct netg_tcp_state* tstate = (struct netg_tcp_state*)buffer->local_endpoint->transport;
	assert(tstate!=NULL);

	//debug_printf("TCP: process_tcp_tx_update_state\n");

	hdr->ack_number = ng_htonl(tstate->rcv_next);
	tstate->rcv_acked = tstate->rcv_next;
	hdr->window_size = ng_htons(tstate->rcv_window);

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_tx_update_state(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_tcp_tx_update_state;

	return node;
}

static errval_t process_tcp_tx_checksum(struct netg_buffer* buffer, struct netg_node* this) {
	struct tcp_header* hdr = buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP;

	/*debug_printf("TCP: calculating checksum for segment %u ack %u for endpoint %hu, length %hu\n",
			hdr->seq_number, hdr->ack_number, ng_ntohs(buffer->local_endpoint->port), buffer->length);*/

	hdr->tcp_checksum = 0;
	hdr->tcp_checksum = inet_chksum_pseudo(buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP,
												buffer->local_endpoint->ip_addr,
												buffer->remote_endpoint->ip_addr,
												IP_PROTOCOL_TCP,
												HEADER_LENGTH_TCP + buffer->length);

	//debug_printf("TCP: chechksum %hx\n", hdr->tcp_checksum);

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_tx_checksum(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_tcp_tx_checksum;

	return node;
}

static errval_t process_tcp_tx_retransmit_enqueue(struct netg_buffer* buffer, struct netg_node* this) {
	struct netg_tcp_state* tstate = (struct netg_tcp_state*)buffer->local_endpoint->transport;
	assert(tstate!=NULL);

	//debug_printf("TCP: enqueueing on the retransmit queue, queue size %u\n",netg_queue_length(tstate->retransmit_queue));

	if(buffer->length > 0 || buffer->transport_flags != TCP_FLAG_ACK) {
		netg_claim_buffer(buffer);
		netg_queue_prepend(tstate->retransmit_queue, buffer);

		// enable retransmit timer if disabled
		if(tstate->rtx_timer == -1) {
			tstate->rtx_timer = 0;
		}
	}

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}

	return SYS_ERR_OK;
}

static void remove_acked_segments(struct netg_tcp_state* tstate, struct netg_node_state_queue* qst, tcp_seq ack) {
	struct netg_buffer* buffer = netg_queue_peek_last(qst);
	//debug_printf("TCP: removing semgent from retransmit queue\n");

	while (buffer != NULL) {
		struct tcp_header* hdr = buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP;

		if(TCP_SEQ_LT(ng_htonl(hdr->seq_number), ack)) {
			//debug_printf("TCP: remove segment %u\n", hdr->seq_number);
			netg_queue_pop_last(qst);
			discard->process(buffer, discard);
			buffer = netg_queue_peek_last(qst);
		} else {
			//debug_printf("TCP: segment %u outside range %u\n", hdr->seq_number, ack);
			//finished with the acked segments
			break;
		}
	}

	// disable retransmit timer if no more segments in retransmit queue
	if( buffer == NULL) {
		tstate->rtx_timer = -1;
	}
}

static errval_t process_tcp_tx_retransmit(struct netg_tcp_state* tstate) {
	struct netg_buffer* buf = netg_queue_pop(tstate->retransmit_queue);

	while(buf != NULL) {
		netg_queue_prepend(tstate->unsent_queue, buf);

		buf = netg_queue_pop(tstate->retransmit_queue);
	}

	// schedule unsent queue
	tstate->unsent_queue->queue_runnable = true;

	return SYS_ERR_OK;
}

static errval_t process_tcp_tx_fast_retransmit(struct netg_tcp_state* tstate) {
	struct netg_buffer* buf = netg_queue_pop_last(tstate->retransmit_queue);

	if(buf != NULL) {
		netg_queue_prepend(tstate->unsent_queue, buf);
	}

	// schedule unsent queue
	tstate->unsent_queue->queue_runnable = true;

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_tx_retransmit(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_tcp_tx_retransmit_enqueue;

	return node;
}

static errval_t process_tcp_rx_checksum(struct netg_buffer* buffer, struct netg_node* this) {
	struct ip_header* ip_hdr = buffer->data + HEADER_LENGTH_ETHERNET;

	//debug_printf("buffer length %hu\n", buffer->length);

	uint16_t chks = inet_chksum_pseudo(buffer->payload,
												ip_hdr->source_ip_addr,
												ip_hdr->dest_ip_addr,
												IP_PROTOCOL_TCP,
												buffer->length);

	if ( chks!=0 ) {
		debug_printf("TCP: checksum MISMATCHES, discarding packet\n");
		return discard->process(buffer, discard);
	}

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_rx_checksum(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_tcp_rx_checksum;

	return node;
}

struct netg_node_state_tcp_rx_classifier
{
	struct netg_node* listen;
	struct netg_node* normal;
};

static errval_t process_tcp_rx_classifier(struct netg_buffer* buffer, struct netg_node* this) {
	//debug_printf("TCP CLASS: trying to classify the incoming tcp packet\n");
	struct ip_header* ip_hdr = buffer->data + HEADER_LENGTH_ETHERNET;
	struct tcp_header* tcp_hdr = buffer->payload;
	//debug_printf("TCP: got a tcp packet with sequence %u\n", tcp_hdr->seq_number);

	struct netg_endpoint* it_remote = active_remote_endpoints;

	struct netg_node_state_tcp_rx_classifier* next = (struct netg_node_state_tcp_rx_classifier*)this->node_state;

	struct netg_tcp_state* tstate = NULL;

	while(it_remote != NULL && tstate == NULL) {
		if (it_remote->type == NETG_ENDPOINT_TCP &&
				it_remote->ip_addr == ip_hdr->source_ip_addr &&
				it_remote->port == tcp_hdr->source_port) {

			//debug_printf("TCP CLASS: found remote endpoint that matches, searching local...\n");

			if(it_remote->transport != NULL) {
				struct netg_tcp_state* temp_tstate = (struct netg_tcp_state*) it_remote->transport;
				if(temp_tstate->local->ip_addr == ip_hdr->dest_ip_addr &&
						temp_tstate->local->port == tcp_hdr->dest_port) {

					//debug_printf("TCP CLASS: found local endpoint that matches\n");

					tstate = temp_tstate;
				} else {
					debug_printf("local ip and/or port does not match\n");
				}
			} else {
				debug_printf("transport was not set!\n");
			}
		}

		it_remote = it_remote->next;
	}

	// is part of an existing connection?
	if( tstate != NULL) {
		// setting buffers endpoints
		buffer->local_endpoint = tstate->local;
		buffer->remote_endpoint = tstate->remote;

		return next->normal->process(buffer, next->normal);
	} else {
		// is a new connection request?
		if ( TCP_FLAG_IS_SYN(tcp_hdr->flags) && !TCP_FLAG_IS_ACK(tcp_hdr->flags)) {
			struct netg_endpoint* listen = NULL;
			struct netg_endpoint* it_local = active_local_endpoints;

			while (it_local != NULL && listen == NULL) {
				if (it_local->type == NETG_ENDPOINT_TCP && it_local->status == NETG_ENDPOINT_LISTEN) {
					if (it_local->ip_addr == ip_hdr->dest_ip_addr && it_local->port == tcp_hdr->dest_port) {
						listen = it_local;
					}
				}

				it_local = it_local->next;
			}

			if (listen != NULL) {
				debug_printf("TCP CLASS: found listen endpoint\n");
				//debug_print_tcp_header(tcp_hdr);

				buffer->local_endpoint = listen;
				if(next->listen!=NULL) {
					return next->listen->process(buffer, next->listen);
				}
			} else {
				debug_printf("TCP: could not find matching listen endpoint\n");
			}
		}
	}

	return discard->process(buffer, discard);
}

struct netg_node* netg_node_create_tcp_rx_classifier(struct netg_node* listen,
													struct netg_node* normal) {
	struct netg_node* node = create_node(NULL);
	node->process = process_tcp_rx_classifier;
	struct netg_node_state_tcp_rx_classifier* state = malloc(sizeof(struct netg_node_state_tcp_rx_classifier));
	state->listen = listen;
	state->normal = normal;

	node->node_state = state;
	return node;
}

static errval_t process_tcp_rx_listen(struct netg_buffer* buffer, struct netg_node* this) {
	struct netg_endpoint* listen = buffer->local_endpoint;

	struct netg_tcp_state* listen_tstate = (struct netg_tcp_state*)listen->transport;

	struct netg_endpoint* local = tcp_new();
	local->ip_addr = listen->ip_addr;
	local->port = listen->port;
	local->mac_addr = listen->mac_addr;
	local->type = listen->type;
	local->net_interface = listen->net_interface;

	struct ethernet_header* eth_hdr = buffer->data;
	struct ip_header* ip_hdr = buffer->data + HEADER_LENGTH_ETHERNET;
	struct tcp_header* hdr = buffer->payload;

	struct netg_endpoint* remote = malloc(sizeof(struct netg_endpoint));
	remote->type = local->type = listen->type;
	remote->next = NULL;

	remote->status = NETG_ENDPOINT_CLOSED;
	remote->recv = NULL;
	remote->recv_arg = NULL;

	remote->ip_addr = ip_hdr->source_ip_addr;
	remote->port    = hdr->source_port;
	remote->mac_addr = eth_hdr->source_mac;
	remote->net_interface   = listen->net_interface;

	netg_endpoint_insert(&active_remote_endpoints, remote);


	struct netg_tcp_state* tstate = (struct netg_tcp_state*)local->transport;

	remote->transport = tstate;

	tstate->state = NETG_TCP_SYN_RECV;
	tstate->remote = remote;
	tstate->local = local;


	tstate->snd_window = hdr->window_size;
	tstate->snd_next = 0;
	tstate->snd_unack = 0;
	tstate->rcv_next = hdr->seq_number+1;
	tstate->rcv_acked = tstate->rcv_next;

	tstate->accept = listen_tstate->accept;
	tstate->arg = listen_tstate->arg;

	//TODO parse options for max segment size

	//debug_printf("TCP: endpoints created, sending SYNACK\n");


	tstate->iss = iss;
	iss += 200;

	tstate->snd_next = tstate->iss + 1;
	tstate->snd_unack = tstate->snd_next;

	netg_free_buffer(buffer);
	buffer = netg_get_buffer(local);

	// reuse buffer
	hdr = buffer->data + HEADER_LENGTH_ETHERNET + HEADER_LENGTH_IP;
	buffer->length = 0;
	buffer->header_length = HEADER_LENGTH_TCP_COMPLETE;
	buffer->local_endpoint = local;
	buffer->remote_endpoint = remote;

	hdr->tcp_checksum = 0;

	hdr->source_port = local->port;
	hdr->dest_port   = remote->port;
	hdr->seq_number  = ng_htonl(tstate->iss);
	hdr->ack_number  = ng_htonl(tstate->rcv_next);
	hdr->header_length = TCP_SET_HEADER_LENGTH(5);
	hdr->flags = TCP_FLAG_ACK | TCP_FLAG_SYN;
	hdr->window_size = ng_htons(tstate->rcv_window);
	hdr->tcp_checksum = 0;
	hdr->urgent = 0;

	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_rx_listen(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_tcp_rx_listen;

	return node;
}

static errval_t process_tcp_rx_update_state(struct netg_buffer* buffer, struct netg_node* this) {
	struct netg_tcp_state* tstate = (struct netg_tcp_state*)buffer->local_endpoint->transport;
	assert(tstate!=NULL);

	struct tcp_header* hdr = buffer->payload;
	struct netg_node* ctrl = (struct netg_node*)this->node_state;

	tcp_seq ack = ng_htonl(hdr->ack_number);
	tcp_seq seq = ng_htonl(hdr->seq_number);

	debug_printf("TCP: updating state [%u], ack %u syn %u fin %u\n", tstate->state,
			TCP_FLAG_IS_ACK(hdr->flags), TCP_FLAG_IS_SYN(hdr->flags), TCP_FLAG_IS_FIN(hdr->flags));
	debug_printf("TCP: seq %u %x\n", seq, seq);
	debug_printf("TCP: ack %u %x\n", ack, ack);

	errval_t err = SYS_ERR_OK;

	switch(tstate->state) {
	case NETG_TCP_SYN_SENT:
		// if syn ack received, established a new connection
		if ( TCP_FLAG_IS_SYN(hdr->flags) && TCP_FLAG_IS_ACK(hdr->flags)) {

			tstate->state = NETG_TCP_ESTABLISHED;
			debug_printf("TCP: STATE[ESTABLISHED] for %hu, seq %d ack %d\n", ng_htons(tstate->local->port), seq, ack);

			tstate->snd_window = ng_htons(hdr->window_size);
			tstate->rcv_next = seq+1;
			tstate->rcv_acked = tstate->rcv_next;
			tstate->last_ack = ack;

			remove_acked_segments(tstate, tstate->retransmit_queue, ack);
			discard->process(buffer, discard);

			netg_tcp_tx_ctrl(tstate->local, TCP_FLAG_ACK, ctrl);

			if (tstate->connected != NULL) {
				return tstate->connected(tstate->arg, tstate->local, SYS_ERR_OK);
			}
		} else if (TCP_FLAG_IS_SYN(hdr->flags)) {
			debug_printf("TCP: simultaneous SYN, not supported... \n");
		}
		break;
	case NETG_TCP_SYN_RECV:
		// API upcall for a new connection
		//FIXME add checks if it is the correct packet... SEQ, ACK
		if (tstate->accept != NULL) {
			tstate->state = NETG_TCP_ESTABLISHED;
			debug_printf("TCP: STATE[ESTABLISHED] for %hu\n", ng_htons(tstate->local->port));
			tstate->last_ack = ack;

			remove_acked_segments(tstate, tstate->retransmit_queue, ack);
			discard->process(buffer, discard);

			return tstate->accept(tstate->arg, tstate->local, SYS_ERR_OK);
		}
		break;
	case NETG_TCP_CLOSE_WAIT:
	case NETG_TCP_ESTABLISHED:
		err = this->next->process(buffer, this->next);
		if (TCP_FLAG_IS_FIN(hdr->flags)) {
			tstate->state = NETG_TCP_CLOSE_WAIT;
			debug_printf("TCP: STATE[CLOSE_WAIT] for %hu\n", ng_htons(tstate->local->port));

			//FIXME handle out of order FIN
			if (seq == tstate->rcv_next) {
				tstate->rcv_next++;
			}

			netg_tcp_tx_ctrl(tstate->local, TCP_FLAG_ACK, ctrl);
		}
		return err;
	case NETG_TCP_FIN_WAIT_1:
		if (!TCP_FLAG_IS_FIN(hdr->flags)) {
			if (TCP_FLAG_IS_ACK(hdr->flags) && ack == tstate->snd_next) {
				tstate->state = NETG_TCP_FIN_WAIT_2;
				tstate->two_msl_timer = 0;
				debug_printf("TCP: STATE[FIN_WAIT_2] for %hu\n", ng_htons(tstate->local->port));
			}
			return this->next->process(buffer, this->next);
		} else {
			err = this->next->process(buffer, this->next);

			//FIXME handle out of order FIN
			if (seq == tstate->rcv_next) {
				tstate->rcv_next++;
			}

			// simultaneous closing
			if (ack == tstate->snd_next) {
				tstate->state = NETG_TCP_TIME_WAIT;
				tstate->two_msl_timer = 0;
				debug_printf("TCP: STATE[TIME_WAIT] for %hu\n", ng_htons(tstate->local->port));
			} else {
				tstate->state = NETG_TCP_CLOSING;
				debug_printf("TCP: STATE[CLOSING] for %hu\n", ng_htons(tstate->local->port));
			}

			netg_tcp_tx_ctrl(tstate->local, TCP_FLAG_ACK, ctrl);

			return err;
		}
	case NETG_TCP_FIN_WAIT_2:
		err = this->next->process(buffer, this->next);
		if (TCP_FLAG_IS_FIN(hdr->flags)) {
			tstate->state = NETG_TCP_TIME_WAIT;
			tstate->rtx_timer = -1;
			tstate->two_msl_timer = 0;
			debug_printf("TCP: STATE[TIME_WAIT] for %hu\n", ng_htons(tstate->local->port));
			netg_tcp_tx_ctrl(tstate->local, TCP_FLAG_ACK, ctrl);
		}
		return err;
	case NETG_TCP_CLOSING:
		err = this->next->process(buffer, this->next);
		if (ack == tstate->snd_next) {
			tstate->state = NETG_TCP_TIME_WAIT;
			tstate->rtx_timer = -1;
			tstate->two_msl_timer = 0;
			debug_printf("TCP: STATE[TIME_WAIT] for %hu\n", ng_htons(tstate->local->port));
		}
		return err;
	case NETG_TCP_LAST_ACK:
		if (TCP_FLAG_IS_ACK(hdr->flags) && ack == tstate->snd_next) {
			tstate->state = NETG_TCP_CLOSED;
			tstate->rtx_timer = -1;
			debug_printf("TCP STATE[CLOSED] for %hu\n", ng_htons(tstate->local->port));
		}
		return this->next->process(buffer, this->next);
	case NETG_TCP_TIME_WAIT:
	case NETG_TCP_CLOSED:
	case NETG_TCP_LISTEN:
	default:
		debug_printf("TCP: state %u, don't know what to do, discard packet...\n", tstate->state);
		break;
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_rx_update_state(struct netg_node* next, struct netg_node* ctrl) {
	struct netg_node* node = create_node(next);
	node->process = process_tcp_rx_update_state;
	node->node_state = ctrl;

	return node;
}

static errval_t process_tcp_rx_update_connection_info(struct netg_buffer* buffer, struct netg_node* this) {
	struct netg_tcp_state* tstate = (struct netg_tcp_state*)buffer->local_endpoint->transport;
	assert(tstate!=NULL);

	struct tcp_header* hdr = buffer->payload;
	bool no_data = (4 * TCP_GET_HEADER_LENGTH(hdr->header_length) == buffer->length);
	tcp_seq ack = ng_htonl(hdr->ack_number);
	tcp_seq seq = ng_htonl(hdr->seq_number);

	debug_printf("TCP: got an ack number %u and window size %hu\n", hdr->ack_number, hdr->window_size);

	if ((tstate->snd_window != ng_htons(hdr->window_size))) {
		// window changed
		if(tstate->unsent_queue->start != tstate->unsent_queue->end) {
			tstate->unsent_queue->queue_runnable = true;
		}

	// same ack number, no data and there is data in the retransmit queue
	} else if ((ack == tstate->last_ack) && no_data &&
			(tstate->retransmit_queue->start != tstate->retransmit_queue->end)) {
		// no window update and same ack
		if( ++tstate->dup_acks == NETG_DUPACKS_THRESH) {
			// do fast retransmit
			process_tcp_tx_fast_retransmit(tstate);

			// update window
		} else if (tstate->dup_acks > NETG_DUPACKS_THRESH) {
			//increase congestion window by maxseg and set unsent queue runnable
		}
	}

	if(TCP_SEQ_GEQ(ack, tstate->snd_unack)) {
		tstate->snd_unack = ack;
		tstate->snd_window = ng_htons(hdr->window_size);

		if(tstate->unsent_queue->start != tstate->unsent_queue->end) {
			tstate->unsent_queue->queue_runnable = true;
		}

		// new ack arrived, reset retransmit
		tstate->rtx_count = 0;
		tstate->rtx_timer = 0;

		// reset fast retransmit
		tstate->dup_acks = 0;
		tstate->last_ack = ack;

		remove_acked_segments(tstate, tstate->retransmit_queue, hdr->ack_number);
	}

	if (seq > tstate->rcv_next) {
		// packet out of order, send immediate ack
		debug_printf("TCP: received packet out of order, sending immediate ACK!\n");

		netg_tcp_tx_ctrl(buffer->local_endpoint, TCP_FLAG_ACK, (struct netg_node*)this->node_state);
	}


	if( no_data  && (TCP_FLAG_IS_ACK(hdr->flags) || TCP_FLAG_IS_FIN(hdr->flags))) {
		//debug_printf("TCP: only ack packet, dropping packet\n");
		return discard->process(buffer, discard);
	} else if (!no_data) {
		/*debug_printf("TCP: packet contains data, ip_len %hu, tcp header len %hu, effective data length %hu\n",
				buffer->length,
				4 * TCP_GET_HEADER_LENGTH(hdr->header_length),
				buffer->length - 4 * TCP_GET_HEADER_LENGTH(hdr->header_length));*/

		if(this->next!=NULL) {
			return this->next->process(buffer, this->next);
		}
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_rx_update_connection_info(struct netg_node* next, struct netg_node* ctrl) {
	struct netg_node* node = create_node(next);
	node->process = process_tcp_rx_update_connection_info;
	node->node_state = ctrl;

	return node;
}

static errval_t process_tcp_rx_reorder(struct netg_buffer* buffer, struct netg_node* this) {
	struct netg_tcp_state* tstate = (struct netg_tcp_state*)buffer->local_endpoint->transport;
	assert(tstate!=NULL);

	struct tcp_header* hdr = buffer->payload;
	uint16_t data_length = buffer->length - 4 * TCP_GET_HEADER_LENGTH(hdr->header_length);
	tcp_seq seq = ng_htonl(hdr->seq_number);

	//debug_printf("TCP: reordering expecting seq %u packet has %u\n", tstate->rcv_next ,hdr->seq_number);

	struct netg_node_state_queue* qst = tstate->reorder_queue;

	// does the packet fit into the recv window? otherwise discard
	if (!TCP_SEQ_BETWEEN(seq, tstate->rcv_next, tstate->rcv_next + tstate->rcv_window) ||
			!TCP_SEQ_BETWEEN(seq + data_length, tstate->rcv_next, tstate->rcv_next + tstate->rcv_window)) {
		//discard since it is not in our window...
		debug_printf("TCP: packet outside of receive window, discard\n");
		return discard->process(buffer, discard);
	}

	// is the right packet? pass along, else enqueue
	if( tstate->rcv_next == seq ) {
		errval_t err = SYS_ERR_OK;
		if(this->next!=NULL) {

			tstate->rcv_next += data_length;

			err = this->next->process(buffer, this->next);
		}

		// got a now inorder packet?
		/*if (qst->queue_length > 0) {
			debug_printf("TCP: got unordered packets... check if now valid\n");
			bool found = true;

			while(found) {
				found = false;

				buffer = qst->queue;
				struct netg_buffer** old = &qst->queue;

				while(buffer != NULL) {
					hdr = buffer->payload;
					if( tstate->rcv_next == hdr->seq_number ) {
						debug_printf("found one\n");

						struct netg_buffer* temp = buffer;
						*old = buffer->next;
						buffer = buffer->next;

						temp->next = NULL;
						found = true;
						tstate->rcv_next += buffer->length - 4 * TCP_GET_HEADER_LENGTH(hdr->header_length);

						if(this->next!=NULL) {
							err = this->next->process(temp, this->next);
						}

					} else {
						if(TCP_SEQ_LT(hdr->seq_number, tstate->rcv_next)) {
							debug_printf("TCP: duplicate, discard\n");

							struct netg_buffer* temp = buffer;
							*old = buffer->next;
							buffer = buffer->next;

							temp->next = NULL;
							discard->process(temp, discard);
						}
						old = &buffer->next;
						buffer = buffer->next;
					}
				}
			}
		}*/

		return err;
	} else {
		debug_printf("TCP: out of order data arrived, enqueue it \n");

		netg_queue_push(qst, buffer);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_rx_reorder(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_tcp_rx_reorder;

	return node;
}

static errval_t process_tcp_rx_ack_new_data(struct netg_buffer* buffer, struct netg_node* this) {
	struct netg_tcp_state* tstate = (struct netg_tcp_state*)buffer->local_endpoint->transport;
	assert(tstate!=NULL);

	//debug_printf("TCP: acking new data, rcv_acked %u, rcv_next %u\n", tstate->rcv_acked, tstate->rcv_next);

	netg_tcp_tx_ctrl(buffer->local_endpoint, TCP_FLAG_ACK, (struct netg_node*)this->node_state);


	if(this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_rx_ack_new_data(struct netg_node* next, struct netg_node* send_ack) {
	struct netg_node* node = create_node(next);
	node->process = process_tcp_rx_ack_new_data;
	node->node_state = send_ack;

	return node;
}

static errval_t netg_node_process_tcp_recv(struct netg_buffer* buffer, struct netg_node* this)
{
	debug_printf("TCP: local port %hu remote port %hu\n", ng_ntohs(buffer->local_endpoint->port), ng_ntohs(buffer->remote_endpoint->port));
	struct netg_tcp_state* tstate = (struct netg_tcp_state*)buffer->local_endpoint->transport;
	assert(tstate!=NULL);

	struct tcp_header* hdr = buffer->payload;

	buffer->header_length += TCP_GET_HEADER_LENGTH(hdr->header_length)*4;
	buffer->payload = buffer->data + buffer->header_length;
	buffer->length -= TCP_GET_HEADER_LENGTH(hdr->header_length)*4;

	if(tstate->recv != NULL) {
		tstate->recv(tstate->arg, buffer->local_endpoint, buffer, SYS_ERR_OK);
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_tcp_recv(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = netg_node_process_tcp_recv;

	return node;
}

static void cleanup_endpoints(struct netg_endpoint* local) {
	//FIXME implement
	/*
	 * free tcp_state
	 * 		free reorder_queue;
	 * 			free buffers
	 * 		free retransmit_queue;
	 * 			free buffers
	 * 		free unsent_queue;
	 * 			free buffers
	 * free remote endpoint
	 * free local endpoint
	 *
	 * free port
	 */
}

errval_t netg_tcp_timer(void) {
	struct netg_endpoint* nept = active_local_endpoints;

	while (nept!=NULL) {
		if ( nept->type == NETG_ENDPOINT_TCP ) {
			struct netg_tcp_state* tstate = (struct netg_tcp_state*)nept->transport;
			assert(tstate != NULL);

			if (tstate->state == NETG_TCP_CLOSED) {
				nept = nept->next;

				debug_printf("TCP: removing closed endpoint %hu\n", ng_htons(tstate->local->port));
				netg_endpoint_remove(&active_local_endpoints, tstate->local);
				netg_endpoint_remove(&active_remote_endpoints, tstate->remote);
				cleanup_endpoints(tstate->local);
				continue;
			}

			if (tstate->rtx_timer >= 0 ) {
				assert(tstate->state != NETG_TCP_TIME_WAIT);
				assert(tstate->state != NETG_TCP_LISTEN);
				assert(tstate->state != NETG_TCP_CLOSED);

				debug_printf("TCP: incrementing rtx_timer for %hu\n", ng_ntohs(nept->port));
				tstate->rtx_timer++;

				if (tstate->rtx_timer >= tstate->rtx_timeout && netg_queue_peek(tstate->retransmit_queue) != NULL) {
					tstate->rtx_count++;
					debug_printf("TCP: retransmit segments for the %u time for %hu\n", tstate->rtx_count, ng_ntohs(nept->port));

					// reset timer
					tstate->rtx_timer = 0;

					// update congestion window


					process_tcp_tx_retransmit(tstate);
				} else if (netg_queue_peek(tstate->retransmit_queue) == NULL) {
					tstate->rtx_timer = -1;
				}
			}

			if (tstate->two_msl_timer >= 0) {
				tstate->two_msl_timer++;
				debug_printf("TCP: endpoint %hu, 2MSL %hu\n", ng_htons(tstate->local->port), tstate->two_msl_timer);

				// check if maximum time in fin wait 2
				if(tstate->state == NETG_TCP_FIN_WAIT_2) {
					if(tstate->two_msl_timer >= NETG_TIMER_MAX_FW2) {
						debug_printf("TCP: FIN_WAIT_2 TIMEOUT... setting state to TIME_WAIT\n");
						tstate->state = NETG_TCP_TIME_WAIT;
						tstate->two_msl_timer = 0;
					}
				// check if 2 msl are up and endpoint can be closed
				} else if (tstate->state == NETG_TCP_TIME_WAIT) {
					if (tstate->two_msl_timer >= NETG_TIMER_2MSL) {
						debug_printf("TCP: TIME_WAIT timeout, changing state to closed\n");
						tstate->state = NETG_TCP_CLOSED;
						tstate->two_msl_timer = -1;
					}
				} else {
					// unknown state fow 2MSL timer... disable it
					tstate->two_msl_timer = -1;
				}
			}
		}

		nept = nept->next;
	}

	return SYS_ERR_OK;
}


struct netg_tcp_state* netg_tcp_create_state(struct netg_endpoint* local) {
	struct netg_tcp_state* state = malloc(sizeof(struct netg_tcp_state));

	if (state != NULL) {
		state->state = NETG_TCP_CLOSED;
		state->remote = NULL;
		state->local = local;

		state->rcv_window = 14600;

		state->maxseg = 1460;

		state->reorder_queue = netg_queue_state_init(128, false, NULL, NULL);

		state->retransmit_queue =  netg_queue_state_init(128, false, NULL, NULL);



		state->unsent_queue =  netg_queue_state_init(2048, false, NULL, process_tcp_tx_check_window_queue);
		netg_enqueue_runnable_queue(state->unsent_queue);

		// disable retransmit timer until data is being sent
		state->rtx_timer = -1;
		// default retransmit timeout to 2s
		state->rtx_timeout   = 4;

		state->dup_acks = 0;

		state->two_msl_timer = -1;
	}

	return state;
}
