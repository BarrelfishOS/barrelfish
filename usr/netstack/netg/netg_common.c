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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <limits.h>
#include <trace/trace.h>

#include "netg_common.h"
#include "netg_udp.h"
#include "netg_ipv4.h"
#include "netg_ethernet.h"
#include "netg_buffer.h"

#include "idc_net_queue_manager.h"



struct netg_endpoint* active_local_endpoints = NULL;
struct netg_endpoint* active_remote_endpoints = NULL;
struct netg_node* discard = NULL;

struct netg_node_state_queue* runnable_queues = NULL;

struct netg_interface* active_interfaces = NULL;

#define SWAP_BYTES_IN_WORD(w) ((w & 0xff) << 8) | ((w & 0xff00) >> 8)
#define FOLD_U32T(u)          ((u >> 16) + (u & 0x0000ffffUL))

uint16_t calc_standard_chksum(void *dataptr, uint32_t start, uint16_t len)
{
    uint32_t acc;
    uint16_t src;
    uint8_t *octetptr;

    acc = start;
    /* dataptr may be at odd or even addresses */
    octetptr = (uint8_t *) dataptr;
    while (len > 1) {
        /* declare first octet as most significant
           thus assume network order, ignoring host order */
        src = (*octetptr) << 8;
        octetptr++;
        /* declare second octet as least significant */
        src |= (*octetptr);
        octetptr++;
        acc += src;
        len -= 2;
    }
    if (len > 0) {
        /* accumulate remaining octet */
        src = (*octetptr) << 8;
        acc += src;
    }
    /* add deferred carry bits */
    acc = (acc >> 16) + (acc & 0x0000ffffUL);
    if ((acc & 0xffff0000UL) != 0) {
        acc = (acc >> 16) + (acc & 0x0000ffffUL);
    }
    /* This maybe a little confusing: reorder sum using htons()
       instead of ntohs() since it has a little less call overhead.
       The caller must invert bits for Internet sum ! */
    return ng_htons((uint16_t) acc);
}

/* inet_chksum_pseudo:
 *
 * Calculates the pseudo Internet checksum used by TCP and UDP for a pbuf chain.
 * IP addresses are expected to be in network byte order.
 *
 * @param p chain of pbufs over that a checksum should be calculated (ip data part)
 * @param src source ip address (used for checksum of pseudo header)
 * @param dst destination ip address (used for checksum of pseudo header)
 * @param proto ip protocol (used for checksum of pseudo header)
 * @param proto_len length of the ip data part (used for checksum of pseudo header)
 * @return checksum (as u16_t) to be saved directly in the protocol header
 */
uint16_t inet_chksum_pseudo(void *dataptr,
                   ip_addr_t src, ip_addr_t dest,
                   uint8_t proto, uint16_t proto_len)
{
    uint32_t acc;
    uint8_t swapped;

    acc = 0;
    swapped = 0;

	acc += calc_standard_chksum(dataptr, 0, proto_len);
	/* just executing this next line is probably faster that the if statement needed
	   to check whether we really need to execute it, and does no harm */
	acc = FOLD_U32T(acc);
	if (proto_len % 2 != 0) {
		swapped = 1 - swapped;
		acc = SWAP_BYTES_IN_WORD(acc);
	}

    if (swapped) {
        acc = SWAP_BYTES_IN_WORD(acc);
    }
    acc += (src & 0xffffUL);
    acc += ((src >> 16) & 0xffffUL);
    acc += (dest & 0xffffUL);
    acc += ((dest >> 16) & 0xffffUL);
    acc += (uint32_t) ng_htons((uint16_t) proto);
    acc += (uint32_t) ng_htons(proto_len);

    /* Fold 32-bit sum to 16 bits
       calling this twice is propably faster than if statements... */
    acc = FOLD_U32T(acc);
    acc = FOLD_U32T(acc);

    return (uint16_t) ~ (acc & 0xffffUL);
}

struct netg_endpoint* netg_endpoint_lookup(struct netg_endpoint** list, uint16_t net_interface, enum netg_endpoint_type type, ip_addr_t ip_addr, uint16_t port)
{
	struct netg_endpoint* it = *list;

	while (it != NULL) {
		//debug_printf("lookup %x %hu endpoint %x %hu \n", ip_addr, port, it->ip_addr, it->port);
		if (it->port == port) {
			if (it->ip_addr == ip_addr) {
				if (it->type == type) {
					if (it->net_interface == net_interface) {
						return it;
					}
				}
			}
		}
		it = it->next;
	}

	return NULL;
}

void netg_endpoint_insert(struct netg_endpoint** list, struct netg_endpoint* endpoint)
{
	assert(endpoint!=NULL);

	if (*list == NULL) {
		*list = endpoint;
	} else {
		struct netg_endpoint* it = *list;

		while (it->next != NULL) {
			it = it->next;
		}

		it->next = endpoint;
	}
}
void netg_endpoint_remove(struct netg_endpoint** list, struct netg_endpoint* endpoint)
{
	if (*list != NULL) {
		struct netg_endpoint* it = *list;
		struct netg_endpoint** old = list;

		while (it != NULL) {
			if (it == endpoint) {
				debug_printf("removing endpoint\n");
				*old = it->next;
			} else {
				old = &it->next;
			}

			it = it->next;
		}
	}
}

struct netg_node_state_queue*  netg_queue_state_init(uint16_t size,
					bool queue_runnable,
					struct netg_node* node,
					errval_t (*process)(uint16_t, struct netg_node_state_queue*)) {
	struct netg_node_state_queue* qst = malloc(sizeof(struct netg_node_state_queue));
	assert(qst!= NULL);

	qst->queue = malloc(size * sizeof(struct netg_buffer*));
	qst->size = size;
	qst->start = 0;
	qst->end = 0;
	qst->queue_runnable = queue_runnable;
	qst->node = node;
	qst->process = process;

	return qst;
}

void netg_queue_push(struct netg_node_state_queue* qst, struct netg_buffer* buffer) {
	if ((qst->end+1)%qst->size != qst->start) {
		qst->queue[qst->end] = buffer;
		qst->end = (qst->end+1)%qst->size;
	} else {
		debug_printf("queue is overflowing, dropping packet\n");
		discard->process(buffer, discard);
	}
}
void netg_queue_prepend(struct netg_node_state_queue* qst, struct netg_buffer* buffer) {
	if ((qst->start-1+qst->size)%qst->size != qst->end) {
		qst->start = (qst->start-1+qst->size)%qst->size;
		qst->queue[qst->start] = buffer;
	} else {
		debug_printf("queue is overflowing, dropping packet\n");
		discard->process(buffer, discard);
	}
}

struct netg_buffer* netg_queue_pop(struct netg_node_state_queue* qst) {
	struct netg_buffer* buf = NULL;
	if (qst->start!=qst->end) {
		buf = qst->queue[qst->start];
		qst->start = (qst->start+1)%qst->size;
	} else {
		debug_printf("no buffer in queue\n");
	}
	return buf;
}

struct netg_buffer* netg_queue_pop_last(struct netg_node_state_queue* qst) {
	struct netg_buffer* buf = NULL;
	if (qst->start!=qst->end) {
		qst->end = (qst->end-1+qst->size)%qst->size;
		buf = qst->queue[qst->end];
	} else {
		debug_printf("no buffer in queue\n");
	}
	return buf;
}

struct netg_buffer* netg_queue_peek(struct netg_node_state_queue* qst) {
	struct netg_buffer* buf = NULL;
	if (qst->start!=qst->end) {
		buf = qst->queue[qst->start];
	} else {
		debug_printf("no buffer in queue\n");
	}
	return buf;
}

struct netg_buffer* netg_queue_peek_last(struct netg_node_state_queue* qst) {
	struct netg_buffer* buf = NULL;
	if (qst->start!=qst->end) {
		buf = qst->queue[(qst->end-1+qst->size)%qst->size];
	} else {
		debug_printf("no buffer in queue\n");
	}
	return buf;
}

uint16_t netg_queue_length(struct netg_node_state_queue* qst) {
	return (qst->end + qst->size - qst->start)%qst->size;
}

void netg_enqueue_runnable_queue(struct netg_node_state_queue* runnable) {
	if (runnable_queues == NULL) {
		runnable_queues = runnable;
	} else {
		struct netg_node_state_queue* it = runnable_queues;

		while (it->next != NULL) {
			it = it->next;
		}

		it->next = runnable;
	}
}

struct netg_node* create_node(struct netg_node* next) {
	struct netg_node* node = malloc(sizeof(struct netg_node));
	node->next = next;
	node->node_state = NULL;

	return node;
}

struct netg_node_state_classifier
{
	struct netg_node* udp;
	struct netg_node* tcp;
};

static errval_t process_classifier(struct netg_buffer* buffer, struct netg_node* this) {
	struct ip_header* ip_hdr = buffer->data + HEADER_LENGTH_ETHERNET;
	struct netg_node_state_classifier* nstate = (struct netg_node_state_classifier*)this->node_state;

	if(ip_hdr->protocol == IP_PROTOCOL_TCP) {
		assert(nstate->tcp != NULL);
		return nstate->tcp->process(buffer, nstate->tcp);
	} else if (ip_hdr->protocol == IP_PROTOCOL_UDP) {
		assert(nstate->udp != NULL);
		return nstate->udp->process(buffer, nstate->udp);
	} else {
		if (discard!=NULL) {
			return discard->process(buffer, this->next);
		}
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_classifier(struct netg_node* tcp, struct netg_node* udp) {
	struct netg_node* node = create_node(NULL);
	struct netg_node_state_classifier* nstate = malloc(sizeof(struct netg_node_state_classifier));
	nstate->udp = udp;
	nstate->tcp = tcp;
	node->node_state = nstate;
	node->process = process_classifier;

	return node;
}

static errval_t process_discard_buffer(struct netg_buffer* buffer, struct netg_node* this) {
	if (buffer->local_endpoint != NULL) {
		debug_printf("discarding buffer for local endpoint %s %hu\n",
							buffer->local_endpoint->type == NETG_ENDPOINT_TCP ? "TCP": "UDP",
							buffer->local_endpoint->port);
	} else {
		debug_printf("discarding unclassified buffer\n");
	}

	netg_free_buffer(buffer);

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_discard_buffer(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_discard_buffer;

	return node;
}

static errval_t process_runnable_queue(uint16_t batch_size, struct netg_node_state_queue* qst) {
	struct netg_node* this = qst->node;
	assert(this != NULL);

	if (qst->start != qst->end) {
		debug_printf("queue should run with batch size %hu, queue contains %hu items\n", batch_size, qst->end - qst->start);


		while ((qst->start != qst->end) && batch_size > 0) {
			struct netg_buffer* buffer = netg_queue_pop(qst);
			if (buffer!=NULL) {
				batch_size--;

				debug_printf("passing along a buffer\n");

				buffer->next = NULL;

				if (this->next!=NULL) {
					this->next->process(buffer, this->next);
				}
			}
		}
	}
	return SYS_ERR_OK;
}

static errval_t enqueue_runnable_queue(struct netg_buffer* buffer, struct netg_node* this) {
	netg_queue_push(this->node_state, buffer);
	//debug_printf("enqueueing buffer\n");
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_runnable_queue(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = enqueue_runnable_queue;

	node->node_state = netg_queue_state_init(128, true, node, process_runnable_queue);

	netg_enqueue_runnable_queue((struct netg_node_state_queue*) node->node_state);

	return node;
}

static errval_t process_timestamp(struct netg_buffer* buffer, struct netg_node* this) {
#if defined(__x86_64__) || defined(__i386__)
    buffer->timestamp = rdtsc();
    debug_printf("NETG: TIMESTAMP NOW %lu\n", buffer->timestamp);
#endif

    if (this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}

    return SYS_ERR_OK;
}

struct netg_node* netg_node_create_timestamp(struct netg_node* next) {
	struct netg_node* node = create_node(next);
	node->process = process_timestamp;

	return node;
}

static errval_t evaluate_timestamp(struct netg_buffer* buffer, struct netg_node* this) {
	uint64_t now = 0;
#if defined(__x86_64__) || defined(__i386__)
    now = rdtsc();

    //uint64_t tscperms;
    //errval_t err = sys_debug_get_tsc_per_ms(&tscperms);
    //assert(err_is_ok(err));

    debug_printf("NETG: TIMESTAMP NOW at eval %lu\n", now);
    //debug_printf("took %lu ms\n", (now - buffer->timestamp)/tscperms);
#endif

    debug_printf("NETG: took %lu ticks\n", now - buffer->timestamp);

	if (this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_evaluate_timestamp(struct netg_node* next)
{
	struct netg_node* node = create_node(next);
	node->process = evaluate_timestamp;

	return node;
}

static errval_t sendto_queue(struct netg_buffer* buffer, struct netg_node* this)
{
	struct netg_queue* queue = get_queue_from_id(buffer->queue_id);
	size_t idx = (buffer->data - queue->buffer_base) / queue->buffer_size;

	printf("NETG: sending buffer %lu to queue manager\n", idx);
	buffer_tx_add(queue, idx, 0, buffer->header_length + buffer->length);

	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_sendto_queue(uint64_t queue_id)
{
	struct netg_node* node = create_node(NULL);
	node->process = sendto_queue;

	return node;
}

void net_tx_done(struct netg_queue* queue, size_t idx)
{
    //LWIPBF_DEBUG("benchmark_tx_done(%"PRIu64")\n", idx);
    //handle_tx_done(idx);

	debug_printf("TX_DONE %lu\n", idx);
}

static struct netg_node* recv_node = NULL;

void net_rx_done(struct netg_queue* queue, size_t idx, size_t len)
{
	debug_printf("RX_DONE %lu\n", idx);
	struct netg_buffer* buf = netg_get_rx_buffer(queue, idx);

	//FIXME remove... only used for debuging
	buf->length = len;

	recv_node->process(buf, recv_node);

	buffer_rx_add(queue, idx);
}

static errval_t recvfrom_queue(struct netg_buffer* buffer, struct netg_node* this)
{
	buffer->local_endpoint = NULL;
	buffer->remote_endpoint = NULL;
	buffer->header_length = 0;
	buffer->payload = buffer->data;

	if (this->next!=NULL) {
		return this->next->process(buffer, this->next);
	}
	return SYS_ERR_OK;
}

struct netg_node* netg_node_create_recvfrom_queue(struct netg_node* next, uint64_t queue_id)
{
	struct netg_node* node = create_node(next);
	node->process = recvfrom_queue;

	recv_node = node;

	return node;
}

void netg_schedule(void) {
	//debug_printf("SCHEDULING\n");
	struct netg_node_state_queue* it = runnable_queues;

	while (it != NULL) {
		if(it->queue_runnable && it->process != NULL && it->start!=it->end) {
			debug_printf("got a queue with %hu entries and runnable is %u, running it with batch size 2\n", it->end - it->start, it->queue_runnable);
			it->process(2, it);
		}

		it = it->next;
	}
}
