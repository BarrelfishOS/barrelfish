/**
 * \file
 * \brief LWIP test/demo code
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "netg_buffer.h"
#include "idc_net_queue_manager.h"

/**
 * Return the buffer size for a particular binding
 */
static inline size_t binding_to_buffersize(struct netg_queue* queue, uint8_t binding_index)
{
    return (queue->buffer_count / 2) * queue->buffer_size;
}

/**
 * Return the buffer address for a particular binding.
 * The idea is that we use the first half of the buffers for RX and the second
 * half for TX.
 */
static inline void *binding_to_buffer(struct netg_queue* queue, uint8_t binding_index)
{
    uint8_t *buf = queue->buffer_base;

    if (binding_index == TX_BUFFER_ID) {
        buf += binding_to_buffersize(queue, RX_BUFFER_ID);
    }

    return buf;
}

static inline uint64_t buffer_to_index(struct netg_queue* queue, struct netg_buffer* buffer)
{
	return (buffer->data - queue->buffer_base) / queue->buffer_size;
}



static void netg_init_tx_buffers(struct netg_queue* queue)
{
	void* data = binding_to_buffer(queue, TX_BUFFER_ID);
	struct netg_buffer* bufs = malloc(sizeof(struct netg_buffer) * (queue->buffer_count / 2));

	for (int i=0;i<(queue->buffer_count / 2);i++) {
		bufs[i].data = data + queue->buffer_size*i;
		bufs[i].queue_id = queue->queue_id;
		bufs[i].next = queue->buffer_free_list;
		queue->buffer_free_list = &bufs[i];

		queue->buffers[buffer_to_index(queue, &bufs[i])] = &bufs[i];
	}
}

static void netg_init_rx_buffers(struct netg_queue* queue)
{
	void* data = binding_to_buffer(queue, RX_BUFFER_ID);
	struct netg_buffer* bufs = malloc(sizeof(struct netg_buffer) * (queue->buffer_count / 2));

	//FIXME
	for (int i=0;i<1024;i++) {
		bufs[i].data = data + queue->buffer_size*i;
		bufs[i].queue_id = queue->queue_id;
		queue->buffers[buffer_to_index(queue, &bufs[i])] = &bufs[i];
		buffer_rx_add(queue, buffer_to_index(queue, &bufs[i]));
	}
}

struct netg_buffer* netg_get_tx_buffer(uint64_t queue_id)
{
	struct netg_queue* queue = get_queue_from_id(queue_id);
	struct netg_buffer* temp = NULL;
	if (queue != NULL) {
		temp = queue->buffer_free_list;

		if (queue->buffer_free_list != NULL) {
			queue->buffer_free_list = queue->buffer_free_list->next;
		}
	}
	return temp;
}

struct netg_buffer* netg_get_rx_buffer(struct netg_queue* queue, uint64_t idx)
{
	return queue->buffers[idx];
}



void netg_init_buffers(struct netg_queue* queue)
{
	queue->buffers = malloc(sizeof(struct netg_buffer*) * queue->buffer_count);
	debug_printf("init tx\n");
	netg_init_tx_buffers(queue);
	debug_printf("init rx\n");
	netg_init_rx_buffers(queue);
	debug_printf("buffer init complete\n");
}

