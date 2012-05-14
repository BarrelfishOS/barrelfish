/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>

#include <barrelfish/net_constants.h>
#include <if/net_queue_manager_defs.h>
#include <contmng/contmng.h>

#include "idc_net_queue_manager.h"

#define MAX_SERVICE_NAME_LEN  256   // Max len that a name of service can have
#define BUFFER_SIZE 2048
#define BUFFER_COUNT ((128*1024*1024) / BUFFER_SIZE)

//FIXME use a more dynamic approach for fast access
#define MAX_QUEUES 256
struct netg_queue* queues[MAX_QUEUES];


static void idc_register_buffer(struct netg_queue* queue,
								struct net_queue_manager_binding *binding,
                                struct capref buf, struct capref sp,
                                uint64_t qid, uint64_t slots, uint8_t role);

static void idc_raw_add_buffer(struct net_queue_manager_binding *binding,
                               uint64_t offset, uint64_t len);

static void idc_get_mac_address(struct net_queue_manager_binding *binding,
                                uint64_t qid);


/******************************************************************************/
/* Buffer management */

void buffer_tx_add(struct netg_queue* queue, size_t idx, size_t offset, size_t len)
{
    idc_raw_add_buffer(queue->tx_binding.binding, idx * BUFFER_SIZE + offset, len);
}

void buffer_rx_add(struct netg_queue* queue, size_t idx)
{
    idc_raw_add_buffer(queue->rx_binding.binding, idx * BUFFER_SIZE, BUFFER_SIZE);
}


static void alloc_mem(struct capref *frame, void** virt, size_t size)
{
    errval_t r;
    vregion_flags_t flags;

    r = frame_alloc(frame, size, NULL);
    if (!err_is_ok(r)) {
        USER_PANIC("Allocating memory region frame failed!");
    }

    flags = VREGION_FLAGS_READ_WRITE;
    r = vspace_map_one_frame_attr(virt, size, *frame, flags, NULL, NULL);
    if (!err_is_ok(r)) {
        USER_PANIC("Mapping memory region frame failed!");
    }
    memset(*virt, 0, size);
}

static void buffers_init(struct netg_queue* queue, size_t count)
{
    struct waitset *ws = get_default_waitset();

    alloc_mem(&queue->buffer_frame, &queue->buffer_base, BUFFER_SIZE * count);

    queue->rx_binding.bufid = queue->tx_binding.bufid = -1ULL;

    idc_register_buffer(queue, queue->rx_binding.binding, queue->buffer_frame, NULL_CAP,
    					queue->queue_id , count,  RX_BUFFER_ID);
    while (queue->rx_binding.bufid  == -1ULL) { event_dispatch(ws); }

    idc_register_buffer(queue, queue->tx_binding.binding, queue->buffer_frame, NULL_CAP,
    					queue->queue_id , count, TX_BUFFER_ID);
    while (queue->tx_binding.bufid  == -1ULL) { event_dispatch(ws); }
}


/******************************************************************************/
/* Flounder interface */

static errval_t send_raw_add_buffer(struct q_entry entry)
{
    struct net_queue_manager_binding *b = entry.binding_ptr;

    if (b->can_send(b)) {
        return net_queue_manager_raw_add_buffer__tx(b,
                                                    MKCONT(cont_queue_callback,
                                                           ((struct netg_queue_binding*)b->st)->cont_queue),
                                                    entry.plist[0],
                                                    entry.plist[1]);
    } else {
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void idc_raw_add_buffer(struct net_queue_manager_binding *binding,
                               uint64_t offset, uint64_t len)
{
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_raw_add_buffer;
    entry.binding_ptr = binding;
    entry.plist[0] = offset;
    entry.plist[1] = len;

    enqueue_cont_q(((struct netg_queue_binding*)binding->st)->cont_queue, &entry);
}


static void idc_register_buffer(struct netg_queue* queue,
								struct net_queue_manager_binding *binding,
                                struct capref buf, struct capref sp,
                                uint64_t qid, uint64_t slots, uint8_t role)
{
    errval_t err;
    err = net_queue_manager_register_buffer__tx(binding, NOP_CONT, buf, sp,
                                                queue->queue_id, slots, role);
    assert(err_is_ok(err));
}

static void idc_get_mac_address(struct net_queue_manager_binding *binding,
                                uint64_t qid)
{
    errval_t err;
    debug_printf("getting mac for queue id %lu\n", qid);
    err = net_queue_manager_get_mac_address__tx(binding, NOP_CONT, qid);
    if(err_is_fail(err)) {
    	debug_printf("ERROR: %s\n", err_getstring(err));
    	assert("FAIL");
    }
}

static void new_buffer_id(struct net_queue_manager_binding *st, errval_t err,
                          uint64_t queueid, uint64_t buffer_id)
{
    assert(err_is_ok(err));

    struct netg_queue_binding* qnb = st->st;

    if (st == qnb->binding) {
        qnb->bufid = buffer_id;
    } else {
        qnb->bufid = buffer_id;
    }
}

static void raw_xmit_done(struct net_queue_manager_binding *st,
                          uint64_t offset, uint64_t len)
{
    size_t idx = offset / BUFFER_SIZE;
    struct netg_queue_binding* nqb = st->st;

    if (st == nqb->queue->rx_binding.binding) {
        net_rx_done(nqb->queue, idx, len);
    } else {
        net_tx_done(nqb->queue, idx);
    }
}

static void get_mac_address_response(struct net_queue_manager_binding *st,
                                     uint64_t qid, uint64_t mac)
{
    struct netg_queue_binding* nqb = st->st;
    nqb->queue->netif->mac = mac;
}

static struct net_queue_manager_rx_vtbl rx_vtbl = {
    .new_buffer_id = new_buffer_id,
    .raw_xmit_done = raw_xmit_done,
    .get_mac_address_response = get_mac_address_response,
};

static void bind_cb(void *st, errval_t err, struct net_queue_manager_binding *b)
{
    assert(err_is_ok(err));

    struct netg_queue* queue = st;

    b->rx_vtbl = rx_vtbl;

    if (queue->rx_binding.binding == NULL) {
        queue->rx_binding.binding = b;
        b->st = &queue->rx_binding;
        queue->rx_binding.cont_queue = create_cont_q("interface_raw");
    } else {
        queue->tx_binding.binding = b;
        b->st = &queue->tx_binding;
        queue->tx_binding.cont_queue = create_cont_q("interface_raw");
    }
}

static void connect_to_driver(const char *cname, struct netg_queue* queue)
{
    errval_t err;
    iref_t iref;
    char qm_name[MAX_SERVICE_NAME_LEN] = { 0 };

    snprintf(qm_name, sizeof(qm_name), "%s_%"PRIu64, cname, queue->queue_id);
    err = nameservice_blocking_lookup(qm_name, &iref);
    assert(err_is_ok(err));

    err = net_queue_manager_bind(iref, bind_cb, queue, get_default_waitset(),
                                 IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));

}

void net_if_init(struct netg_interface* netif, uint64_t qid)
{
    struct waitset *ws = get_default_waitset();

    struct netg_queue* queue = malloc(sizeof(struct netg_queue));
    queue->queue_id = qid;
    queue->netif = netif;
    queue->next = netif->queues;
    queue->rx_binding.binding = NULL;
    queue->rx_binding.queue = queue;
    queue->tx_binding.binding = NULL;
    queue->tx_binding.queue = queue;
    queue->buffer_size = BUFFER_SIZE;
    queue->buffer_count = BUFFER_COUNT;
    netif->queues = queue;
    netif->mac = 0;
    queues[qid] = queue;

    // Connect RX path
    connect_to_driver(netif->card_name, queue);
    while (queue->rx_binding.binding  == NULL) { event_dispatch(ws); }
    //queue->binding_rx->st = queue;

    // Connect TX path
    connect_to_driver(netif->card_name, queue);
    while (queue->tx_binding.binding  == NULL) { event_dispatch(ws); }
    //queue->binding_tx->st = queue;

    buffers_init(queue, BUFFER_COUNT);

    // Get MAC address
    idc_get_mac_address(queue->rx_binding.binding, queue->queue_id);
    while (netif->mac == 0) { event_dispatch(ws); }
}

void net_if_terminate(struct netg_queue* queue)
{
    vspace_unmap(queue->buffer_base);
    cap_delete(queue->buffer_frame);
}


struct netg_queue* get_queue_from_id(uint64_t queue_id)
{
	assert(queue_id < MAX_QUEUES);

	return queues[queue_id];
}
