/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef E10K_QUEUE_H_
#define E10K_QUEUE_H_

#include <string.h>
#include <stdlib.h>

#include "e10k_q_dev.h"

struct e10k_queue_ops {
    errval_t (*update_txtail)(void*, size_t);
    errval_t (*update_rxtail)(void*, size_t);
};

struct e10k_queue {
    // FIXME: Look for appropriate type for the _head/tail/size fields
    e10k_q_tdesc_legacy_array_t*    tx_ring;
    void**                          tx_opaque;
    size_t                          tx_head;
    size_t                          tx_tail;
    size_t                          tx_size;

    e10k_q_rdesc_legacy_array_t*    rx_ring;
    void**                          rx_opaque;
    size_t                          rx_head;
    size_t                          rx_tail;
    size_t                          rx_size;

    struct e10k_queue_ops           ops;
    void*                           opaque;
};

typedef struct e10k_queue e10k_queue_t;

static inline e10k_queue_t* e10k_queue_init(void* tx, size_t tx_size, void* rx,
    size_t rx_size, struct e10k_queue_ops* ops, void* opaque)
{
    e10k_queue_t* q = malloc(sizeof(*q));

    q->tx_ring = tx;
    q->tx_opaque = malloc(sizeof(void*) * tx_size);
    q->tx_head = 0;
    q->tx_tail = 0;
    q->tx_size = tx_size;

    q->rx_ring = rx;
    q->rx_opaque = malloc(sizeof(void*) * rx_size);
    q->rx_head = 0;
    q->rx_tail = 0;
    q->rx_size = rx_size;

    q->ops = *ops;
    q->opaque = opaque;

    // Initialize ring memory with zero
    memset(tx, 0, tx_size * e10k_q_tdesc_legacy_size);
    memset(rx, 0, rx_size * e10k_q_rdesc_legacy_size);

    return q;
}


static inline int e10k_queue_add_txbuf(e10k_queue_t* q, uint64_t phys,
    size_t len, void* opaque, int last)
{
    e10k_q_tdesc_legacy_t d;
    size_t tail = q->tx_tail;

    // TODO: Check if there is room in the queue
    q->tx_opaque[tail] = opaque;
    d = q->tx_ring[tail];

    e10k_q_tdesc_legacy_buffer_insert(d, phys);
    e10k_q_tdesc_legacy_length_insert(d, len);
    // OPTIMIZATION: Maybe only set rs on last packet?
    e10k_q_tdesc_legacy_rs_insert(d, 1);
    e10k_q_tdesc_legacy_ifcs_insert(d, 1);
    e10k_q_tdesc_legacy_eop_insert(d, last);

    q->tx_tail = (tail + 1) % q->tx_size;
    return 0;
}

static inline int e10k_queue_get_txbuf(e10k_queue_t* q, void** opaque,
    int* last)
{
    e10k_q_tdesc_legacy_t d;
    size_t head = q->tx_head;

    d = q->tx_ring[head];
    if (e10k_q_tdesc_legacy_dd_extract(d)) {
        *last = e10k_q_tdesc_legacy_eop_extract(d);
        *opaque = q->tx_opaque[head];

        memset(d, 0, e10k_q_tdesc_legacy_size);

        q->tx_head = (head + 1) % q->tx_size;
        return 0;
    } else {
        return 1;
    }
}

static inline errval_t e10k_queue_bump_txtail(e10k_queue_t* q)
{
    return q->ops.update_txtail(q->opaque, q->tx_tail);
}

static inline size_t e10k_queue_free_txslots(e10k_queue_t* q)
{
    size_t head = q->tx_head;
    size_t tail = q->tx_tail;
    size_t size = q->tx_size;

    if (tail > head) {
        return size - (head - tail) - 1; // TODO: could this be off by 1?
    } else {
        return size - (head + size - tail) - 1; // TODO: off by 1?
    }
}

static inline int e10k_queue_add_rxbuf(e10k_queue_t* q, uint64_t phys,
    void* opaque)
{
    e10k_q_rdesc_legacy_t d;
    size_t tail = q->rx_tail;

    // TODO: Check if there is room in the queue
    q->rx_opaque[tail] = opaque;
    d = q->rx_ring[tail];

    e10k_q_rdesc_legacy_buffer_insert(d, phys);

    q->rx_tail = (tail + 1) % q->rx_size;

    return 0;
}

static inline int e10k_queue_get_rxbuf(e10k_queue_t* q, void** opaque,
    size_t* len, int* last)
{
    e10k_q_rdesc_legacy_t d;
    size_t head = q->rx_head;

    d = q->rx_ring[head];
    if (e10k_q_rdesc_legacy_dd_extract(d)) {
        *last = e10k_q_rdesc_legacy_eop_extract(d);
        *len = e10k_q_rdesc_legacy_length_extract(d);
        // TODO: Extract status (okay/error)
        *opaque = q->rx_opaque[head];

        memset(d, 0, e10k_q_rdesc_legacy_size);

        q->rx_head = (head + 1) % q->rx_size;
        return 0;
    } else {
        return 1;
    }

}

static inline errval_t e10k_queue_bump_rxtail(e10k_queue_t* q)
{
    return q->ops.update_rxtail(q->opaque, q->rx_tail);
}


#endif // ndef E10K_QUEUE_H_
