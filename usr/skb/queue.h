/**
 * \file
 * \brief Queue for stack-ripped inter-monitor code
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef QUEUE_H
#define QUEUE_H

#include <barrelfish/barrelfish.h>
#include <if/skb_defs.h>


struct msg_queue {
    struct msg_queue_elem *head, *tail;
};

struct msg_queue_elem {
    struct msg_queue_elem *next;
};

struct skb_queue_state {
    struct msg_queue queue;
};

static inline bool msg_queue_is_empty(struct msg_queue *q)
{
    return (q->head == NULL);
}

struct skb_msg_queue_elem;
typedef void (*skb_msg_cont_handler_fn)(struct skb_binding *b,
                                        struct skb_msg_queue_elem*);

struct skb_msg_queue_elem {
    struct msg_queue_elem queue;
    skb_msg_cont_handler_fn cont;
};

errval_t skb_enqueue_send(struct skb_binding* b, struct msg_queue *q,
                          struct waitset *ws, struct msg_queue_elem *ms);

errval_t skb_enqueue_send_at_front(struct skb_binding *b, struct msg_queue *q,
                                   struct waitset *ws, struct msg_queue_elem *ms);

#endif
