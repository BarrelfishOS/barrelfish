/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CAPQUEUE_H
#define CAPQUEUE_H

#include <stdbool.h>
#include <barrelfish/event_queue.h>

struct capqueue_queue {
    struct event_queue queues[2];
    struct event_queue_node queue_end;
    bool on_first, running, retrigger;
};

static inline void
capqueue_wait(struct capqueue_queue *q, struct event_queue_node *qn,
             struct event_closure event)
{
    event_queue_add(&q->queues[!q->on_first], qn, event);
}

void capqueue_notify(struct capqueue_queue *q);

void capqueue_init(struct capqueue_queue *q, struct waitset *ws);

#endif
