/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include "capqueue.h"

static void
queue_complete(void *arg)
{
    struct capqueue_queue *q = (struct capqueue_queue*)arg;
    q->running = false;
    if (q->retrigger) {
        q->retrigger = false;
        capqueue_notify(q);
    }
}

void
capqueue_notify(struct capqueue_queue *q)
{
    if (q->retrigger) {
        return;
    }

    if (q->running) {
        q->retrigger = true;
        return;
    }

    q->on_first = !q->on_first;
    q->running = true;
    capqueue_wait(q, &q->queue_end, MKCLOSURE(queue_complete, q));
    // event queue is in continuous mode, does not need triggering
}

void
capqueue_init(struct capqueue_queue *q, struct waitset *ws)
{
    memset(q, 0, sizeof(*q));
    event_queue_init(&q->queues[0], ws, EVENT_QUEUE_CONTINUOUS);
    event_queue_init(&q->queues[1], ws, EVENT_QUEUE_CONTINUOUS);
    q->on_first = true;
}
