/**
 * \file
 * \brief Queue to deal with flounder continuations.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include "queue.h"

static void dist_rpc_send_next(void *arg)
{
    struct dist2_binding *b = arg;

    struct dist_reply_state* current = dist_rpc_dequeue_reply(b);

    // If more state in the queue, send them
    if (current) {
        current->reply(b, current);
    }
}

void dist_rpc_enqueue_reply(struct dist2_binding *b,
        struct dist_reply_state* st)
{
    if (b->st == NULL) {
        struct waitset *ws = get_default_waitset();
        b->register_send(b, ws, MKCONT(dist_rpc_send_next, b));
    }

    struct dist_reply_state** walk = (struct dist_reply_state**) &(b->st);
    for (; *walk != NULL; walk = &(*walk)->next) {
        // continue
    }
    *walk = st;
}

struct dist_reply_state* dist_rpc_dequeue_reply(struct dist2_binding *b)
{
    struct dist_reply_state* head = b->st;
    b->st = head->next;

    // Reregister for sending, if we need to send more
    if (b->st != NULL) {
        struct waitset *ws = get_default_waitset();
        errval_t err = b->register_send(b, ws, MKCONT(dist_rpc_send_next, b));
        assert(err_is_ok(err));
    }

    return head;
}

