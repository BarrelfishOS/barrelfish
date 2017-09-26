/*
 * \brief Spawnd state internals for the process manager.
 *
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SPAWND_STATE_H
#define SPAWND_STATE_H

#include <stdbool.h>

#include <if/spawn_defs.h>
#include <barrelfish/barrelfish.h>

struct spawnd_state;
struct msg_queue_elem;
typedef bool (*msg_cont_handler_fn)(struct msg_queue_elem*);

struct msg_queue_elem {
	void *st;
	msg_cont_handler_fn cont;

    struct msg_queue_elem *next;
};

struct msg_queue {
    struct msg_queue_elem *head, *tail;
};

struct spawnd_state {
    coreid_t core_id;
    struct spawn_binding *b;

    struct msg_queue sendq;
    struct msg_queue recvq;
};

errval_t spawnd_state_alloc(coreid_t core_id, struct spawn_binding *b);
bool spawnd_state_exists(coreid_t core_id);
struct spawnd_state *spawnd_state_get(coreid_t core_id);

errval_t spawnd_state_enqueue_send(struct spawnd_state *spawnd,
                                   struct msg_queue_elem *msg);
void *spawnd_state_dequeue_recv(struct spawnd_state *spawnd);

#endif  // SPAWND_STATE
