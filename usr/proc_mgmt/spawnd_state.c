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

#include <barrelfish/barrelfish.h>

#include "spawnd_state.h"

static struct spawnd_state *spawnds[MAX_COREID];

/**
 * \brief Allocates a state structure for a new spawnd binding.
 *
 * \param core_id       core where the spawnd newly bound with runs.
 * \param spawn_binding Flounder binding structure for the spawnd.
 */
errval_t spawnd_state_alloc(coreid_t core_id, struct spawn_binding *b)
{
    spawnds[core_id] = (struct spawnd_state*) malloc(
            sizeof(struct spawnd_state));
    if (spawnds[core_id] == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    spawnds[core_id]->b = b;
    spawnds[core_id]->core_id = core_id;
    spawnds[core_id]->sendq.head = NULL;
    spawnds[core_id]->sendq.tail = NULL;
    spawnds[core_id]->recvq.head = NULL;
    spawnds[core_id]->recvq.tail = NULL;

    b->st = spawnds[core_id];

    return SYS_ERR_OK;
}

/**
 * \brief Returns whether connected to spawnd on the given core.
 */
inline bool spawnd_state_exists(coreid_t core_id)
{
    return spawnds[core_id] != NULL;
}

/**
 * \brief Returns the state element for the spawnd on the given core.
 */
inline struct spawnd_state *spawnd_state_get(coreid_t core_id)
{
    return spawnds[core_id];
}

/**
 * \brief Enqueue on a waitset queue.
 *
 * \param q    Pointer to queue to enqueue on
 * \param m    Pointer to element to enqueue
 *
 * \return true if queue was empty, false if not.
 */
static bool enqueue(struct msg_queue *q, struct msg_queue_elem *m)
{
    assert(m->next == NULL);

    // Enqueue on the queue
    if(q->tail != NULL) {
        q->tail->next = m;
    } else {
        assert(q->head == NULL);
        q->head = m;
    }
    q->tail = m;

    return q->head == q->tail ? true : false;
}

/**
 * \brief Dequeues from a waitset queue.
 *
 * \param q    Pointer to queue to dequeue from
 *
 * \return the newly dequeued element.
 */
static struct msg_queue_elem *dequeue(struct msg_queue *q)
{
    // Queue should have at least one element
    assert(q->head != NULL && q->tail != NULL);

    struct msg_queue_elem *e = q->head;
    q->head = e->next;
    if(q->head == NULL) {
        q->tail = NULL;
    }

    return e;
}

/**
 * \brief Enqueue an element on a waitset queue IN FRONT.
 *
 * \param q    Pointer to queue to enqueue on
 * \param m    Pointer to element to enqueue
 *
 * \return true if queue was empty, false if not.
 */
static bool enqueue_at_front(struct msg_queue *q, struct msg_queue_elem *m)
{
    assert(m->next == NULL);
    if(q->tail == NULL) {
        assert(q->head == NULL);
        q->head = m;
        q->tail = m;
    } else {
        m->next = q->head;
        q->head = m;
    }
    return q->head == q->tail ? true : false;
}

/**
 * \brief Event-based handler for sending requests to spawnd.
 *
 * This function pops the next request from the send queue of the targeted
 * spawnd (wrapped inside arg). It attempts to send the request, re-enqueuing
 * it at front if sending fails. It then re-registers a new send if the queue
 * still has pending requests.
 *
 * \param arg Wrapper over the spawnd_state structure for the target spawnd.
 */
static void spawnd_send_handler(void *arg)
{
    struct spawnd_state *spawnd = (struct spawnd_state*) arg;
    struct msg_queue *q = &spawnd->sendq;

    // Dequeue next element from the queue
    struct msg_queue_elem *m = (struct msg_queue_elem*) dequeue(q);

    assert(m->cont != NULL);
    if (m->cont(m)) {
        // Send continuation succeeded, need to enqueue a receive.
        struct msg_queue_elem *recvm = (struct msg_queue_elem*) malloc(
                sizeof(struct msg_queue_elem));
        recvm->st = m->st;
        recvm->next = NULL;
        enqueue(&spawnd->recvq, recvm);
    } else {
        // Send continuation failed, need to re-enqueue message.
        enqueue_at_front(q, m);
    }

    if (q->head != NULL) {
        // Queue is non-empty, therefore re-register.
        errval_t err = spawnd->b->register_send(spawnd->b, spawnd->b->waitset,
                                                MKCONT(spawnd_send_handler,
                                                       arg));
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "regitering for spawnd send");
            return;
        }
    }
}

/**
 * \brief Enqueues a new send request event.
 *
 * \param spawnd target spawnd to send the request to.
 * \param msg    request to enqueue.
 */
errval_t spawnd_state_enqueue_send(struct spawnd_state *spawnd,
                                   struct msg_queue_elem *msg)
{
    msg->next = NULL;

    // If queue was empty, enqueue on waitset
    if(enqueue(&spawnd->sendq, msg)) {
        return spawnd->b->register_send(spawnd->b, spawnd->b->waitset,
                                        MKCONT(spawnd_send_handler, spawnd));
    } else {
        return SYS_ERR_OK;
    }
}

/**
 * \brief Dequeues and returns the next message in a receive queue.
 *
 * \param spawnd spawnd instance whose receive queue to pop.
 */
void *spawnd_state_dequeue_recv(struct spawnd_state *spawnd)
{
    struct msg_queue_elem *m = dequeue(&spawnd->recvq);
    assert(m != NULL);
    return m->st;
}