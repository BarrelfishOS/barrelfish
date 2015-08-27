/**
 * \file
 * \brief Event queue implementatino
 *
 * This code implements a thread-safe queue of pending events which are
 * serviced by a single waitset.
 *
 * [!] WARNING: current realization of event queues is unsuitable for
 * cross-core operation. Crux of the issue: trigger channel for waitset
 * belonging to *another* core but waitsets are assumed to be local to a
 * dispatcher.
 *
 * Example scenario:
 * - Consumer C (on core 0) calls get_next_event and blocks on
 *   ws->waiting_threads.
 * - Producer P (on core 1) invokes event_queue_add which in turn triggers
 *   channel. P disables its dispatcher, unblocks a thread and C is returned.
 *   Code assumes remote wakeup should not occur but C is on core 0.
 *
 * For further details see:
 *   https://lists.inf.ethz.ch/mailman/private/barrelfish/2013/002746.html
 */

/*
 * Copyright (c) 2010, 2012, 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/event_queue.h>
#include <barrelfish/waitset_chan.h>

/**
 * \brief Initialise a new event queue
 *
 * \param q Storage for event queue
 * \param waitset Waitset that will service the queue
 * \param mode Operating mode for the queue
 */
void event_queue_init(struct event_queue *q, struct waitset *waitset,
                      enum event_queue_mode mode)
{
    waitset_chanstate_init(&q->waitset_state, CHANTYPE_EVENT_QUEUE);
    thread_mutex_init(&q->mutex);
    q->head = q->tail = NULL;
    q->waitset = waitset;
    q->mode = mode;
}

static struct event_queue_node *next_event(struct event_queue *q)
{
    // dequeue the next node from the head
    struct event_queue_node *qn = q->head;

    if (qn == NULL) {
        return NULL;
    }

    assert(qn->prev == NULL);

    if (qn->next == NULL) {
        assert(q->tail == qn);
        q->head = q->tail = NULL;
    } else {
        qn->next->prev = NULL;
        q->head = qn->next;
    }

    return qn;
}

static void event_queue_runner(void *arg)
{
    struct event_queue *q = arg;
    errval_t err;

    assert(q->mode == EVENT_QUEUE_CONTINUOUS);

    thread_mutex_lock(&q->mutex);

    // dequeue the next node from the head
    struct event_queue_node *qn = next_event(q);
    if (qn == NULL) {
        // an event was cancelled while we were pending
        thread_mutex_unlock(&q->mutex);
        return;
    }

    if (q->head != NULL) {
        // queue is non-empty: trigger ourselves again
        // (note: event registrations are single shot)
        struct event_closure self = {
            .handler = event_queue_runner,
            .arg = arg
        };
        err = waitset_chan_trigger_closure(q->waitset, &q->waitset_state, self);
        assert(err_is_ok(err)); // shouldn't fail
    }

    qn->run = true;
    thread_mutex_unlock(&q->mutex);

    // run closure
    qn->event.handler(qn->event.arg);
}

/**
 * \brief Cancel the runner if mode is continuous and queue is empty.
 *
 * Must hold this event_queue's mutex.
 *
 * \param q The event queue
 */
static errval_t
event_queue_cancel_runner(struct event_queue *q)
{
    errval_t err = SYS_ERR_OK;

    if (q->head == NULL && q->mode == EVENT_QUEUE_CONTINUOUS) {
        assert(q->tail == NULL);
        err = waitset_chan_deregister(&q->waitset_state);
        if (err_is_fail(err)) {
            // can fail if the event already fired, but this is ok
            if (err_no(err) == LIB_ERR_CHAN_NOT_REGISTERED) {
                err = SYS_ERR_OK;
            }
        }
    }
    return err;
}

/**
 * \brief Add a new event to an event queue
 *
 * \param q Event queue
 * \param qn Storage for queue node (uninitialised)
 * \param event Event closure
 */
void event_queue_add(struct event_queue *q, struct event_queue_node *qn,
                     struct event_closure event)
{
    errval_t err;

    qn->event = event;
    qn->run = false;

    thread_mutex_lock(&q->mutex);

    // enqueue at tail
    if (q->tail == NULL) {
        assert(q->head == NULL);
        qn->next = qn->prev = NULL;
        q->head = q->tail = qn;

        // was empty: need to trigger queue runner if in continuous mode
        if (q->mode == EVENT_QUEUE_CONTINUOUS) {
            struct event_closure runner = {
                .handler = event_queue_runner,
                .arg = q
            };
            err = waitset_chan_trigger_closure(q->waitset, &q->waitset_state,
                                               runner);
            // apparently there's a situation when we dont really need to
            // trigger the queue runner here, as we can get an
            // LIB_ERR_CHAN_ALREADY REGISTERED error from the call.
            // -SG, 2013-07-31
            assert(err_is_ok(err) ||
                   err_no(err) == LIB_ERR_CHAN_ALREADY_REGISTERED);
        }
    } else {
        assert(q->tail != qn); // don't re-enqueue the same node!
        assert(q->tail->next == NULL);
        q->tail->next = qn;
        qn->prev = q->tail;
        qn->next = NULL;
        q->tail = qn;

        // runner is already active if it needs to be, don't need to do anything
    }

    thread_mutex_unlock(&q->mutex);
}

/**
 * \brief Cancel an event previously added to an event queue
 *
 * \param q Event queue
 * \param qn Queue node which was previously added to #q by event_queue_add()
 */
errval_t event_queue_cancel(struct event_queue *q, struct event_queue_node *qn)
{
    errval_t err = SYS_ERR_OK;

    if (qn->run) {
        return LIB_ERR_EVENT_ALREADY_RUN;
    }

    thread_mutex_lock(&q->mutex);

    if (qn->run) {
        thread_mutex_unlock(&q->mutex);
        return LIB_ERR_EVENT_ALREADY_RUN;
    }

    // dequeue
    if (qn->next == NULL) {
        assert(q->tail == qn);
        q->tail = qn->prev;
    } else {
        qn->next->prev = qn->prev;
    }

    if (qn->prev == NULL) {
        assert(q->head == qn);
        q->head = qn->next;
    } else {
        qn->prev->next = qn->next;
    }

    // if the queue is now empty, we should cancel the runner
    err = event_queue_cancel_runner(q);

    thread_mutex_unlock(&q->mutex);

    return err;
}

/**
 * \brief Flush all pending events from the event queue.
 *
 * \param q Event queue.
 */
errval_t
event_queue_flush(struct event_queue *q) {
    errval_t err = SYS_ERR_OK;

    thread_mutex_lock(&q->mutex);

    struct event_queue_node *qn;
    do {
        qn = next_event(q);
    } while (qn);

    err = event_queue_cancel_runner(q);

    thread_mutex_unlock(&q->mutex);

    return err;
}

/**
 * \brief Trigger the next event on a queue which is operating in one-shot mode
 *
 * Must not be called before the previously-triggered event has run.
 *
 * \param q Event queue
 */
errval_t event_queue_trigger(struct event_queue *q)
{
    assert(q->mode == EVENT_QUEUE_ONESHOT);

    thread_mutex_lock(&q->mutex);

    struct event_queue_node *qn = next_event(q);

    if (qn == NULL) {
        thread_mutex_unlock(&q->mutex);
        return LIB_ERR_EVENT_QUEUE_EMPTY;
    }

    qn->run = true;
    thread_mutex_unlock(&q->mutex);

    // trigger closure on waitset
    return waitset_chan_trigger_closure(q->waitset, &q->waitset_state, qn->event);
}
