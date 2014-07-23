/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#include <barrelfish/barrelfish.h>
#include <flounder/flounder.h>
#include <flounder/flounder_txqueue.h>


static void send_cont(void *arg)
{
    struct txq_msg_st *st = arg;
    st->send(st);
}

/*
 * ===========================================================================
 * Public Interface
 * ===========================================================================
 */

/**
 * \brief initializes a tx_queue to be used for a Flounder binding
 *
 * \param queue         TX queue to be initialized
 * \param binding       Flounder binding
 * \param waitset       the waitset to be used
 * \param register_send register send function of the binding
 * \param msg_st_size   size of the message state elements of this queue
 */
void txq_init(struct tx_queue *queue,
              void *binding,
              struct waitset *waitset,
              txq_register_fn_t register_send,
              uint32_t msg_st_size)
{
    assert(msg_st_size >= sizeof(struct txq_msg_st));
    assert(binding);
    assert(waitset);
    assert(register_send);

    queue->binding = binding;
    queue->register_send = register_send;
    queue->msg_st_size = msg_st_size;
    queue->waitset = waitset;
    queue->head = NULL;
    queue->tail = NULL;
    queue->free = NULL;
}

/**
 * \brief allocates new message state for an outgoing flounder message
 *
 * \param txq   TX queue to allocate from
 *
 * \returns mx_mst_st on success
 *          NULL on failure
 */
struct txq_msg_st *txq_msg_st_alloc(struct tx_queue *txq)
{
    struct txq_msg_st *st;

    if (txq->free != NULL) {
        st = txq->free;
        txq->free = st->next;
        return st;
    }
    st = calloc(1, txq->msg_st_size);
    if (st == NULL) {
        return NULL;
    }

    st->queue = txq;

    return st;
}

/**
 * \brief frees up an unused message state
 *
 * \param st    txq message state to be freed
 */
void txq_msg_st_free(struct txq_msg_st *st)
{
    st->next = st->queue->free;
    st->queue->free = st;
}

/**
 * \brief handler to be called when the message was sent successfully
 *
 * \param st    TX queue message state
 */
void txq_sent_cb(void *arg)
{
    struct txq_msg_st *st = arg;
    struct tx_queue *q = st->queue;
    assert(q->head == st);

    if (st->cleanup) {
        st->cleanup(st);
    }

    q->head = q->head->next;
    if (q->head == NULL) {
        q->tail = NULL;
    } else {
        st = q->head;
        q->register_send(q->binding, q->waitset, MKCONT(send_cont, st));
    }
    txq_msg_st_free(st);
}

/**
 * \brief sends the Flounder message
 *
 * \param st    txq message state of the message
 */
void txq_send(struct txq_msg_st *st)
{
    assert(st->queue);

    struct tx_queue *q = st->queue;

    st->next = NULL;
    if (q->tail == NULL) {
        q->head = st;
    } else {
        q->tail->next = st;
    }
    q->tail = st;

    /* queue was empty so we should succeed in sending */
    if (q->tail == q->head) {
        st->send(st);
    }
}
