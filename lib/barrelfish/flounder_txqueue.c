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
    TXQ_DEBUG("send_cont: sending message %p\n", arg);

    struct txq_msg_st *st = arg;
    errval_t err = st->send(st);
    /* this function is called form regsiter_send callback sending shoudl not fail*/
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send even in register_send callback\n");
    }
}

static inline void try_send(struct txq_msg_st *st)
{
    struct tx_queue *q = st->queue;
    assert(q->head == st);

    errval_t err = st->send(st);

    TXQ_DEBUG("try_send: st=%p, #queued=%u, err=%s\n", st, q->queue_count,
              err_getstring(err));

    switch (err_no(err)) {
        case SYS_ERR_OK:
            return;
        case FLOUNDER_ERR_TX_BUSY:
            /* registering here should not fail */
            err = q->register_send(q->binding, q->waitset,
                                   MKCONT(send_cont, q->head));
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not register.\n");
            }
            break;
        default:
            USER_PANIC_ERR(err, "error while sending");
            break;
    }
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
 * \param can_send      can send function of the binding
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
#ifdef FLOUNDER_TXQUEUE_DEBUG
    queue->alloc_count = 0;
    queue->free_count = 0;
    queue->queue_count = 0;
#endif
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

        TXQ_ASSERT(txq->free_count);
        TXQ_OP(txq->free_count--);
        TXQ_DEBUG("txq_mst_st_alloc: reusing msg st %p. free [%u / %u]\n", st,
                  txq->free_count, txq->alloc_count);

        st->next = NULL;
        return st;
    }

    TXQ_OP(txq->alloc_count++);
    TXQ_ASSERT(txq->free_count == 0);

    st = calloc(1, txq->msg_st_size);
    if (st == NULL) {
        return NULL;
    }

    TXQ_DEBUG("txq_msg_st_alloc: allocating new msg st %p. #allocated: %u\n", st,
              txq->alloc_count);

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
    struct tx_queue *q = st->queue;

    TXQ_ASSERT(!(q->free_count == 0) || (q->free == NULL));
    TXQ_OP(st->queue->free_count++);
    TXQ_DEBUG("txq_mst_st_free: %p, free msg states %p [ %u / %u ]\n", st, q->free,
              st->queue->free_count, st->queue->alloc_count);

    st->next = q->free;
    q->free = st;
}

/**
 * \brief handler to be called when the message was sent successfully
 *
 * \param st    TX queue message state
 */
void txq_sent_cb(void *arg)
{
    errval_t err;

    struct txq_msg_st *st = arg;
    struct tx_queue *q = st->queue;
    assert(q->head == st);
    TXQ_OP(q->queue_count--);

    if (st->cleanup) {
        st->cleanup(st);
    }

    q->head = q->head->next;

    if (q->head == NULL) {
        q->tail = NULL;

        TXQ_ASSERT(q->queue_count == 0);
        TXQ_DEBUG("txq_sent_cb: st=%p, head=%p, tail=%p, count=%u\n", st, q->head,
                  q->tail, q->queue_count);
    } else {
        TXQ_ASSERT(q->queue_count);
        TXQ_DEBUG("txq_sent_cb: st=%p, register sending head=%p, tail=%p, count=%u\n",
                  st, q->head, q->tail, q->queue_count);

        err = q->register_send(q->binding, q->waitset, MKCONT(send_cont, q->head));
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "could not register for sending!\n");
        }
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
        TXQ_ASSERT(q->queue_count == 0);
    } else {
        q->tail->next = st;
        TXQ_ASSERT(q->queue_count);
    }
    q->tail = st;

    TXQ_OP(q->queue_count++);
    TXQ_DEBUG("txq_send: st=%p, head=%p, tail=%p, count=%u\n", st, q->head, q->tail,
              q->queue_count);

    if (q->tail == q->head) {
        TXQ_ASSERT(q->queue_count == 1);
        try_send(st);
    }
}
