/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __FLOUNDER_TXQUEUE_H
#define __FLOUNDER_TXQUEUE_H

#include <barrelfish/waitset.h>

//#define FLOUNDER_TXQUEUE_DEBUG 1

#ifdef FLOUNDER_TXQUEUE_DEBUG
#define TXQ_DEBUG(x...) debug_printf("[TXQ]" x);
#define TXQ_ASSERT(x) assert(x)
#define TXQ_OP(x) x
#else
#define TXQ_DEBUG(x...)
#define TXQ_ASSERT(x)
#define TXQ_OP(x)
#endif

struct tx_queue;
struct txq_msg_st;

/// Utility macro to construct a continuation structure (handler & arg)
#define TXQCONT(a) MKCLOSURE(txq_sent_cb,a)

/// typedef for the send handler function. argument is struct txq_msg_st*
typedef errval_t (*txq_send_fn_t)(struct txq_msg_st *st);

/// typedef for cleanup function. argument is struct txq_msg_st*
typedef void (*txq_cleanup_fn_t)(struct txq_msg_st *st);

typedef errval_t (*txq_register_fn_t)(void *binding,
                                      struct waitset *ws,
                                      struct event_closure _continuation);

struct tx_queue
{
    void *binding;                   ///< binding
    txq_register_fn_t register_send; ///< function to register sending
    struct waitset *waitset;         ///< waitset to use
    struct txq_msg_st *head;         ///< head of the queue
    struct txq_msg_st *tail;         ///< tail of the queue
    struct txq_msg_st *free;         ///< free list of message states
    uint32_t msg_st_size;            ///< size of the message state
#ifdef FLOUNDER_TXQUEUE_DEBUG
    uint32_t free_count;
    uint32_t alloc_count;
    uint32_t queue_count;
#endif
};

struct txq_msg_st
{
    struct txq_msg_st *next;         ///< pointer to the next message in the queue
    struct tx_queue *queue;          ///< queue this element belongs to
    txq_send_fn_t send;              ///< send handler function
    txq_cleanup_fn_t cleanup;        ///< cleanup function to be called
    errval_t err;                    ///< error error status
    /* user state follows */
};


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
              uint32_t msg_st_size);

/**
 * \brief allocates new message state for an outgoing flounder message
 *
 * \param txq   TX queue to allocate from
 *
 * \returns mx_mst_st on success
 *          NULL on failure
 */
struct txq_msg_st *txq_msg_st_alloc(struct tx_queue *txq);

/**
 * \brief frees up an unused message state
 *
 * \param st    txq message state to be freed
 */
void txq_msg_st_free(struct txq_msg_st *st);

/**
 * \brief handler to be called when the message was sent successfully
 *
 * \param arg    TX queue message state
 */
void txq_sent_cb(void *arg);

/**
 * \brief sends the Flounder message
 *
 * \param st    txq message state of the message
 */
void txq_send(struct txq_msg_st *st);

#endif // __FLOUNDER_TXQUEUE_H
