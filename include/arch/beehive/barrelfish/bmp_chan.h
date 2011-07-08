/**
 * \file
 * \brief Bidirectional Beehive messaging channel
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_BMP_CHAN_H
#define BARRELFISH_BMP_CHAN_H

#include <barrelfish/waitset.h>
#include <barrelfish/lmp_endpoints.h>
#include <barrelfish/syscall_arch.h>
#include <barrelfish_kpi/bmp.h>
#include <barrelfish/monitor_client.h>
#include <trace/trace.h>

struct bmp_chan;

/// Default size of BMP endpoint buffer (in words), must be >= BMP_MSG_LENGTH
// We make this reasonably large, because the flow control logic sends
// an ack whenever there is less than BMP_MSG_LENGTH space remaining.
#define DEFAULT_BMP_BUF_WORDS           (BMP_MSG_LENGTH * 2)

struct bmp_bind_continuation {
    /**
     * \brief Handler which runs when a binding succeeds or fails
     * \param st State pointer set in closure
     * \param err Success/failure of binding
     * \param c On success, contains pointer to channel
     */
    void (*handler)(void *st, errval_t err, struct bmp_chan *c);
    void *st;
};

/// A bidirectional Beehive channel
struct bmp_chan {
    struct monitor_cap_handlers cap_handlers;   /* XXX: must be first */

    struct bmp_chan *next, *prev;  ///< Next/prev in list of channels with send events
    struct capref outepcap;        ///< Cap to remote Beehive endpoint
    size_t outeplen;               ///< Size of remote endpoint buffer
    struct lmp_endpoint *inep;     ///< Incoming LMP endpoint
    struct capref inepcap;         ///< Cap to incoming LMP endpoint
    struct waitset_chanstate send_waitset; ///< State belonging to waitset (for send)

    /// connection state
    enum {BMP_DISCONNECTED,     ///< Disconnected
          BMP_BIND_WAIT,        ///< Waiting for bind reply
          BMP_CONNECTED,        ///< Connection established
    } connstate;

    uintptr_t monitor_id;       ///< Local monitor's connection ID for this channel
    struct monitor_binding *monitor_binding; ///< Monitor binding used for cap xfer

    /* Arguments for an ongoing bind attempt */
    iref_t iref;                ///< IREF to which we bound
    struct bmp_bind_continuation bind_continuation; ///< Continuation for bind
};

/// Fixed-length version of #lmp_recv_buf
struct bmp_recv_msg {
    struct lmp_recv_buf buf;
    uintptr_t words[BMP_MSG_LENGTH]; ///< Payload (fixed length)
};

/// Static initialiser for lmp_recv_msg
#define BMP_RECV_MSG_INIT { .buf.buflen = BMP_MSG_LENGTH }

struct event_queue_node;

void bmp_chan_init(struct bmp_chan *c);
errval_t bmp_chan_bind(struct bmp_chan *c, struct bmp_bind_continuation cont,
                       struct event_queue_node *qnode,  iref_t iref,
                       struct monitor_binding *monitor_binding,
                       size_t ep_buflen);
errval_t bmp_chan_accept(struct bmp_chan *c, uintptr_t mon_id,
                         struct capref remote_ep, size_t remote_ep_len,
                         size_t local_ep_len);
void bmp_chan_send_bind_reply(struct monitor_binding *mb,
                              struct bmp_chan *c, errval_t err,
                              uintptr_t monitor_id);
errval_t bmp_chan_register_send(struct bmp_chan *c, struct waitset *ws,
                                struct event_closure closure);
errval_t bmp_chan_deregister_send(struct bmp_chan *c);
void bmp_chan_destroy(struct bmp_chan *c);
void bmp_channels_retry_send_disabled(dispatcher_handle_t handle);
void bmp_init(void);

/**
 * \brief Register an event handler to be notified when messages can be received
 *
 * In the future, call the closure on the given waitset when it is likely that
 * a message can be received on the channel. A channel may only be registered
 * with a single receive event handler on a single waitset at any one time.
 *
 * \param c channel
 * \param ws Waitset
 * \param closure Event handler
 */
static inline errval_t bmp_chan_register_recv(struct bmp_chan *c,
                                              struct waitset *ws,
                                              struct event_closure closure)
{
    return lmp_endpoint_register(c->inep, ws, closure);
}

/**
 * \brief Cancel an event registration made with bmp_chan_register_recv()
 *
 * \param c channel
 */
static inline errval_t bmp_chan_deregister_recv(struct bmp_chan *c)
{
    return lmp_endpoint_deregister(c->inep);
}

/**
 * \brief Receive a message from a BMP channel, if possible
 *
 * Non-blocking. May fail if no message is available.
 *
 * \param c  channel
 * \param msg BMP message buffer, to be filled-in
 */
static inline errval_t bmp_chan_recv(struct bmp_chan *c,
                                     struct bmp_recv_msg *msg)
{
    assert(msg != NULL);
    assert(msg->buf.buflen == BMP_MSG_LENGTH);
    return lmp_endpoint_recv(c->inep, &msg->buf, NULL);
}

/**
 * \brief Send a message to a BMP channel, if possible
 *
 * Non-blocking. May fail if the channel cannot currently transmit.
 *
 * \param c  channel
 * \param ptr Message payload as an array of words
 * \param len Length of message payload, in words
 */
static inline errval_t bmp_chan_send(struct bmp_chan *c,
                                     uintptr_t *ptr, size_t len)
{
    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_BMP_SEND, (uintptr_t)c);

    assert(c != NULL);
    assert(ptr != NULL);
    assert(len > 0 && len <= BMP_MSG_LENGTH);

    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, len);
    idc_msg_encode_word(&msg, (uintptr_t)ptr);

    errval_t err;
    err = cap_invoke(c->outepcap, &msg).error;
    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_BMP_SEND, ((uintptr_t)c)|1);
    return err;
}

#endif // BARRELFISH_BMP_CHAN_H
