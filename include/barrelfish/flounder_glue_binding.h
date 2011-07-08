/**
 * \file
 * \brief Glue binding for old Flounder, can send/receive arbitrary-length messages
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_FLOUNDER_GLUE_BINDING_H
#define BARRELFISH_FLOUNDER_GLUE_BINDING_H

#include <barrelfish/idc.h>
#include <barrelfish/msgbuf.h>
#include <barrelfish/idc_export.h>
#include <barrelfish/event_queue.h>

struct flounder_glue_binding;

struct flounder_glue_bind_continuation {
    /**
     * \brief Handler which runs when a binding succeeds or fails
     * \param st State pointer set in closure
     * \param err Success/failure of binding
     * \param b On success, contains pointer to binding
     */
    void (*handler)(void *st, errval_t err, struct flounder_glue_binding *b);
    void *st;
};

struct flounder_glue_binding {
    struct waitset *waitset; ///< Waitset for receive handlers and send continuations
    void *st; ///< Arbitrary state pointer passed to message handlers

    /**
     * \brief message send function
     *
     * \param b Binding object
     * \param continuation Continuation to be run once send completes
     * \param msg Message payload to send (see note on buffer ownership)
     * \param msglen Length of message payload
     * \param caps Array of capability to send (see note on buffer ownership)
     * \param caps_tm Array of transport mode for caps being sent 
     *                (true for give_awya, false for copy
     * \param ncaps Number of capability references in caps array
     *
     * \note Any buffers passed by the caller in the msg and caps parameters
     * are owned by the binding until the message is sent, and may not be
     * re-used or freed until a subsequent send operation is possible (ie.
     * either a successful call to send() or can_send() returns true).
     */
    errval_t (*send)(struct flounder_glue_binding *b,
                     struct event_closure continuation,
                     const void *msg, size_t msglen,
                     const struct capref *caps,
                     size_t ncaps);

    /// returns true iff a message could currently be sent on the binding
    bool (*can_send)(struct flounder_glue_binding *b);

    /// register an event for when a message is likely to be able to be sent
    errval_t (*register_send)(struct flounder_glue_binding *b,
                              struct waitset *ws, struct event_closure cl);

    /// initiate connection teardown (NYI)
    //void (*teardown)(struct flounder_glue_binding *b);

    /// set generic IDC flags (NYI)
    //void (*set_flags)(struct flounder_glue_binding *b, idc_flags_t flags);

    /**
     * \brief callback for incoming messages (filled in by user)
     *
     * \param st State pointer
     * \param msgbuf Message buffer structure (must be destroyed by callee)
     */
    void (*receive_handler)(void *st, struct msgbuf *msgbuf);

    /**
     * \brief callback for asynchronous error handling (filled in by user)
     *
     * \param st State pointer
     * \param err Error code
     */
    void (*error_handler)(void *st, errval_t err);

    /* ------------------ private state of implementation ------------------ */

    struct waitset_chanstate send_waitset, sendcont_waitset;

    // message currently being sent
    const uint8_t *sendbuf;
    size_t sendpos, sendlen;

    // caps currently being sent
    const struct capref *sendcaps;
    size_t sendcappos, nsendcaps;

    // message currently being received
    uint8_t *recvbuf;
    size_t recvpos, recvlen;

    // caps currently being received
    struct capref *recvcaps;
    size_t recvcappos, nrecvcaps;

    // msgbuf struct for interacting with old flounder code
    struct msgbuf recvmb;

    // bind continuation
    struct flounder_glue_bind_continuation bind_continuation;

    struct event_queue_node event_qnode;
};

/// Utility send function for wrapping msgbuf objects
static inline errval_t flounder_glue_binding_send_msgbuf(struct flounder_glue_binding *b,
                                                         struct event_closure continuation,
                                                         struct msgbuf *msg)
{
    return b->send(b, continuation, msg->buf, msg->pos, msg->caps, msg->cap_pos);
}

/**
 * \brief Callback handler for when a new connection is attempted on an export
 *
 * This function is called with a new binding whenever a connection is
 * attempted to an exported service. If it returns success, the binding
 * is established; if it returns failure, the connection is refused.
 *
 * When this function runs, the binding is still in the
 * disconnected/unbound state. If the user attempts to send on the binding,
 * the results are undefined. A user wishing to send a message on the
 * binding once it is established should use register_send().
 *
 * \param st Closure state pointer
 * \param b  Generic binding
 */
typedef errval_t flounder_glue_connect_callback_fn(void *st, struct flounder_glue_binding *b);

/// Private state for a generic service export
struct flounder_glue_export {
    struct idc_export export;
    flounder_glue_connect_callback_fn *connect_callback;
    struct waitset *ws;
    void *st;
};

errval_t flounder_glue_export(struct flounder_glue_export *e,
                        idc_export_callback_fn *export_callback,
                        flounder_glue_connect_callback_fn *connect_callback,
                        void *st, struct waitset *ws, idc_export_flags_t flags);

errval_t flounder_glue_bind(iref_t iref, struct waitset *ws,
                            struct flounder_glue_bind_continuation cont);

#ifdef CONFIG_INTERCONNECT_DRIVER_LMP

#include <barrelfish/lmp_chan.h>

// concrete implementation for LMP
struct flounder_glue_lmp_binding {
    struct flounder_glue_binding common;
    struct lmp_chan chan;
    lmp_send_flags_t lmpflags;
};

errval_t flounder_glue_lmp_bind(struct flounder_glue_lmp_binding *b, struct waitset *ws,
                          struct flounder_glue_bind_continuation cont,
                          iref_t iref, size_t buflen_words);

#endif // CONFIG_INTERCONNECT_DRIVER_LMP

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP

#include <barrelfish/ump_chan.h>

// concrete implementation for UMP
struct flounder_glue_ump_binding {
    struct flounder_glue_binding common;
    struct ump_chan chan;

    ump_index_t sent_id;   ///< Sequence number of next message to be sent
    ump_index_t seq_id;    ///< Last sequence number received from remote
    ump_index_t ack_id;    ///< Last sequence number acknowledged by remote
    ump_index_t last_ack;  ///< Last acknowledgement we sent to remote

    enum {
        UMP_SEND_STATE_IDLE,        ///< nothing doing
        UMP_SEND_STATE_HEADER,      ///< sending the start-of-message header
        UMP_SEND_STATE_DATA_ONLY,   ///< sending data only (waiting for cap ack)
        UMP_SEND_STATE_DATA_CAPS,   ///< sending data and/or caps
    } send_state;

    enum {
        UMP_RECV_STATE_IDLE,        ///< nothing doing
        UMP_RECV_STATE_SEND_CAP_ACK,///< seen header, waiting to send cap ack
        UMP_RECV_STATE_DATA_CAPS,   ///< receiving data and/or caps
    } recv_state;
};

errval_t flounder_glue_ump_bind(struct flounder_glue_ump_binding *b, struct waitset *ws,
                          struct flounder_glue_bind_continuation cont,
                          iref_t iref, size_t inchanlen, size_t outchanlen);

#endif // CONFIG_INTERCONNECT_DRIVER_UMP

#endif // BARRELFISH_FLOUNDER_GLUE_BINDING_H
