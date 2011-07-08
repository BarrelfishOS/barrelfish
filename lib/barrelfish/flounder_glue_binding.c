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

#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/flounder_glue_binding.h>
#include "waitset_chan.h"
#include <if/monitor_defs.h>

/* ------------ functions common to all transports ------------ */

static inline uintptr_t getword(const uint8_t *buf, size_t *pos, size_t len)
{
    uintptr_t word = 0;

    for (int i = 0; *pos < len && i < sizeof(uintptr_t); i++) {
        // read and shift in next byte
        word <<= NBBY;
        word |= buf[(*pos)++];
    }

    return word;
}

static inline void putword(uintptr_t word, uint8_t *buf, size_t *pos, size_t len)
{
    const size_t shift_bits = (sizeof(uintptr_t) - 1) * NBBY;

    // throw away leading zeros if this is the end of the message
    if (len - *pos < sizeof(uintptr_t)) {
        word <<= NBBY * (sizeof(uintptr_t) - (len - *pos));
    }

    for (int i = 0; *pos < len && i < sizeof(uintptr_t); i++) {
        buf[(*pos)++] = (word & ((uintptr_t)0xff << shift_bits)) >> shift_bits;
        word <<= NBBY;
    }
}

static bool can_send(struct flounder_glue_binding *g)
{
    return g->sendbuf == NULL && g->sendcaps == NULL;
}

static errval_t register_send(struct flounder_glue_binding *g, struct waitset *ws,
                              struct event_closure cl)
{
    return waitset_chan_register(ws, &g->send_waitset, cl);
}

static void flounder_glue_binding_init(struct flounder_glue_binding *g, struct waitset *ws)
{
    g->sendbuf = NULL;
    g->sendlen = 0;
    g->sendcaps = NULL;
    g->nsendcaps = 0;
    g->recvbuf = NULL;
    g->recvcaps = NULL;

    g->can_send = can_send;
    g->register_send = register_send;

    g->receive_handler = NULL;
    g->error_handler = NULL;

    g->waitset = ws;
    waitset_chanstate_init(&g->send_waitset, CHANTYPE_OTHER);
    waitset_chanstate_init(&g->sendcont_waitset, CHANTYPE_OTHER);
}

static void flounder_glue_binding_destroy(struct flounder_glue_binding *g)
{
    waitset_chanstate_destroy(&g->send_waitset);
    waitset_chanstate_destroy(&g->sendcont_waitset);
}

/* ------------------------ LMP implementation ------------------------ */

#ifdef CONFIG_INTERCONNECT_DRIVER_LMP

static void lmp_receive_handler(void *arg)
{
    struct flounder_glue_lmp_binding *b = arg;
    struct flounder_glue_binding *g = &b->common;

    struct event_closure receive_closure = {
        .handler = lmp_receive_handler,
        .arg = b,
    };

    // try to retrieve a message from the channel
    struct lmp_recv_msg msg = LMP_RECV_MSG_INIT;
    struct capref cap;
    errval_t err;

    while (1) {
        err = lmp_chan_recv(&b->chan, &msg, &cap);
        if (err_is_fail(err)) {
            if (err_no(err) == LIB_ERR_NO_LMP_MSG) {
                // nothing there
                break;
            } else {
                // real error, report to user
                g->error_handler(g->st, err_push(err, LIB_ERR_LMP_CHAN_RECV));
                return;
            }
        }

        // allocate a new receive slot if needed
        if (!capref_is_null(cap)) {
            err = lmp_chan_alloc_recv_slot(&b->chan);
            if (err_is_fail(err)) {
                g->error_handler(g->st,
                                 err_push(err, LIB_ERR_LMP_ALLOC_RECV_SLOT));
            }
        }

        // is this the first fragment of a message?
        int firstword;
        if (g->recvbuf == NULL && g->recvcaps == NULL) { // first fragment
            // find length, and allocate space for message
            assert(msg.buf.msglen > 1);
            g->recvlen = msg.words[0];
            g->recvpos = 0;
            if (g->recvlen > 0) {
                g->recvbuf = malloc(g->recvlen);
                assert(g->recvbuf != NULL);
            } else {
                g->recvbuf = NULL;
            }

            g->nrecvcaps = msg.words[1];
            g->recvcappos = 0;
            if (g->nrecvcaps > 0) {
                g->recvcaps = malloc(sizeof(struct capref) * g->nrecvcaps);
                assert(g->recvcaps != NULL);
            } else {
                g->recvcaps = NULL;
            }

            firstword = 2; // skip header
        } else {
            firstword = 0;
        }

        // store a cap if we have one
        if (g->recvcappos < g->nrecvcaps) {
            assert(!capref_is_null(cap));
            g->recvcaps[g->recvcappos++] = cap;
        } else {
            assert(capref_is_null(cap));
        }

        if (g->recvpos < g->recvlen) {
            // copy remainder of fragment to buffer
            for (int i = firstword; i < msg.buf.msglen; i++) {
                putword(msg.words[i], g->recvbuf, &g->recvpos, g->recvlen);
            }
        }

        // complete message, deliver to user
        if (g->recvpos == g->recvlen && g->recvcappos == g->nrecvcaps) {
            msgbuf_init_static(&g->recvmb, g->recvbuf, g->recvlen,
                               g->recvcaps, g->nrecvcaps);
            g->recvmb.dynamic = true; // XXX
            g->receive_handler(g->st, &g->recvmb);
            g->recvbuf = NULL;
            g->recvcaps = NULL;
            g->recvpos = g->recvlen = 0;
            g->recvcappos = g->nrecvcaps = 0;
        }
    }

    // re-register for another receive notification
    err = lmp_chan_register_recv(&b->chan, g->waitset, receive_closure);
    if (err_is_fail(err)) {
        g->error_handler(g->st, err_push(err, LIB_ERR_CHAN_REGISTER_RECV));
    }
}

static void lmp_send_handler(void *arg);

static errval_t do_lmp_send(struct flounder_glue_lmp_binding *b,
                            const uint8_t *buf, size_t pos, size_t len,
                            const struct capref *caps, size_t cappos, size_t ncaps)
{
    struct flounder_glue_binding *g = &b->common;
    errval_t err;

    while (pos < len || cappos < ncaps) {
        // store current position in buffer for restarting if this fails
        size_t restart_pos = pos;
        size_t restart_cappos = cappos;

        size_t total_words = DIVIDE_ROUND_UP(len - pos, sizeof(uintptr_t));
        if (pos == 0 && cappos == 0) { // space for header
            total_words += 2;
        }

        size_t chunk_words = total_words > LMP_MSG_LENGTH
                                ? LMP_MSG_LENGTH : total_words;

#if LMP_MSG_LENGTH < 2
#error Cannot support such an insane transport!
#endif

        uintptr_t m1, m2;
        if (pos == 0 && cappos == 0) { // send header in first two words
            m1 = len;
            m2 = ncaps;
        } else {
            m1 = chunk_words < 1 ? 0 : getword(buf, &pos, len);
            m2 = chunk_words < 2 ? 0 : getword(buf, &pos, len);
        }

#if LMP_MSG_LENGTH > 2
        uintptr_t m3 = chunk_words < 3 ? 0 : getword(buf, &pos, len);
#endif
#if LMP_MSG_LENGTH > 3
        uintptr_t m4 = chunk_words < 4 ? 0 : getword(buf, &pos, len);
#endif
#if LMP_MSG_LENGTH > 4
        uintptr_t m5 = chunk_words < 5 ? 0 : getword(buf, &pos, len);
#endif
#if LMP_MSG_LENGTH > 5
        uintptr_t m6 = chunk_words < 6 ? 0 : getword(buf, &pos, len);
#endif
#if LMP_MSG_LENGTH > 6
        uintptr_t m7 = chunk_words < 7 ? 0 : getword(buf, &pos, len);
#endif
#if LMP_MSG_LENGTH > 7
        uintptr_t m8 = chunk_words < 8 ? 0 : getword(buf, &pos, len);
#endif
#if LMP_MSG_LENGTH > 8
        uintptr_t m9 = chunk_words < 9 ? 0 : getword(buf, &pos, len);
#endif
#if LMP_MSG_LENGTH > 9
        uintptr_t m10 = chunk_words < 10 ? 0 : getword(buf, &pos, len);
#endif
#if LMP_MSG_LENGTH > 10
#error Need to unroll message send loop further
#endif

        struct capref sendcap;
        if (cappos < ncaps) {
            sendcap = caps[cappos++];
        } else {
            sendcap = NULL_CAP;
        }

        err = lmp_chan_send(&b->chan, b->lmpflags, sendcap, chunk_words,
                            m1, m2
#if LMP_MSG_LENGTH > 2
                            , m3
#endif
#if LMP_MSG_LENGTH > 3
                            , m4
#endif
#if LMP_MSG_LENGTH > 4
                            , m5
#endif
#if LMP_MSG_LENGTH > 5
                            , m6
#endif
#if LMP_MSG_LENGTH > 6
                            , m7
#endif
#if LMP_MSG_LENGTH > 7
                            , m8
#endif
#if LMP_MSG_LENGTH > 8
                            , m9
#endif
#if LMP_MSG_LENGTH > 9
                            , m10
#endif
                            );

        if (lmp_err_is_transient(err)) {
            // transient error, queue rest of message
            g->sendbuf = buf;
            g->sendpos = restart_pos;
            g->sendlen = len;
            g->sendcaps = caps;
            g->sendcappos = restart_cappos;
            g->nsendcaps = ncaps;
            struct event_closure callback = {
                .handler = lmp_send_handler,
                .arg = b,
            };
            err = lmp_chan_register_send(&b->chan, g->waitset, callback);
            if (err_is_fail(err)) {
                 return err_push(err, LIB_ERR_CHAN_REGISTER_SEND);
            }
            return SYS_ERR_OK; // queued
        } else if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_LMP_CHAN_SEND);
        }
    }

    // finished!
    g->sendbuf = NULL;
    g->sendcaps = NULL;

    // trigger send continuation if registered
    if (waitset_chan_is_registered(&g->sendcont_waitset)) {
        err = waitset_chan_trigger(&g->sendcont_waitset);
        assert(err_is_ok(err)); // shouldn't fail
    }

    // trigger send waitset if registered
    if (waitset_chan_is_registered(&g->send_waitset)) {
        err = waitset_chan_trigger(&g->send_waitset);
        assert(err_is_ok(err)); // shouldn't fail
    }

    return SYS_ERR_OK;
}

static void lmp_send_handler(void *arg)
{
    struct flounder_glue_binding *g = arg;

    assert(g);
    assert(g->sendbuf != NULL || g->sendcaps != NULL);

    errval_t err = do_lmp_send((struct flounder_glue_lmp_binding *)g,
                               g->sendbuf, g->sendpos, g->sendlen,
                               g->sendcaps, g->sendcappos, g->nsendcaps);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "while transmitting more fragments of queued message");
        // fatal, discard rest of queued message
        g->sendbuf = NULL;
        g->sendcaps = NULL;
        // signal error to user
        g->error_handler(g->st, err);
    }
}

static errval_t lmp_send(struct flounder_glue_binding *g,
                         struct event_closure continuation,
                         const void *msg, size_t len,
                         const struct capref *caps, size_t ncaps)
{
    struct flounder_glue_lmp_binding *b = (struct flounder_glue_lmp_binding *)g;
    errval_t err;

    // messages already queued?
    if (!can_send(g)) {
        return FLOUNDER_ERR_TX_BUSY;
    }

    // register send continuation
    if (continuation.handler != NULL) {
        err = waitset_chan_register(g->waitset, &g->sendcont_waitset,
                                    continuation);
        assert(err_is_ok(err)); // shouldn't fail if we have no messages queued
    }

    return do_lmp_send(b, msg, 0, len, caps, 0, ncaps);
}

static void lmp_bind_continuation(void *st, errval_t err, struct lmp_chan *lc)
{
    struct flounder_glue_lmp_binding *b = st;
    struct flounder_glue_binding *g = &b->common;
    // just pass it on to the user
    assert(g->bind_continuation.handler != NULL);
    g->bind_continuation.handler(g->bind_continuation.st, err, g);
    if (err_is_fail(err)) {
        flounder_glue_binding_destroy(g);
        return;
    }

    // allocate a cap receive slot
    err = lmp_chan_alloc_recv_slot(&b->chan);
    if (err_is_fail(err)) {
        g->error_handler(g->st, err_push(err, LIB_ERR_LMP_ALLOC_RECV_SLOT));
    }

    /* register for receiving */
    struct event_closure receive_closure = {
        .handler = lmp_receive_handler,
        .arg = b,
    };
    err = lmp_chan_register_recv(&b->chan, g->waitset, receive_closure);
    if (err_is_fail(err)) {
        g->error_handler(g->st, err_push(err, LIB_ERR_CHAN_REGISTER_RECV));
    }
}

static void lmp_binding_init(struct flounder_glue_lmp_binding *b, struct waitset *ws)
{
    struct flounder_glue_binding *g = &b->common;
    flounder_glue_binding_init(g, ws);
    b->lmpflags = LMP_SEND_FLAGS_DEFAULT;
    g->send = lmp_send;
}

static void lmp_binding_destroy(struct flounder_glue_lmp_binding *b)
{
    flounder_glue_binding_destroy(&b->common);
}

/**
 * \brief Initialise and attempt to bind a new generic LMP binding
 *
 * \param b       Storage for binding state
 * \param ws      Waitset on which to register for receive handling
 * \param cont    Continuation closure for bind completion
 * \param iref    IREF to which to bind
 * \param buflen_words Size of incoming buffer, in number of words
 */
errval_t flounder_glue_lmp_bind(struct flounder_glue_lmp_binding *b, struct waitset *ws,
                          struct flounder_glue_bind_continuation cont,
                          iref_t iref, size_t buflen_words)
{
    errval_t err;

    struct flounder_glue_binding *g = &b->common;
    lmp_binding_init(b, ws);
    g->bind_continuation = cont;

    struct lmp_bind_continuation mycont = {
        .handler = lmp_bind_continuation,
        .st = g,
    };

    err = lmp_chan_bind(&b->chan, mycont, &g->event_qnode, iref, buflen_words);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_LMP_CHAN_BIND);
    }

    return SYS_ERR_OK;
}

/// Connect handler for incoming LMP connections
static errval_t lmp_connect_handler(void *st, size_t buflen_words,
                                    struct capref endpoint,
                                    struct lmp_chan **retlc)
{
    struct flounder_glue_export *e = st;
    errval_t err;

    // allocate storage for binding
    struct flounder_glue_lmp_binding *b = malloc(sizeof(struct flounder_glue_lmp_binding));
    if (b == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct flounder_glue_binding *g = &b->common;
    lmp_binding_init(b, e->ws);

    // run user's connect handler
    err = e->connect_callback(e->st, g);
    if (err_is_fail(err)) {
        // connection refused
        lmp_binding_destroy(b);
        free(b);
        return err;
    }

    // accept the connection and setup the channel
    // FIXME: do we want some user policy here to decide on the size of our end
    // of the message buffer?
    err = lmp_chan_accept(&b->chan, buflen_words, endpoint);
    if (err_is_fail(err)) {
        // TODO: run user's error handler, destroy binding
        return LIB_ERR_LMP_CHAN_ACCEPT;
    }

    // allocate a cap receive slot
    err = lmp_chan_alloc_recv_slot(&b->chan);
    if (err_is_fail(err)) {
        lmp_chan_destroy(&b->chan);
        // TODO: run user's error handler, destroy binding
        return LIB_ERR_LMP_ALLOC_RECV_SLOT;
    }

    /* register for receiving */
    struct event_closure receive_closure = {
        .handler = lmp_receive_handler,
        .arg = b,
    };
    err = lmp_chan_register_recv(&b->chan, g->waitset, receive_closure);
    if (err_is_fail(err)) {
        lmp_chan_destroy(&b->chan);
        // TODO: run user's error handler, destroy binding
        return err_push(err, LIB_ERR_CHAN_REGISTER_RECV);
    }

    *retlc = &b->chan;
    return SYS_ERR_OK;
}

#endif // CONFIG_INTERCONNECT_DRIVER_LMP

/* ------------------------ UMP implementation ------------------------ */

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP

// FIXME: this is arch-specific, should be elsewhere
#if defined(__x86_64__) || defined(__i386__)
# define BARRIER()   __asm volatile ("" : : : "memory")
#else
# error "don't know how to do a memory barrier on this architecture"
#endif

static void ump_send_handler(void *st);

/// Computes (from seq/ack numbers) whether we can currently send on the channel
static bool ump_cansend(struct flounder_glue_ump_binding *b) {
    return (ump_index_t)(b->sent_id - b->ack_id) < b->chan.max_send_msgs;
}

/// Message types that we might send
enum ump_msgtype {
    UMP_MSG_START,  ///< start of a new message (includes number of bytes/caps)
    UMP_CAP_ACK,    ///< response to a start message that included caps
    UMP_PAYLOAD     ///< generic data payload
};

/// Prepare a "control" word (header for each UMP message fragment)
static void ump_control_fill(struct flounder_glue_ump_binding *b,
                             struct ump_control *ctrl, enum ump_msgtype msgtype)
{
    ctrl->header = ((uintptr_t)msgtype << UMP_INDEX_BITS) | (uintptr_t)b->seq_id;
    b->last_ack = b->seq_id;
    b->sent_id++;
}

/// Process a "control" word
static enum ump_msgtype ump_control_process(struct flounder_glue_ump_binding *b,
                                            struct ump_control ctrl)
{
    b->ack_id = ctrl.header & UMP_INDEX_MASK;
    b->seq_id++;
    return ctrl.header >> UMP_INDEX_BITS;
}

static void ump_receive_handler(void *arg)
{
    struct flounder_glue_ump_binding *b = arg;
    struct flounder_glue_binding *g = &b->common;
    volatile struct ump_message *msg;
    struct ump_control ctrl;
    errval_t err;

    struct event_closure receive_closure = {
        .handler = ump_receive_handler,
        .arg = b,
    };

    while (1) {
        // try to retrieve a message from the channel
        err = ump_chan_recv(&b->chan, &msg);
        if (err_is_fail(err)) {
            if (err_no(err) == LIB_ERR_NO_UMP_MSG) {
                break; // nothing there
            } else {
                // real error, report to user
                g->error_handler(g->st, err_push(err, LIB_ERR_UMP_CHAN_RECV));
                return;
            }
        }

        // what kind of message is this?
        switch (ump_control_process(b, msg->header.control)) {
        case UMP_MSG_START: // new message
            // finished processing last message?
            assert(g->recvbuf == NULL);
            assert(b->recv_state == UMP_RECV_STATE_IDLE);

            // allocate space for payload
            g->recvlen = msg->data[0];
            g->recvpos = 0;
            if (g->recvlen > 0) {
                g->recvbuf = malloc(g->recvlen);
                assert(g->recvbuf != NULL);
            } else {
                g->recvbuf = NULL;
            }

            // allocate space for caps
            g->nrecvcaps = msg->data[1];
            g->recvcappos = 0;
            if (g->nrecvcaps > 0) {
                g->recvcaps = malloc(sizeof(struct capref) * g->nrecvcaps);
                assert(g->recvcaps != NULL);

                b->recv_state = UMP_RECV_STATE_SEND_CAP_ACK;
            } else {
                g->recvcaps = NULL;
                if (g->recvlen == 0) {
                    // this was an empty message (just the ack)
                    b->recv_state = UMP_RECV_STATE_IDLE;
                } else {
                    b->recv_state = UMP_RECV_STATE_DATA_CAPS;
                }
            }
            break;

        case UMP_CAP_ACK: // ok to send caps now
            assert(b->send_state == UMP_SEND_STATE_DATA_ONLY);
            b->send_state = UMP_SEND_STATE_DATA_CAPS;
            break;

        case UMP_PAYLOAD: // data payload
            assert(b->recv_state != UMP_RECV_STATE_IDLE);
            // copy message payload to buffer
            for (int i = 0; i < UMP_PAYLOAD_WORDS && g->recvpos < g->recvlen;
                    i++) {
                putword(msg->data[i], g->recvbuf, &g->recvpos, g->recvlen);
            }
            break;

        default:
            USER_PANIC("invalid UMP message");
            return;
        }

        // send a forced ACK if the channel is now full
        // FIXME: should send it when "nearly" full
        if ((ump_index_t)(b->seq_id - b->last_ack) >=
            (ump_index_t)(b->chan.max_recv_msgs - 1)) {
            // run our own send process if we have stuff waiting to go
            if (b->send_state != UMP_SEND_STATE_IDLE
                || b->recv_state == UMP_RECV_STATE_SEND_CAP_ACK) {
                ump_send_handler(b);
            } else { // send an empty message
                assert(ump_cansend(b));
                msg = ump_chan_get_next(&b->chan, &ctrl);
                ump_control_fill(b, &ctrl, UMP_MSG_START);
                msg->data[0] = 0;
                msg->data[1] = 0;
                BARRIER();
                msg->header.control = ctrl;
            }
        }

        // complete message, deliver to user
        if ((g->recvbuf != NULL || g->recvcaps != NULL)
            && g->recvpos == g->recvlen && g->recvcappos == g->nrecvcaps) {
            msgbuf_init_static(&g->recvmb, g->recvbuf, g->recvlen,
                               g->recvcaps, g->nrecvcaps);
            g->recvmb.dynamic = true; // XXX
            g->receive_handler(g->st, &g->recvmb);
            g->recvbuf = NULL;
            g->recvcaps = NULL;
            g->recvpos = g->recvlen = 0;
            g->recvcappos = g->nrecvcaps = 0;
            b->recv_state = UMP_RECV_STATE_IDLE;
        } else if (g->recvpos == g->recvlen && g->recvcaps != NULL) {
            // almost complete, but we're just waiting for caps
            // don't try to handle more messages until we're done with this one
            break;
        }
    }

    // now that we've received some stuff, try to send more if necessary
    if (b->recv_state == UMP_RECV_STATE_SEND_CAP_ACK
        || b->send_state != UMP_SEND_STATE_IDLE) {
        ump_send_handler(b);
    }

    // no more messages, re-register for another receive notification
    // (but only if we're ready to handle it)
    if (!(b->recv_state == UMP_RECV_STATE_DATA_CAPS && g->recvpos == g->recvlen)) {
        err = ump_chan_register_recv(&b->chan, g->waitset, receive_closure);
        if (err_is_fail(err)) {
            g->error_handler(g->st, err_push(err, LIB_ERR_CHAN_REGISTER_RECV));
        }
    }
}

static errval_t do_ump_send(struct flounder_glue_ump_binding *b,
                            const uint8_t *buf, size_t pos, size_t len,
                            const struct capref *caps,
                            size_t cappos, size_t ncaps)
{
    struct flounder_glue_binding *g = &b->common;
    struct ump_control ctrl;
    volatile struct ump_message *msg;
    errval_t err;

    // send caps via monitor if possible
    if (b->send_state == UMP_SEND_STATE_DATA_CAPS && cappos < ncaps) {
        struct monitor_binding *mcb = get_monitor_binding();

        // will we have more caps to send after this one is gone?
        struct event_closure send_cont;
        if (cappos == ncaps - 1) {
            send_cont = NOP_CONT;
        } else {
            send_cont = MKCONT(ump_send_handler, g);
        };

        err = mcb->tx_vtbl.cap_send_request(mcb, send_cont, b->chan.monitor_id,
                                            caps[cappos], ncaps,
                                            false /* XXX - old flounder only supports copy transfer type*/ );
        if (err_is_ok(err)) {
            cappos++;
        } else {
            return err_push(err, LIB_ERR_MONITOR_CAP_SEND);
        }
    }

    // do we need to send a cap ack for our current receive process?
    if (b->recv_state == UMP_RECV_STATE_SEND_CAP_ACK && ump_cansend(b)) {
        msg = ump_chan_get_next(&b->chan, &ctrl);
        ump_control_fill(b, &ctrl, UMP_CAP_ACK);
        BARRIER();
        msg->header.control = ctrl;
        b->recv_state = UMP_RECV_STATE_DATA_CAPS;
    }

    // send a header first
    if (b->send_state == UMP_SEND_STATE_HEADER && ump_cansend(b)) {
        msg = ump_chan_get_next(&b->chan, &ctrl);
        ump_control_fill(b, &ctrl, UMP_MSG_START);
        msg->data[0] = len;
        msg->data[1] = ncaps;
        BARRIER();
        msg->header.control = ctrl;
        b->send_state = UMP_SEND_STATE_DATA_ONLY;
    }

    // send payload down UMP channel
    while (b->send_state > UMP_SEND_STATE_HEADER && ump_cansend(b) && pos < len) {
        msg = ump_chan_get_next(&b->chan, &ctrl);
        for (int i = 0; i < UMP_PAYLOAD_WORDS && pos < len; i++) {
            msg->data[i] = getword(buf, &pos, len);
        }
        ump_control_fill(b, &ctrl, UMP_PAYLOAD);
        BARRIER();
        msg->header.control = ctrl;
    }

    if (b->send_state == UMP_SEND_STATE_HEADER || pos < len || cappos < ncaps) {
        // ran out of space, queue rest of message
        g->sendbuf = buf;
        g->sendpos = pos;
        g->sendlen = len;
        g->sendcaps = caps;
        g->sendcappos = cappos;
        g->nsendcaps = ncaps;
    } else if (b->send_state != UMP_SEND_STATE_IDLE) {
        // finished!
        g->sendbuf = NULL;
        g->sendcaps = NULL;
        b->send_state = UMP_SEND_STATE_IDLE;

        // trigger send continuation if registered
        if (waitset_chan_is_registered(&g->sendcont_waitset)) {
            err = waitset_chan_trigger(&g->sendcont_waitset);
            assert(err_is_ok(err)); // shouldn't fail
        }

        // trigger send waitset if registered
        if (waitset_chan_is_registered(&g->send_waitset)) {
            err = waitset_chan_trigger(&g->send_waitset);
            assert(err_is_ok(err)); // shouldn't fail
        }
    }

    return SYS_ERR_OK;
}

static void ump_send_handler(void *st)
{
    struct flounder_glue_ump_binding *b = st;
    struct flounder_glue_binding *g = &b->common;

    assert(g);
    assert(b->send_state != UMP_SEND_STATE_IDLE
           || b->recv_state == UMP_RECV_STATE_SEND_CAP_ACK);

    errval_t err = do_ump_send(b, g->sendbuf, g->sendpos, g->sendlen,
                               g->sendcaps, g->sendcappos, g->nsendcaps);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "while transmitting more fragments of queued message");
        // fatal, discard rest of queued message
        g->sendbuf = NULL;
        g->sendcaps = NULL;
        b->send_state = UMP_SEND_STATE_IDLE;
        // signal error to user
        g->error_handler(g->st, err);
    }
}

static void ump_cap_receive_handler(void *st, struct capref cap, uint32_t capid)
{
    struct flounder_glue_ump_binding *b = st;
    struct flounder_glue_binding *g = &b->common;
    errval_t err;

    // should be waiting for a cap
    assert(b->recv_state == UMP_RECV_STATE_DATA_CAPS);
    assert(g->recvcappos < g->nrecvcaps);
    assert(g->recvcaps != NULL);

    // store the cap
    g->recvcaps[g->recvcappos++] = cap;

    // have we finished? if so deliver to user
    if (g->recvpos == g->recvlen && g->recvcappos == g->nrecvcaps) {
        msgbuf_init_static(&g->recvmb, g->recvbuf, g->recvlen,
                           g->recvcaps, g->nrecvcaps);
        g->recvmb.dynamic = true; // XXX
        g->receive_handler(g->st, &g->recvmb);

        g->recvbuf = NULL;
        g->recvcaps = NULL;
        g->recvpos = g->recvlen = 0;
        g->recvcappos = g->nrecvcaps = 0;
        b->recv_state = UMP_RECV_STATE_IDLE;

        // re-register for another UMP receive notification
        struct event_closure receive_closure = {
            .handler = ump_receive_handler,
            .arg = b,
        };

        err = ump_chan_register_recv(&b->chan, g->waitset, receive_closure);
        if (err_is_fail(err)) {
            g->error_handler(g->st, err_push(err, LIB_ERR_CHAN_REGISTER_RECV));
        }
    }
}

static void ump_cap_send_reply_handler(void *st, uint32_t capid, errval_t success)
{
    struct flounder_glue_ump_binding *b = st;
    struct flounder_glue_binding *g = &b->common;

    if (err_is_fail(success)) {
        assert(g != NULL && g->error_handler != NULL);
        DEBUG_ERR(success, "monitor refused to accept cap for UMP send");
        g->error_handler(g->st, err_push(success, LIB_ERR_MONITOR_CAP_SEND));
    }
}

static errval_t ump_send(struct flounder_glue_binding *g,
                         struct event_closure continuation,
                         const void *msg, size_t len,
                         const struct capref *caps, 
                         size_t ncaps)
{
    struct flounder_glue_ump_binding *b = (struct flounder_glue_ump_binding *)g;
    errval_t err;

    // messages already queued?
    if (!can_send(g)) {
        return FLOUNDER_ERR_TX_BUSY;
    }

    // register send continuation
    if (continuation.handler != NULL) {
        err = waitset_chan_register(g->waitset, &g->sendcont_waitset,
                                    continuation);
        assert(err_is_ok(err)); // shouldn't fail if we have no messages queued
    }

    assert(b->send_state == UMP_SEND_STATE_IDLE);
    b->send_state = UMP_SEND_STATE_HEADER;
    return do_ump_send(b, msg, 0, len, caps, 0, ncaps);
}

static void ump_bind_continuation(void *st, errval_t err, struct ump_chan *lc,
                                  struct capref notify_cap)
{
    struct flounder_glue_ump_binding *b = st;
    struct flounder_glue_binding *g = &b->common;

    assert(capref_is_null(notify_cap)); // not supported

    // just pass it on to the user
    assert(g->bind_continuation.handler != NULL);
    g->bind_continuation.handler(g->bind_continuation.st, err, g);
    if (err_is_fail(err)) {
        // FIXME: release state!
        ump_chan_destroy(&b->chan);
        flounder_glue_binding_destroy(g);
    }
}

static void ump_binding_init(struct flounder_glue_ump_binding *b, struct waitset *ws)
{
    struct flounder_glue_binding *g = &b->common;
    flounder_glue_binding_init(g, ws);
    g->send = ump_send;
    b->send_state = UMP_SEND_STATE_IDLE;
    b->recv_state = UMP_RECV_STATE_IDLE;
    b->sent_id = b->seq_id = b->ack_id = b->last_ack = 0;
}

static void ump_binding_destroy(struct flounder_glue_ump_binding *b)
{
    flounder_glue_binding_destroy(&b->common);
}

/**
 * \brief Initialise and attempt to bind a new generic UMP binding
 *
 * \param b       Storage for binding state
 * \param ws      Waitset on which to register for receive handling
 * \param cont    Continuation closure for bind completion
 * \param iref    IREF to which to bind
 * \param inchanlen Size of incoming channel, in bytes
 * \param outchanlen Size of outgoing channel, in bytes
 */
errval_t flounder_glue_ump_bind(struct flounder_glue_ump_binding *b, struct waitset *ws,
                          struct flounder_glue_bind_continuation cont,
                          iref_t iref, size_t inchanlen, size_t outchanlen)
{
    errval_t err;

    struct flounder_glue_binding *g = &b->common;
    ump_binding_init(b, ws);
    g->bind_continuation = cont;

    struct ump_bind_continuation mycont = {
        .handler = ump_bind_continuation,
        .st = g,
    };

    err = ump_chan_bind(&b->chan, mycont, &g->event_qnode, iref,
                        get_monitor_binding(), inchanlen, outchanlen, NULL_CAP);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_UMP_CHAN_BIND);
    }

    /* setup handlers */
    b->chan.cap_handlers.st = b;
    b->chan.cap_handlers.cap_receive_handler = ump_cap_receive_handler;
    b->chan.cap_handlers.cap_send_reply_handler = ump_cap_send_reply_handler;

    /* register for receiving */
    struct event_closure receive_closure = {
        .handler = ump_receive_handler,
        .arg = b,
    };
    err = ump_chan_register_recv(&b->chan, ws, receive_closure);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CHAN_REGISTER_RECV);
    }

    return SYS_ERR_OK;
}

/// Connect handler for incoming UMP connections
static errval_t ump_connect_handler(void *st, struct monitor_binding *mb,
                                    uintptr_t mon_id, struct capref frame,
                                    size_t inchanlen,
                                    size_t outchanlen,
                                    struct capref notify_cap)
{
    struct flounder_glue_export *e = st;
    errval_t err;

    assert(capref_is_null(notify_cap)); // unhandled here!

    // allocate storage for binding
    struct flounder_glue_ump_binding *b = malloc(sizeof(struct flounder_glue_ump_binding));
    if (b == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct flounder_glue_binding *g = &b->common;
    ump_binding_init(b, e->ws);

    // run user's connect handler
    err = e->connect_callback(e->st, g);
    if (err_is_fail(err)) {
        // connection refused
        ump_binding_destroy(b);
        free(b);
        return err;
    }

    // accept the connection and setup the UMP channel
    err = ump_chan_accept(&b->chan, mon_id, frame, inchanlen, outchanlen);
    if (err_is_fail(err)) {
        // TODO: run user's error handler, destroy binding
        return LIB_ERR_UMP_CHAN_ACCEPT;
    }

    /* setup handlers */
    b->chan.cap_handlers.st = b;
    b->chan.cap_handlers.cap_receive_handler = ump_cap_receive_handler;
    b->chan.cap_handlers.cap_send_reply_handler = ump_cap_send_reply_handler;

    /* register for receiving */
    struct event_closure receive_closure = {
        .handler = ump_receive_handler,
        .arg = b,
    };
    err = ump_chan_register_recv(&b->chan, g->waitset, receive_closure);
    if (err_is_fail(err)) {
        ump_chan_destroy(&b->chan);
        // TODO: run user's error handler, destroy binding
        return err_push(err, LIB_ERR_CHAN_REGISTER_RECV);
    }

    // send back bind reply
    ump_chan_send_bind_reply(mb, &b->chan, SYS_ERR_OK, mon_id, NULL_CAP);

    return SYS_ERR_OK;
}

#endif // CONFIG_INTERCONNECT_DRIVER_UMP

/* ------------------------ Generic service export ------------------------ */

/**
 * \brief Export a new service with a generic binding type
 *
 * \param e Storage for export state
 * \param export_callback Callback closure for when the service is exported
 * \param connect_callback Callback closure for connection attempts
 * \param ws Default waitset to be used for newly-created bindings
 * \param flags Export flags
 */
errval_t flounder_glue_export(struct flounder_glue_export *e,
                        idc_export_callback_fn *export_callback,
                        flounder_glue_connect_callback_fn *connect_callback,
                        void *st, struct waitset *ws, idc_export_flags_t flags)
{
    e->connect_callback = connect_callback;
    e->st = st;
    e->export.export_callback = export_callback;
    e->export.flags = flags;
    e->export.export_cb_st = st;
    e->export.connect_cb_st = e;
    e->ws = ws;
#ifdef CONFIG_INTERCONNECT_DRIVER_LMP
    e->export.lmp_connect_callback = lmp_connect_handler;
#endif
#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
    e->export.ump_connect_callback = ump_connect_handler;
#endif
    return idc_export_service(&e->export);
}

/* -------------- Generic bind function ------------------ */

struct bind_attempt {
    iref_t iref;
    struct waitset *ws;
    struct flounder_glue_bind_continuation cont;
    void *b;
};

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
static void generic_bind_ump_cont(void *st, errval_t err,
                                  struct flounder_glue_binding *b)
{
    struct bind_attempt *ba = st;

    // clean up on failure
    if (err_is_fail(err)) {
        free(ba->b);
    }

    ba->cont.handler(ba->cont.st, err, b);
    free(ba);
}
#endif

static void generic_bind_lmp_cont(void *st, errval_t err,
                                  struct flounder_glue_binding *b)
{
    struct bind_attempt *ba = st;

    // clean up on failure
    if (err_is_fail(err)) {
        free(ba->b);
    }

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
    if (err_no(err) == MON_ERR_IDC_BIND_NOT_SAME_CORE) {
        // try UMP
        ba->b = malloc(sizeof(struct flounder_glue_ump_binding));
        assert(ba->b != NULL);

        struct flounder_glue_bind_continuation cont = {
            .handler = generic_bind_ump_cont,
            .st = ba,
        };

        err = flounder_glue_ump_bind(ba->b, ba->ws, cont, ba->iref,
                                     DEFAULT_UMP_BUFLEN, DEFAULT_UMP_BUFLEN);
        if (err_is_ok(err)) {
            return;
        } else {
            free(ba->b);
        }
    }
#endif

    // success or real failure: report to user and stop
    ba->cont.handler(ba->cont.st, err, b);
    free(ba);
}

errval_t flounder_glue_bind(iref_t iref, struct waitset *ws,
                            struct flounder_glue_bind_continuation cont)
{
    errval_t err;

    struct bind_attempt *ba = malloc(sizeof(struct bind_attempt));
    assert(ba != NULL);

    ba->iref = iref;
    ba->ws = ws;
    ba->cont = cont;

    // try IDC first
    ba->b = malloc(sizeof(struct flounder_glue_lmp_binding));
    assert(ba->b != NULL);

    struct flounder_glue_bind_continuation mycont = {
        .handler = generic_bind_lmp_cont,
        .st = ba,
    };
    err = flounder_glue_lmp_bind(ba->b, ws, mycont, iref, DEFAULT_LMP_BUF_WORDS);
    if (err_is_fail(err)) {
        free(ba->b);
        free(ba);
    }

    return err;
}

/* -------------- Misc crap for old flounder glue stubs ------------------ */

#include <flounder/glue_internal.h>

void flounder_glue_error_handler(void *st, errval_t err)
{
    USER_PANIC_ERR(err, "async error in flounder glue stubs");
}

void messages_wait_and_handle_next(void)
{
    errval_t err = event_dispatch(get_default_waitset());
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error in event_dispatch for messages_wait_and_handle_next hack");
    }
}

void messages_handler_loop(void)
{
    while (1) {
        messages_wait_and_handle_next();
    }
}
