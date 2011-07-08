/**
 * \file
 * \brief Prototypes for use by flounder-generated stubs
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __FLOUNDER_SUPPORT_BMP_H
#define __FLOUNDER_SUPPORT_BMP_H

#include <barrelfish/bmp_chan.h>
#include <flounder/flounder_support_caps.h>

/// Type used for indices of BMP message slots
typedef uint16_t bmp_index_t;
#define BMP_INDEX_BITS  16
#define BMP_MSGNUM_BITS 16

/// Message header word
union bmp_msg_header {
    struct {
        uintptr_t acknum:BMP_INDEX_BITS;
        uintptr_t msgnum:BMP_MSGNUM_BITS;
    } x;
    uintptr_t raw;
};
STATIC_ASSERT_SIZEOF(union bmp_msg_header, sizeof(uintptr_t));

/// Special message types
enum flounder_bmp_msgnum {
    FL_BMP_ACK = 0,
    FL_BMP_CAP_ACK = (1 << BMP_INDEX_BITS) - 1,
};

struct flounder_bmp_state {
    struct bmp_chan chan;

    bmp_index_t sent_id;   ///< Sequence number of next message to be sent
    bmp_index_t seq_id;    ///< Last sequence number received from remote
    bmp_index_t ack_id;    ///< Last sequence number acknowledged by remote
    bmp_index_t last_ack;  ///< Last acknowledgement we sent to remote

    struct flounder_cap_state capst; ///< State for indirect cap tx/rx machinery
};

void flounder_stub_bmp_state_init(struct flounder_bmp_state *s, void *binding);

errval_t flounder_stub_bmp_send_string(struct flounder_bmp_state *s,
                                       int msgnum, const char *str,
                                       size_t *pos, size_t *len);

errval_t flounder_stub_bmp_recv_string(struct bmp_recv_msg *msg,
                                       char **str, size_t *pos, size_t *len);

errval_t flounder_stub_bmp_send_buf(struct flounder_bmp_state *s,
                                    int msgnum, const void *buf,
                                    size_t len, size_t *pos);

errval_t flounder_stub_bmp_recv_buf(struct bmp_recv_msg *msg,
                                    void **buf, size_t *len, size_t *pos);

/// Computes (from seq/ack numbers) whether we can currently send on the channel
static inline bool flounder_stub_bmp_can_send(struct flounder_bmp_state *s,
                                              size_t totlen)
{
    assert(totlen <= s->chan.outeplen);
    return (bmp_index_t)(s->sent_id - s->ack_id) <= (s->chan.outeplen - totlen);
}

/// Prepare a header word
static inline uintptr_t flounder_stub_bmp_mkheader(struct flounder_bmp_state *s,
                                                   int msgtype)
{
    assert(msgtype < (1 << BMP_MSGNUM_BITS)); // check for overflow
    union bmp_msg_header h = {
        .x = {
            .acknum = s->seq_id,
            .msgnum = msgtype }};
    return h.raw;
}

/// Try to send a message
static inline errval_t flounder_stub_bmp_send(struct flounder_bmp_state *s,
                                              uintptr_t *msg, size_t totlen)
{
    assert(flounder_stub_bmp_can_send(s, totlen));
    errval_t err = bmp_chan_send(&s->chan, msg, totlen);
    if (err_is_ok(err)) {
        s->last_ack = s->seq_id;
        s->sent_id += totlen + LMP_RECV_HEADER_LENGTH; /* header on receive LMP side */
    }
    return err;
}

/// Process a header word
static inline int flounder_stub_bmp_process_header(struct flounder_bmp_state *s,
                                                   uintptr_t header, size_t totlen)
{
    union bmp_msg_header h = { .raw = header };
    s->ack_id = h.x.acknum;
    s->seq_id += totlen + LMP_RECV_HEADER_LENGTH;
    return h.x.msgnum;
}

/// Should we send an ACK?
static inline bool flounder_stub_bmp_needs_ack(struct flounder_bmp_state *s)
{
    // send a forced ACK if the channel would now not be able
    // to accept another MTU-sized message
    assert(s->chan.inep->buflen >= BMP_MSG_LENGTH);
    return (bmp_index_t)(s->seq_id - s->last_ack)
                >= s->chan.inep->buflen - BMP_MSG_LENGTH;
}

/// Send an explicit ACK
static inline errval_t flounder_stub_bmp_send_ack(struct flounder_bmp_state *s)
{
    assert(flounder_stub_bmp_can_send(s, 1));
    uintptr_t msg = flounder_stub_bmp_mkheader(s, FL_BMP_ACK);
    return flounder_stub_bmp_send(s, &msg, 1);
}

/// Send a cap ACK (message that we are ready to receive caps)
static inline errval_t flounder_stub_bmp_send_cap_ack(struct flounder_bmp_state *s)
{
    assert(flounder_stub_bmp_can_send(s, 1));
    uintptr_t msg = flounder_stub_bmp_mkheader(s, FL_BMP_CAP_ACK);
    return flounder_stub_bmp_send(s, &msg, 1);
}

#endif // __FLOUNDER_SUPPORT_BMP_H
