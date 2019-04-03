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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __FLOUNDER_SUPPORT_UMP_H
#define __FLOUNDER_SUPPORT_UMP_H

#include <sys/cdefs.h>

#include <barrelfish/ump_chan.h>
#include <flounder/flounder_support_caps.h>
#include <trace/trace.h>

__BEGIN_DECLS

/// Number of bits available for the message type in the header
#define FL_UMP_MSGTYPE_BITS (UMP_HEADER_BITS - UMP_INDEX_BITS)

/// Special message types
enum flounder_ump_msgtype {
    FL_UMP_BIND = 0,
    FL_UMP_BIND_REPLY = 1,
    FL_UMP_CAP_ACK = (1 << FL_UMP_MSGTYPE_BITS) - 1,
};

struct flounder_ump_state {
    struct ump_chan chan;

    struct flounder_cap_state capst; ///< State for indirect cap tx/rx machinery
    uint32_t token;        ///< Outgoing message's token
};

void flounder_stub_ump_state_init(struct flounder_ump_state *s, void *binding);

errval_t flounder_stub_ump_send_string(struct flounder_ump_state *s,
                                       int msgnum, const char *str,
                                       size_t *pos, size_t *len);

errval_t flounder_stub_ump_recv_string(volatile struct ump_message *msg,
                                       char *str, size_t *pos, size_t *len,
                                       size_t maxsize);

errval_t flounder_stub_ump_send_buf(struct flounder_ump_state *s,
                                       int msgnum, const void *buf,
                                       size_t len, size_t *pos);

errval_t flounder_stub_ump_recv_buf(volatile struct ump_message *msg,
                                    void *buf, size_t *len, size_t *pos,
                                    size_t maxsize);


#define ENABLE_MESSAGE_PASSING_TRACE 1
/// Prepare a "control" word (header for each UMP message fragment)
static inline void flounder_stub_ump_control_fill(struct flounder_ump_state *s,
                                                  struct ump_control *ctrl,
                                                  int msgtype)
{
#if ENABLE_MESSAGE_PASSING_TRACE
    trace_event_raw((((uint64_t)0xEA)<<56) |
                    ((uint64_t)s->chan.sendid << 12));
#endif // ENABLE_MESSAGE_PASSING_TRACE
    assert(s->chan.sendid != 0);
    assert(msgtype < (1 << FL_UMP_MSGTYPE_BITS)); // check for overflow
    ctrl->header = ((uintptr_t)msgtype << UMP_INDEX_BITS);
    ctrl->token = s->token;
}

/// Process a "control" word
static inline int flounder_stub_ump_control_process(struct flounder_ump_state *s,
                                                    struct ump_control ctrl)
{
#if ENABLE_MESSAGE_PASSING_TRACE
    trace_event_raw( (((uint64_t)0xEB)<<56) |
                     ((uint64_t)s->chan.recvid << 12));
#endif // ENABLE_MESSAGE_PASSING_TRACE
    return ctrl.header >> UMP_INDEX_BITS;
}

/// Emit memory barrier needed between writing UMP payload and header
static inline void flounder_stub_ump_barrier(void)
{
#if defined(__i386__) || defined(__x86_64__)
    /* the x86 memory model ensures ordering of stores, so all we need to do
     * is prevent the compiler from reordering the instructions */
    __asm volatile ("" : : : "memory");
#else
    /* use conservative GCC intrinsic */
    __sync_synchronize();
#endif
}

/// Send a cap ACK (message that we are ready to receive caps)
static inline void flounder_stub_ump_send_cap_ack(struct flounder_ump_state *s)
{
    struct ump_control ctrl;
    volatile struct ump_message *msg = ump_chan_get_next(&s->chan, &ctrl);
    assert(msg);
    flounder_stub_ump_control_fill(s, &ctrl, FL_UMP_CAP_ACK);
    msg->header.control = ctrl;
}

__END_DECLS

#endif // __FLOUNDER_SUPPORT_UMP_H
