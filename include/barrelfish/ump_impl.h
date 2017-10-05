/**
 * \file
 * \brief User-space messaging (UMP, formerly URPC) data transport implementation
 */

/*
 * Copyright (c) 2007-2010, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef UMP_IMPL_H
#define UMP_IMPL_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

/**
 * UMP message size is fixed to cache-line size (64 bytes on x86_64).
 * FIXME: this should be different transports, not a compile-time constant
 */
// XXX Should be from SKB or whatever.  On Arm this is variable and you need
// to read the coproc to see what size it is.
#if defined(__x86_64__) || defined(__i386__)
#  define CACHELINE_BYTES 64
#elif defined(__arm__)
#  define CACHELINE_BYTES 32
#elif defined(__aarch64__)
// XXX: is this true?
#  define CACHELINE_BYTES 64
#else
# error set CACHELINE_BYTES appropriately
#endif

// Size of a UMP message in Bytes
// This needs to be such that ump_payload defined in params in flounder/UMP.hs
// and the size of the UMP headers fits into it. It also needs to be a multiple
// of a cache-line.
#define UMP_MSG_BYTES      64
#define UMP_MSG_WORDS      (UMP_MSG_BYTES / sizeof(uintptr_t))
#define UMP_PAYLOAD_BYTES  (UMP_MSG_BYTES - sizeof(uint64_t))
#define UMP_PAYLOAD_WORDS  (UMP_PAYLOAD_BYTES / sizeof(uintptr_t))

/// Default size of a unidirectional UMP message buffer, in bytes
#define DEFAULT_UMP_BUFLEN  (BASE_PAGE_SIZE / 2 / UMP_MSG_BYTES * UMP_MSG_BYTES)

// control word is 32-bit, because it must be possible to atomically write it
typedef uint32_t ump_control_t;
#define UMP_USED_BITS  1
#define UMP_HEADER_BITS 31

struct ump_control {
    ump_control_t used:UMP_USED_BITS;
    ump_control_t header:UMP_HEADER_BITS;
    ump_control_t token:32;
};

struct ump_message {
    uintptr_t data[UMP_PAYLOAD_WORDS] __attribute__((aligned (CACHELINE_BYTES)));
    union {
        struct ump_control control;
        uint64_t raw;
    } header;
};
STATIC_ASSERT((sizeof(struct ump_message)%CACHELINE_BYTES)==0,
               "Size of UMP message is not a multiple of cache-line size");

/// Type used for indices of UMP message slots
typedef uint16_t ump_index_t;
#define UMP_INDEX_BITS         (sizeof(ump_index_t) * NBBY)
#define UMP_INDEX_MASK         ((((uintptr_t)1) << UMP_INDEX_BITS) - 1)

/**
 * UMP direction
 */
enum ump_direction {
    UMP_OUTGOING,
    UMP_INCOMING
};

/**
 * \brief State of a (one-way) UMP channel
 */
struct ump_chan_state {
    volatile struct ump_message *buf;  ///< Ring buffer
    ump_index_t        pos;            ///< Current position
    ump_index_t        bufmsgs;        ///< Buffer size in messages
    enum ump_direction dir;            ///< Channel direction
};

/// Cache-aligned size of a #ump_chan_state struct
#define UMP_CHAN_STATE_SIZE ROUND_UP(sizeof(struct ump_chan_state), CACHELINE_BYTES)


/**
 * \brief Initialize UMP channel state
 *
 * The channel-state structure and buffer must already be allocated.
 *
 * \param       c       Pointer to channel-state structure to initialize.
 * \param       buf     Pointer to ring buffer for the channel. Must be aligned to a cacheline.
 * \param       size    Size (in bytes) of buffer. Must be multiple of #UMP_MSG_BYTES
 * \param       dir     Channel direction.
 */
static inline errval_t ump_chan_state_init(struct ump_chan_state *c,
                                           volatile void *buf,
                                           size_t size, enum ump_direction dir)
{
    // check alignment and size of buffer.
    if (size == 0 || (size % UMP_MSG_BYTES) != 0) {
        return LIB_ERR_UMP_BUFSIZE_INVALID;
    }

    if (buf == NULL || (((uintptr_t)buf) % CACHELINE_BYTES) != 0) {
        return LIB_ERR_UMP_BUFADDR_INVALID;
    }

    c->pos = 0;
    c->buf = (volatile struct ump_message *) buf;
    c->dir = dir;
    c->bufmsgs = size / UMP_MSG_BYTES;

    if(dir == UMP_INCOMING) {
        ump_index_t i;
        for(i = 0; i < c->bufmsgs; i++) {
            c->buf[i].header.raw = 0;
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Return pointer to a message if outstanding on 'c'.
 *
 * \param c     Pointer to UMP channel-state structure.
 *
 * \return Pointer to message if outstanding, or NULL.
 */
static inline volatile struct ump_message *ump_impl_poll(struct ump_chan_state *c)
{
    assert(c->dir == UMP_INCOMING);
    ump_control_t ctrl_used =  c->buf[c->pos].header.control.used;
    if (ctrl_used)
        return &c->buf[c->pos];
    return NULL;
}

/**
 * \brief Return pointer to a message if outstanding on 'c' and
 * advance pointer.
 *
 * \param c     Pointer to UMP channel-state structure.
 *
 * \return Pointer to message if outstanding, or NULL.
 */
static inline volatile struct ump_message *ump_impl_recv(struct ump_chan_state *c)
{
    volatile struct ump_message *msg = ump_impl_poll(c);

    if (msg != NULL) {
        if (++c->pos == c->bufmsgs)
            c->pos = 0;
        return msg;
    }
    return NULL;
}

/**
 * \brief Determine next position for an outgoing message on a channel, and
 *   advance send pointer.
 *
 * \param c     Pointer to UMP channel-state structure.
 * \param ctrl  Pointer to storage for control word for next message, to be filled in
 *
 * \return Pointer to message if outstanding, or NULL.
 */
static inline volatile struct ump_message *ump_impl_get_next(
                            struct ump_chan_state *c, struct ump_control *ctrl)
{
    assert(c->dir == UMP_OUTGOING);

    // construct header
    ctrl->used = 1;
    ctrl->token = 0;

#ifdef __x86_64__
    if(debug_notify_syscall) {
        printf("ump_impl_get_next while forbidden from %p, %p, %p, %p, %p, %p, %p\n",
               __builtin_return_address(0),
               __builtin_return_address(1),
               __builtin_return_address(2),
               __builtin_return_address(3),
               __builtin_return_address(4),
               __builtin_return_address(5),
               __builtin_return_address(6));
    }
#endif

    volatile struct ump_message *msg = &c->buf[c->pos];
    if (msg->header.control.used)
        return NULL;
    // update pos
    if (++c->pos == c->bufmsgs)
        c->pos = 0;
    return msg;
}

static inline volatile bool ump_impl_can_send(struct ump_chan_state *c)
{
    assert(c->dir == UMP_OUTGOING);
    volatile struct ump_message *msg = &c->buf[c->pos];
    return !msg->header.control.used;
}

static inline void ump_impl_free_message(volatile struct ump_message *msg)
{
    msg->header.control.used = 0;
}

__END_DECLS

#endif // UMP_IMPL_H
