/**
 * \file
 * \brief Waitset and low-level event handling mechanism
 */

/*
 * Copyright (c) 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef BARRELFISH_WAITSET_H
#define BARRELFISH_WAITSET_H

#include <barrelfish/types.h>
#include <errors/errno.h>
#include <sys/cdefs.h>
#include <barrelfish/dispatch.h>

__BEGIN_DECLS

#include <errors/errno.h>

#include <stdbool.h>
#include <barrelfish/types.h>

struct waitset;
struct thread;

struct event_closure {
    void (*handler)(void *arg);
    void *arg;
};

#define MKCLOSURE(h,a)  (struct event_closure){ /*handler*/ (h), /*arg*/ (a) }
#define NOP_CLOSURE     MKCLOSURE(NULL, NULL)


/**
 * \brief Channel type
 *
 * This is used for debugging and to determine the function to call when polling.
 */
enum ws_chantype {
    CHANTYPE_LMP_IN,
    CHANTYPE_LMP_OUT,
    CHANTYPE_UMP_IN,
    CHANTYPE_DEFERRED, ///< Timer events
    CHANTYPE_EVENT_QUEUE,
    CHANTYPE_FLOUNDER,
    CHANTYPE_AHCI,
    CHANTYPE_LWIP_SOCKET,
    CHANTYPE_BULK_E10K,
    CHANTYPE_OTHER
};

/// Current state of a channel on a specific waitset
enum ws_chanstate {
    CHAN_UNREGISTERED,  ///< Initialised, but not yet registered on a waitset
    CHAN_IDLE,          ///< Has a registered event handler, but the event has not fired
    CHAN_POLLED,        ///< Idle and polled. Channel implementation must be called to check for pending events
    CHAN_PENDING,       ///< Has a pending event waiting to be delivered
    CHAN_WAITING        ///< There's no registered event handler (for now)
};

/**
 * \brief Per-channel state belonging to waitset
 *
 * This data is logically private to the waitset, but is allocated and stored
 * inside the channels themselves.
 */
struct waitset_chanstate {
    struct waitset_chanstate *next, *prev;  ///< Next/prev channel in queue
    struct waitset *waitset;                ///< Waitset in which this channel is registered
    struct event_closure closure;           ///< Event closure to run when channel is ready
    enum ws_chantype chantype;              ///< Channel type
    enum ws_chanstate state;                ///< Channel event state

    uint32_t token;                         ///< Token of an event
    bool persistent;                        ///< Channel should be always registered
    struct waitset_chanstate *polled_next, *polled_prev;    ///< Dispatcher's polled queue
    struct thread *wait_for;                ///< Thread waiting for this event
    struct waitset_chanstate *trigger;      ///< Chanstate that triggers this chanstate 
};

/**
 * \brief Wait set
 *
 * This data is private to the waitset (a waitset is an opaque type),
 * but defined in the header for allocation purposes.
 */
struct waitset {
    struct waitset_chanstate *pending, ///< Channels with pending events
                             *polled,  ///< Channels that need to be polled
                             *idle,    ///< All other channels on this waitset
                             *waiting; ///< Channels waiting for an event handler registration

    /// Queue of threads blocked on this waitset (when no events are pending)
    struct thread *waiting_threads;
};

void poll_channels_disabled(dispatcher_handle_t handle);

void waitset_init(struct waitset *ws);
errval_t waitset_destroy(struct waitset *ws);

errval_t get_next_event(struct waitset *ws, struct event_closure *retclosure);
errval_t get_next_event_disabled(struct waitset *ws, struct waitset_chanstate **retchan,
    struct event_closure *retclosure, struct waitset_chanstate *waitfor,
    struct waitset_chanstate *waitfor2, dispatcher_handle_t handle, bool debug);
errval_t check_for_event(struct waitset *ws);
errval_t event_dispatch(struct waitset *ws);
errval_t wait_for_channel(struct waitset *ws, struct waitset_chanstate *channel, errval_t *error_var);
errval_t event_dispatch_disabled(struct waitset *ws, dispatcher_handle_t handle);
errval_t event_dispatch_debug(struct waitset *ws);
errval_t event_dispatch_non_block(struct waitset *ws);

__END_DECLS

#endif // BARRELFISH_WAITSET_H
