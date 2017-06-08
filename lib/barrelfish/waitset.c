/**
 * \file
 * \brief Waitset and low-level event handling mechanism
 *
 * A "wait set" is a collection of channels to wait on, much like an
 * FDSET in POSIX. There should be a default, static wait set for each
 * dispatcher. Threads which wait for events specify the wait set they
 * are waiting on.
 */

/*
 * Copyright (c) 2009-2012, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/waitset_chan.h>
#include <barrelfish/threads.h>
#include <barrelfish/dispatch.h>
#include "threads_priv.h"
#include "waitset_chan_priv.h"
#include <stdio.h>
#include <string.h>

#include <flounder/flounder.h>

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
#  include <barrelfish/ump_endpoint.h>
#endif

/// Dequeue a chanstate from a queue
static void dequeue(struct waitset_chanstate **queue, struct waitset_chanstate *chan)
{
    if (chan->next == chan) {
        assert(chan->prev == chan);
        assert(*queue == chan);
        *queue = NULL;
    } else {
        chan->prev->next = chan->next;
        chan->next->prev = chan->prev;
        if (*queue == chan) {
            *queue = chan->next;
        }
    }
    chan->prev = chan->next = NULL;
}

/// Enqueue a chanstate on a queue
static void enqueue(struct waitset_chanstate **queue, struct waitset_chanstate *chan)
{
    if (*queue == NULL) {
        *queue = chan;
        chan->next = chan->prev = chan;
    } else {
        chan->next = *queue;
        chan->prev = (*queue)->prev;
        chan->next->prev = chan;
        chan->prev->next = chan;
    }
}

/// Dequeue a chanstate from polled queue
static void dequeue_polled(struct waitset_chanstate **queue,
                            struct waitset_chanstate *chan)
{
    if (chan->polled_next == chan) {
        assert(chan->polled_prev == chan);
        assert(*queue == chan);
        *queue = NULL;
    } else {
        chan->polled_prev->polled_next = chan->polled_next;
        chan->polled_next->polled_prev = chan->polled_prev;
        if (*queue == chan) {
            *queue = chan->polled_next;
        }
    }
    chan->polled_prev = chan->polled_next = NULL;
}

/// Enqueue a chanstate on polled queue
static void enqueue_polled(struct waitset_chanstate **queue,
                            struct waitset_chanstate *chan)
{
    if (*queue == NULL) {
        *queue = chan;
        chan->polled_next = chan->polled_prev = chan;
    } else {
        chan->polled_next = *queue;
        chan->polled_prev = (*queue)->polled_prev;
        chan->polled_next->polled_prev = chan;
        chan->polled_prev->polled_next = chan;
    }
}

/**
 * \brief Initialise a new waitset
 */
void waitset_init(struct waitset *ws)
{
    assert(ws != NULL);
    ws->pending = ws->polled = ws->idle = ws->waiting = NULL;
    ws->waiting_threads = NULL;
}

/**
 * \brief Destroy a previously initialised waitset
 */
errval_t waitset_destroy(struct waitset *ws)
{
    assert(ws != NULL);

    // FIXME: do we want to support cancelling all the pending events/channels?
    if (ws->pending || ws->waiting_threads) {
        return LIB_ERR_WAITSET_IN_USE;
    }

    // remove idle and polled channels from waitset
    struct waitset_chanstate *chan, *next;
    for (chan = ws->idle; chan != NULL; chan = next) {
        next = chan->next;
        assert(chan->state == CHAN_IDLE);
        assert(chan->waitset == ws);
        chan->waitset = NULL;
        chan->next = chan->prev = NULL;

        if (next == ws->idle) {
            break;
        }
    }
    ws->idle = NULL;

    for (chan = ws->polled; chan != NULL; chan = next) {
        next = chan->next;
        assert(chan->state == CHAN_POLLED);
        assert(chan->waitset == ws);
        chan->waitset = NULL;
        chan->next = chan->prev = NULL;

        if (next == ws->polled) {
            break;
        }
    }
    ws->polled = NULL;

    return SYS_ERR_OK;
}

/// Check if the thread can receive the event
static bool waitset_can_receive(struct waitset_chanstate *chan,
                                struct thread *thread)
{
    bool res = false;

    if (chan->wait_for) // if a thread is waiting for this specific event
        res = chan->wait_for == thread;
    else
        res = (chan->token & 1 && !thread->token) // incoming token is a request
            // and a thread is not waiting for a token
            || (!chan->token && chan != thread->channel) // there's no token
            // and a thread is not waiting specifically for that event
            || (chan->token == thread->token && chan == thread->channel);
            // there is a token and it matches thread's token and event
    return res;
}

/// Returns a channel with a pending event on the given waitset matching
/// our thread
static struct waitset_chanstate *get_pending_event_disabled(struct waitset *ws,
          struct waitset_chanstate *waitfor, struct waitset_chanstate *waitfor2)
{
    struct thread *me = thread_self_disabled();

    if (waitfor) { // channel that we wait for
        if ((waitfor->state == CHAN_PENDING || waitfor->state == CHAN_WAITING)
            && waitset_can_receive(waitfor, me)) {
            return waitfor;
        }
        if (waitfor2 && (waitfor2->state == CHAN_PENDING || waitfor2->state == CHAN_WAITING)
            && waitset_can_receive(waitfor2, me)) {
            return waitfor2;
        }
        return NULL;
    }
    struct waitset_chanstate *chan;
    // check a waiting queue for matching event
    for (chan = ws->waiting; chan; ) {
        if (waitset_can_receive(chan, me)) {
            assert_disabled(chan->state == CHAN_WAITING);
            return chan;
        }
        chan = chan->next;
        if (chan == ws->waiting)
            break;
    }
    // check a pending queue for matching event
    for (chan = ws->pending; chan;) {
        if (waitset_can_receive(chan, me)) {
            assert_disabled(chan->state == CHAN_PENDING);
            return chan;
        }
        chan = chan->next;
        if (chan == ws->pending)
            break;
    }
    return NULL;
}

void arranet_polling_loop_proxy(void) __attribute__((weak));
void arranet_polling_loop_proxy(void)
{
    USER_PANIC("Network polling not available without Arranet!\n");
}

void poll_ahci(struct waitset_chanstate *) __attribute__((weak));
void poll_ahci(struct waitset_chanstate *chan)
{
    errval_t err = waitset_chan_trigger(chan);
    assert(err_is_ok(err)); // should not be able to fail
}

/// Check polled channels
void poll_channels_disabled(dispatcher_handle_t handle) {
    struct dispatcher_generic *dp = get_dispatcher_generic(handle);
    struct waitset_chanstate *chan;

    if (!dp->polled_channels)
        return;
    chan = dp->polled_channels;
    do {
        switch (chan->chantype) {
#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
        case CHANTYPE_UMP_IN: {
            if (ump_endpoint_poll(chan)) {
                errval_t err = waitset_chan_trigger_disabled(chan, handle);
                assert(err_is_ok(err)); // should not fail
                if (!dp->polled_channels) // restart scan
                    return;
                chan = dp->polled_channels;
                continue;
            } else
                chan = chan->polled_next;
        } break;
#endif // CONFIG_INTERCONNECT_DRIVER_UMP
        case CHANTYPE_LWIP_SOCKET:
            arranet_polling_loop_proxy();
            break;
        case CHANTYPE_AHCI:
            poll_ahci(chan);
            break;
        default:
            assert(!"invalid channel type to poll!");
        }
    } while (chan != dp->polled_channels);
}

/// Re-register a channel (if persistent)
static void reregister_channel(struct waitset *ws, struct waitset_chanstate *chan,
                                dispatcher_handle_t handle)
{
    assert(chan->waitset == ws);
    if (chan->state == CHAN_PENDING) {
        dequeue(&ws->pending, chan);
    } else {
        assert(chan->state == CHAN_WAITING);
        dequeue(&ws->waiting, chan);
    }

    chan->token = 0;
    if (chan->chantype == CHANTYPE_UMP_IN
        || chan->chantype == CHANTYPE_LWIP_SOCKET
        || chan->chantype == CHANTYPE_AHCI) {
        enqueue(&ws->polled, chan);
        enqueue_polled(&get_dispatcher_generic(handle)->polled_channels, chan);
        chan->state = CHAN_POLLED;
    } else {
        enqueue(&ws->idle, chan);
        chan->state = CHAN_IDLE;
    }
}

/// Find a thread that is able to receive an event
static struct thread * find_recipient(struct waitset *ws,
                        struct waitset_chanstate *channel, struct thread *me)
{
    struct thread *t = ws->waiting_threads;

    if (!t)
        return NULL;
    do {
        if (waitset_can_receive(channel, t))
            return t;
        t = t->next;
    } while (t != ws->waiting_threads);
    return ws->waiting_threads;
}

/// Wake up other thread if there's more pending events
static void wake_up_other_thread(dispatcher_handle_t handle, struct waitset *ws)
{
    if (ws->pending && ws->waiting_threads) {
        struct thread *t;

        t = thread_unblock_one_disabled(handle, &ws->waiting_threads, NULL);
        assert_disabled(t == NULL); // shouldn't see a remote thread
    }
}

/**
 * \brief Get next pending event
 *
 * Check if there is a pending event that matches current thread and return it.
 * Pending events are in a pending queue and in a waiting queue.
 * A pending event then will be removed from a pending/waiting queue and become
 * unregistered or, if it's persistent, will be re-registered to an idle queue
 * or a polled queue (UMP channels) of a waitset.
 * If there's no pending event, block this thread.
 * If there's a pending event but it doesn't match our thread, don't remove it
 * from a pending queue and wake up a matching thread.
 * If there's no matching thread, add it to a waiting queue.
 *
 * \param ws Waitset with sources of events
 * \param retchannel Holder of returned event
 * \param retclosure Holder of returned closure
 * \param waitfor Specific event that we're waiting for (can be NULL)
 * \param handle Dispatcher's handle
 * \param debug Debug mode (not used)
 */

errval_t get_next_event_disabled(struct waitset *ws,
    struct waitset_chanstate **retchannel, struct event_closure *retclosure,
    struct waitset_chanstate *waitfor, struct waitset_chanstate *waitfor2,
    dispatcher_handle_t handle, bool debug)
{
	//debug_printf("----entering get_next_event_disabled function.....\n");
    struct waitset_chanstate * chan;

// debug_printf("%s: %p %p %p %p\n", __func__, __builtin_return_address(0), __builtin_return_address(1), __builtin_return_address(2), __builtin_return_address(3));
    for (;;) {
        chan = get_pending_event_disabled(ws, waitfor, waitfor2); // get our event
        if (chan) {
            *retchannel = chan;
            *retclosure = chan->closure;
            chan->wait_for = NULL;
            chan->token = 0;
            if (chan->persistent)
                reregister_channel(ws, chan, handle);
            else
                waitset_chan_deregister_disabled(chan, handle);
            wake_up_other_thread(handle, ws);
  //   debug_printf("%s.%d: %p\n", __func__, __LINE__, retclosure->handler);
            return SYS_ERR_OK;
        }
        chan = ws->pending; // check a pending queue
        if (!chan) { // if nothing then wait
			//debug_printf("nothing then wait\n");
            thread_block_disabled(handle, &ws->waiting_threads);
            disp_disable();
        } else { // something but it's not our event
			//debug_printf("something then check\n");
            if (!ws->waiting_threads) { // no other thread interested in
                dequeue(&ws->pending, chan);
                enqueue(&ws->waiting, chan);
                chan->state = CHAN_WAITING;
                chan->waitset = ws;
            } else {
                // find a matching thread
				//debug_printf("finding a matching thread\n");
                struct thread *t;
                for (t = ws->waiting_threads; t; ) {
                    if (waitset_can_receive(chan, t)) { // match found, wake it
                        ws->waiting_threads = t;
                        t = thread_unblock_one_disabled(handle,
                                                    &ws->waiting_threads, chan);
                        assert_disabled(t == NULL); // shouldn't see a remote thread
                        break;
                    }
                    t = t->next;
                    if (t == ws->waiting_threads) { // no recipient found
                        dequeue(&ws->pending, chan);
                        enqueue(&ws->waiting, chan);
                        chan->state = CHAN_WAITING;
                        chan->waitset = ws;
                        break;
                    }
                }
            }
        }
    }
}

/**
 * \brief Wait for (block) and return next event on given waitset
 *
 * Wait until something happens, either activity on some channel, or a deferred
 * call, and then return the corresponding closure. This is the core of the
 * event-handling system.
 *
 * \param ws Waitset
 * \param retclosure Pointer to storage space for returned event closure
 */
errval_t get_next_event(struct waitset *ws, struct event_closure *retclosure)
{
    dispatcher_handle_t handle = disp_disable();
    struct waitset_chanstate *channel;
    errval_t err = get_next_event_disabled(ws, &channel, retclosure, NULL, NULL,
                                            handle, false);
    disp_enable(handle);
    return err;
}



/**
 * \brief Check if there is an event pending on given waitset
 *
 * This is essentially a non-blocking variant of get_next_event(). It should be
 * used with great care, to avoid the creation of busy-waiting loops.
 *
 * \param ws Waitset
 *
 * \returns LIB_ERR_NO_EVENT if nothing is pending
 */
static errval_t check_for_event_disabled(struct waitset *ws, dispatcher_handle_t handle)
{
    struct waitset_chanstate *chan;

    poll_channels_disabled(handle);
    chan = get_pending_event_disabled(ws, NULL, NULL);
    if (chan != NULL) {
        return SYS_ERR_OK;
    }
    return LIB_ERR_NO_EVENT;
}

errval_t check_for_event(struct waitset *ws)
{
    errval_t err;

    assert(ws != NULL);
    dispatcher_handle_t handle = disp_disable();
    err = check_for_event_disabled(ws, handle);
    disp_enable(handle);
    return err;
}

/**
 * \brief Wait for (block) and dispatch next event on given waitset
 *
 * Wait until something happens, either activity on some channel, or deferred
 * call, and then call the corresponding closure.
 *
 * \param ws Waitset
 */

errval_t event_dispatch(struct waitset *ws)
{
//    debug_printf("entering event_dispatch\n");
    struct event_closure closure;
    errval_t err = get_next_event(ws, &closure);
    if (err_is_fail(err)) {
//        debug_printf("get next event failed\n");
        return err;
    }

    assert(closure.handler != NULL);
//    debug_printf("event received. executing handler\n");
    closure.handler(closure.arg);
//    debug_printf("event handled\n");
//    debug_printf("returning from event_dispatch\n");
    return SYS_ERR_OK;
}

errval_t event_dispatch_debug(struct waitset *ws)
{
    struct event_closure closure;
    struct waitset_chanstate *channel;
    dispatcher_handle_t handle = disp_disable();
    errval_t err = get_next_event_disabled(ws, &channel, &closure, NULL, NULL,
                                           handle, true);
    disp_enable(handle);
    if (err_is_fail(err)) {
        return err;
    }

    assert(closure.handler != NULL);
    closure.handler(closure.arg);
    return SYS_ERR_OK;
}

/**
 * \brief Dispatch events until a specific event is received
 *
 * Wait for events and dispatch them. If a specific event comes, don't call
 * a closure, just return.
 *
 * \param ws Waitset
 * \param waitfor Event, that we are waiting for
 * \param error_var Error variable that can be changed by closures
 */

errval_t wait_for_channel(struct waitset *ws, struct waitset_chanstate *waitfor,
                          errval_t *error_var)
{
    assert(waitfor->waitset == ws);
    thread_set_local_trigger(NULL);
    for (;;) {
        struct event_closure closure;
        struct waitset_chanstate *channel, *trigger;

        trigger = thread_get_local_trigger();
        dispatcher_handle_t handle = disp_disable();
        errval_t err = get_next_event_disabled(ws, &channel, &closure, waitfor,
            trigger ? trigger: waitfor->trigger, handle, false);
        disp_enable(handle);
        if (err_is_fail(err)) {
            assert(0);
            return err;
        }
        if (channel == waitfor)
            return SYS_ERR_OK;
        assert(!channel->wait_for);
        assert(closure.handler != NULL);
        closure.handler(closure.arg);
        if (err_is_fail(*error_var))
            return *error_var;
    }
}

/**
 * \brief check and dispatch next event on given waitset
 *
 * Check if there is any pending activity on some channel, or deferred
 * call, and then call the corresponding closure.
 *
 * Do not wait!  In case of no pending events, return err LIB_ERR_NO_EVENT.
 *
 * \param ws Waitset
 */
errval_t event_dispatch_non_block(struct waitset *ws)
{
    struct waitset_chanstate *channel;
    struct event_closure closure;

    assert(ws != NULL);

    // are there any pending events on the waitset?
    dispatcher_handle_t handle = disp_disable();
    errval_t err = check_for_event_disabled(ws, handle);
    if (err_is_fail(err)) {
        disp_enable(handle);
        return err;
    }
    err = get_next_event_disabled(ws, &channel, &closure, NULL, NULL, handle,
                                            false);
    if (err_is_fail(err))
        return err;
    disp_enable(handle);
    assert(closure.handler != NULL);
    closure.handler(closure.arg);
    return SYS_ERR_OK;
}


/**
 * \privatesection
 * "Private" functions that are called only by the channel implementations
 */

/**
 * \brief Initialise per-channel waitset state
 *
 * \param chan Channel state
 * \param chantype Channel type
 */
void waitset_chanstate_init(struct waitset_chanstate *chan,
                            enum ws_chantype chantype)
{
    assert(chan != NULL);
    chan->waitset = NULL;
    chan->chantype = chantype;
    chan->state = CHAN_UNREGISTERED;
#ifndef NDEBUG
    chan->prev = chan->next = NULL;
#endif
    chan->persistent = false;
    chan->token = 0;
    chan->wait_for = NULL;
    chan->trigger = NULL;
}

/**
 * \brief Destroy previously-initialised per-channel waitset state
 * \param chan Channel state
 */
void waitset_chanstate_destroy(struct waitset_chanstate *chan)
{
    assert(chan != NULL);
    if (chan->waitset != NULL) {
        errval_t err = waitset_chan_deregister(chan);
        assert(err_is_ok(err)); // can't fail if registered
    }
}

/**
 * \brief Register a closure to be called when a channel is triggered
 *
 * In the Future, call the closure on a thread associated with the waitset
 * when the channel is triggered. Only one closure may be registered per
 * channel state at any one time.
 * This function must only be called when disabled.
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 */
errval_t waitset_chan_register_disabled(struct waitset *ws,
                                        struct waitset_chanstate *chan,
                                        struct event_closure closure)
{
    if (chan->waitset != NULL) {
        return LIB_ERR_CHAN_ALREADY_REGISTERED;
    }

    chan->waitset = ws;
    chan->token = 0;

    // channel must not already be registered!
    assert_disabled(chan->next == NULL && chan->prev == NULL);
    assert_disabled(chan->state == CHAN_UNREGISTERED);

    // this is probably insane! :)
    // assert_disabled(closure.handler != NULL);

    // store closure
    chan->closure = closure;

    // enqueue this channel on the waitset's queue of idle channels
    enqueue(&ws->idle, chan);
    chan->state = CHAN_IDLE;

    return SYS_ERR_OK;
}

/**
 * \brief Register a closure on a channel, and mark the channel as polled
 *
 * In the Future, call the closure on a thread associated with the waitset
 * when the channel is triggered. Only one closure may be registered per
 * channel state at any one time. Additionally, mark the channel as polled.
 * This function must only be called when disabled.
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 * \param disp Current dispatcher pointer
 */
errval_t waitset_chan_register_polled_disabled(struct waitset *ws,
                                               struct waitset_chanstate *chan,
                                               struct event_closure closure,
                                               dispatcher_handle_t handle)
{
    if (chan->waitset != NULL) {
        return LIB_ERR_CHAN_ALREADY_REGISTERED;
    }

    chan->waitset = ws;
    chan->token = 0;

    // channel must not already be registered!
    assert_disabled(chan->next == NULL && chan->prev == NULL);
    assert_disabled(chan->state == CHAN_UNREGISTERED);

    // store closure
    chan->closure = closure;

    // enqueue this channel on the waitset's queue of polled channels
    enqueue(&ws->polled, chan);
    chan->state = CHAN_POLLED;
    enqueue_polled(&get_dispatcher_generic(handle)->polled_channels, chan);

    return SYS_ERR_OK;
}

/**
 * \brief Register a closure to be called when a channel is triggered
 *
 * In the Future, call the closure on a thread associated with the waitset
 * when the channel is triggered. Only one closure may be registered per
 * channel state at any one time.
 * This function must only be called when enabled.
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 */
errval_t waitset_chan_register(struct waitset *ws, struct waitset_chanstate *chan,
                               struct event_closure closure)
{
    dispatcher_handle_t handle = disp_disable();
    errval_t err = waitset_chan_register_disabled(ws, chan, closure);
    disp_enable(handle);
    return err;
}

/**
 * \brief Register a closure on a channel, and mark the channel as polled
 *
 * In the Future, call the closure on a thread associated with the waitset
 * when the channel is triggered. Only one closure may be registered per
 * channel state at any one time. Additionally, mark the channel as polled.
 * This function must only be called when enabled. It is equivalent to
 * calling waitset_chan_register() followed by waitset_chan_start_polling().
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 */
errval_t waitset_chan_register_polled(struct waitset *ws,
                                      struct waitset_chanstate *chan,
                                      struct event_closure closure)
{
    dispatcher_handle_t handle = disp_disable();
    errval_t err = waitset_chan_register_polled_disabled(ws, chan, closure, handle);
    disp_enable(handle);
    return err;
}

/**
 * \brief Cancel a previous callback registration
 *
 * Remove the registration for a callback on the given channel.
 * This function must only be called when disabled.
 *
 * \param chan Waitset's per-channel state
 */
errval_t waitset_chan_deregister_disabled(struct waitset_chanstate *chan,
                                          dispatcher_handle_t handle)
{
    assert_disabled(chan != NULL);
    struct waitset *ws = chan->waitset;
    if (ws == NULL) {
        return LIB_ERR_CHAN_NOT_REGISTERED;
    }

    // remove this channel from the queue in which it is waiting
    chan->waitset = NULL;
    assert_disabled(chan->next != NULL && chan->prev != NULL);

    switch (chan->state) {
    case CHAN_IDLE:
        dequeue(&ws->idle, chan);
        break;

    case CHAN_POLLED:
        dequeue(&ws->polled, chan);
        dequeue_polled(&get_dispatcher_generic(handle)->polled_channels, chan);
        break;

    case CHAN_PENDING:
        dequeue(&ws->pending, chan);
        break;

    case CHAN_WAITING:
        dequeue(&ws->waiting, chan);
        break;

    default:
        assert_disabled(!"invalid channel state in deregister");
    }
    chan->state = CHAN_UNREGISTERED;
    chan->wait_for = NULL;
    return SYS_ERR_OK;
}

/**
 * \brief Cancel a previous callback registration
 *
 * Remove the registration for a callback on the given channel.
 * This function must only be called when enabled.
 *
 * \param chan Waitset's per-channel state
 */
errval_t waitset_chan_deregister(struct waitset_chanstate *chan)
{
    dispatcher_handle_t handle = disp_disable();
    errval_t err = waitset_chan_deregister_disabled(chan, handle);
    disp_enable(handle);
    return err;
}

/**
 * \brief Migrate callback registrations to a new waitset.
 *
 * \param chan Old waitset's per-channel state to migrate
 * \param new_ws New waitset to migrate to
 */
void waitset_chan_migrate(struct waitset_chanstate *chan,
                          struct waitset *new_ws)
{
    struct waitset *ws = chan->waitset;

    // Only when registered
    if(ws == NULL) {
        return;
    }

    switch(chan->state) {
    case CHAN_IDLE:
        dequeue(&ws->idle, chan);
        enqueue(&new_ws->idle, chan);
        break;

    case CHAN_POLLED:
        dequeue(&ws->polled, chan);
        enqueue(&new_ws->polled, chan);
        break;

    case CHAN_PENDING:
        dequeue(&ws->pending, chan);
        enqueue(&new_ws->pending, chan);
        break;

    case CHAN_WAITING:
        dequeue(&ws->waiting, chan);
        enqueue(&new_ws->waiting, chan);
        break;

    case CHAN_UNREGISTERED:
        // Do nothing
        break;
    }

    // Remember new waitset association
    chan->waitset = new_ws;
}

/**
 * \brief Trigger an event callback on a channel
 *
 * Marks the given channel as having a pending event, causing some future call
 * to get_next_event() to return the registered closure.
 * This function must only be called when disabled.
 *
 * \param chan Waitset's per-channel state
 * \param disp Current dispatcher pointer
 */
errval_t waitset_chan_trigger_disabled(struct waitset_chanstate *chan,
                                       dispatcher_handle_t handle)
{
    assert_disabled(chan != NULL);
    struct waitset *ws = chan->waitset;
    assert_disabled(ws != NULL);
    assert_disabled(chan->prev != NULL && chan->next != NULL);

    // no-op if already pending
    if (chan->state == CHAN_PENDING) {
        return SYS_ERR_OK;
    }

    // remove from previous queue (either idle or polled)
    if (chan->state == CHAN_IDLE) {
        dequeue(&ws->idle, chan);
    } else {
        assert_disabled(chan->state == CHAN_POLLED);
        dequeue(&ws->polled, chan);
        dequeue_polled(&get_dispatcher_generic(handle)->polled_channels, chan);
    }

    // else mark channel pending and move to end of pending event queue
    enqueue(&ws->pending, chan);
    chan->state = CHAN_PENDING;

    // is there a thread blocked on this waitset? if so, awaken it with the event
    struct thread *thread = find_recipient(ws, chan, thread_self_disabled());
    if (thread) {
        struct thread *t;
        ws->waiting_threads = thread;
        t = thread_unblock_one_disabled(handle, &ws->waiting_threads, chan);
        assert_disabled(t == NULL);
    }
    return SYS_ERR_OK;
}

/**
 * \brief Trigger an event callback on a channel
 *
 * Marks the given channel as having a pending event, causing some future call
 * to get_next_event() to return the registered closure.
 * This function must only be called when enabled.
 *
 * \param chan Waitset's per-channel state
 * \param disp Current dispatcher pointer
 */
errval_t waitset_chan_trigger(struct waitset_chanstate *chan)
{
    dispatcher_handle_t handle = disp_disable();
    errval_t err = waitset_chan_trigger_disabled(chan, handle);
    disp_enable(handle);
    return err;
}

/**
 * \brief Trigger a specific event callback on an unregistered channel
 *
 * This function is equivalent to waitset_chan_register_disabled() immediately
 * followed by waitset_chan_trigger_disabled(), but avoids unneccessary queue
 * manipulation. This function must only be called when disabled.
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 * \param disp Current dispatcher pointer
 */
errval_t waitset_chan_trigger_closure_disabled(struct waitset *ws,
                                               struct waitset_chanstate *chan,
                                               struct event_closure closure,
                                               dispatcher_handle_t handle)
{
    assert_disabled(chan != NULL);
    assert_disabled(ws != NULL);

    // check if already registered
    if (chan->waitset != NULL || chan->state != CHAN_UNREGISTERED) {
        return LIB_ERR_CHAN_ALREADY_REGISTERED;
    }

    assert_disabled(chan->prev == NULL && chan->next == NULL);

    // set closure
    chan->closure = closure;

    // mark channel pending and place on end of pending event queue
    chan->waitset = ws;
    enqueue(&ws->pending, chan);
    // if (first)
    //     ws->pending = chan;
    chan->state = CHAN_PENDING;

    // is there a thread blocked on this waitset? if so, awaken it with the event
    struct thread *thread = find_recipient(ws, chan, thread_self_disabled());
    if (thread) {
        struct thread *t;
        ws->waiting_threads = thread;
        t = thread_unblock_one_disabled(handle, &ws->waiting_threads, chan);
        assert_disabled(t == NULL);
    }
    return SYS_ERR_OK;
}


/**
 * \brief Trigger a specific event callback on an unregistered channel
 *
 * This function is equivalent to waitset_chan_register()
 * followed by waitset_chan_trigger(), but avoids unneccessary queue
 * manipulation. This function must only be called when enabled.
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 */
errval_t waitset_chan_trigger_closure(struct waitset *ws,
                                      struct waitset_chanstate *chan,
                                      struct event_closure closure)
{
    dispatcher_handle_t disp = disp_disable();
    errval_t err = waitset_chan_trigger_closure_disabled(ws, chan, closure, disp);
    disp_enable(disp);
    return err;
}
