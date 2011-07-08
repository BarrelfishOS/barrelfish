/**
 * \file
 * \brief Bidirectional Beehive messaging channel implementation
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/bmp_chan.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/idc_export.h>
#include "waitset_chan.h"
#include <if/monitor_defs.h>

#ifndef CONFIG_INTERCONNECT_DRIVER_BMP
#error "This file shouldn't be compiled without CONFIG_INTERCONNECT_DRIVER_BMP"
#endif

/**
 * \brief Initialise a new Beehive channel
 *
 * Most code should be using one of bmp_chan_bind() or bmp_chan_accept().
 *
 * \param c Storage for channel state
 */
void bmp_chan_init(struct bmp_chan *c)
{
    assert(c != NULL);

    c->inep = NULL;
    waitset_chanstate_init(&c->send_waitset, CHANTYPE_BMP_OUT);
    memset(&c->cap_handlers, 0, sizeof(c->cap_handlers));
    c->connstate = BMP_DISCONNECTED;
    c->iref = 0;
    c->monitor_binding = get_monitor_binding(); // default
}

/// Destroy the local state associated with a given channel
void bmp_chan_destroy(struct bmp_chan *c)
{
    USER_PANIC("NYI");
}

/// Handler for bind reply messages from the Monitor
static void bind_bmp_reply_handler(struct monitor_binding *b, uintptr_t mon_id,
                                   uintptr_t conn_id, struct capref bee_ep,
                                   size_t remote_ep_len, errval_t success)
{
    struct bmp_chan *c = (void *)conn_id;

    assert(c->connstate == BMP_BIND_WAIT);

    if (err_is_ok(success)) { /* bind succeeded */
        c->connstate = BMP_CONNECTED;
        c->monitor_id = mon_id;
        c->outepcap = bee_ep;
        c->outeplen = remote_ep_len;
    } else { /* bind failed */
        c->connstate = BMP_DISCONNECTED;
        /* TODO: delete endpoint, destroy local_cap */
    }

    /* either way, tell the user what happened */
    assert(c->bind_continuation.handler != NULL);
    c->bind_continuation.handler(c->bind_continuation.st, success, c);
}

struct bind_bmp_reply_state {
    struct monitor_binding *b;
    struct bmp_chan *c;
    uintptr_t monitor_id;
    errval_t bind_success;
    struct event_queue_node qnode;
};

static void send_bind_reply(void *arg)
{
    struct bind_bmp_reply_state *st = arg;
    struct monitor_binding *b = st->b;
    struct bmp_chan *c = st->c;
    errval_t err;

    // send back a bind success/failure message to the monitor
    err =
        st->b->tx_vtbl.bind_bmp_reply_monitor(b, NOP_CONT, st->monitor_id,
                                              (uintptr_t)c,
                                              c ? c->inepcap : NULL_CAP,
                                              st->bind_success);
    if (err_is_ok(err)) {
        event_mutex_unlock(&b->mutex);
        free(st);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = st->b->register_send(b, b->waitset, MKCONT(send_bind_reply,st));
        assert(err_is_ok(err)); // shouldn't fail, as we have the mutex
    } else {
        event_mutex_unlock(&b->mutex);
        DEBUG_ERR(err, "failed sending back reply to BMP bind request;"
                       " request dropped!");
        if (c != NULL) {
            bmp_chan_destroy(c);
            // FIXME: how do we tell the binding about this!?
        }
        free(st);
    }
}

/// Handler for BMP bind request messages from the Monitor
static void bind_bmp_service_request_handler(struct monitor_binding *b,
                                             uintptr_t service_id,
                                             uintptr_t mon_id,
                                             struct capref bee_ep,
                                             size_t remote_ep_len)
{
    struct idc_export *e = (void *)service_id;
    errval_t err;

    // call the binding's connect handler
    if (e->bmp_connect_callback != NULL) {
        err = e->bmp_connect_callback(e->connect_cb_st, b, mon_id, bee_ep,
                                      remote_ep_len);
    } else {
        err = LIB_ERR_NO_BMP_BIND_HANDLER;
    }

    if (err_is_fail(err)) {
        bmp_chan_send_bind_reply(b, NULL, err, mon_id);
    } else {
        // binding is responsible for sending reply
    }
}

void bmp_chan_send_bind_reply(struct monitor_binding *mb,
                              struct bmp_chan *c, errval_t err,
                              uintptr_t monitor_id)
{
    struct bind_bmp_reply_state *st = malloc(sizeof(struct bind_bmp_reply_state));
    assert(st != NULL);

    if (err_is_ok(err)) {
        assert(c != NULL);
    } else {
        assert(c == NULL);
    }

    st->b = mb;
    st->c = c;
    st->bind_success = err;
    st->monitor_id = monitor_id;

    // wait for the ability to use the monitor binding
    event_mutex_enqueue_lock(&mb->mutex, &st->qnode, MKCONT(send_bind_reply, st));
}

static void send_bind_cont(void *arg)
{
    struct bmp_chan *c = arg;
    struct monitor_binding *b = c->monitor_binding;
    errval_t err;

    /* Send bind request to the monitor */
    assert(c->monitor_binding == b);
    assert(b->tx_vtbl.bind_bmp_client_request);
    err = b->tx_vtbl.bind_bmp_client_request(b, NOP_CONT, c->iref,
                                             (uintptr_t)c, c->inepcap);
    if (err_is_ok(err)) { // request sent ok
        event_mutex_unlock(&b->mutex);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        // register to retry
        err = b->register_send(b, b->waitset, MKCONT(send_bind_cont, c));
        assert(err_is_ok(err)); // we hold the monitor binding mutex
    } else { // permanent failure sending message
        event_mutex_unlock(&b->mutex);
        c->bind_continuation.handler(c->bind_continuation.st,
                                     err_push(err, LIB_ERR_BIND_BMP_REQ), NULL);
    }
}

/**
 * \brief Initialise a new BMP channel and initiate a binding
 *
 * \param c  Storage for channel state
 * \param cont Continuation for bind completion/failure
 * \param qnode Storage for an event queue node (used for queuing bind request)
 * \param iref IREF to which to bind
 * \param monitor_binding Monitor binding to use
 * \param ep_buflen Length of local endpoint buffer to allocate, in words
 */
errval_t bmp_chan_bind(struct bmp_chan *c, struct bmp_bind_continuation cont,
                       struct event_queue_node *qnode,  iref_t iref,
                       struct monitor_binding *monitor_binding,
                       size_t ep_buflen)
{
    errval_t err;

    // initialise channel state
    bmp_chan_init(c);

    // store bind args
    c->bind_continuation = cont;
    c->monitor_binding = monitor_binding;
    c->iref = iref;

    // allocate a local endpoint
    err = endpoint_create(ep_buflen, &c->inepcap, &c->inep);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }

    // wait for the ability to use the monitor binding
    c->connstate = BMP_BIND_WAIT;
    event_mutex_enqueue_lock(&monitor_binding->mutex, qnode,
                             MKCONT(send_bind_cont, c));

    return SYS_ERR_OK;
}

/**
 * \brief Initialise a new BMP channel to accept an incoming binding request
 *
 * \param c  Storage for channel state
 * \param mon_id Monitor's connection ID for this channel
 * \param remote_ep Capability to remote endpoint
 * \param remot_ep_len Length of local endpoint buffer to allocate, in words
 * \param local_ep_len Length of local endpoint buffer to allocate, in words
 */
errval_t bmp_chan_accept(struct bmp_chan *c, uintptr_t mon_id,
                         struct capref remote_ep, size_t remote_ep_len,
                         size_t local_ep_len)
{
    errval_t err;

    bmp_chan_init(c);
    c->monitor_id = mon_id;
    c->outepcap = remote_ep;
    c->outeplen = remote_ep_len;

    // allocate a local endpoint
    err = endpoint_create(local_ep_len, &c->inepcap, &c->inep);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }

    /* mark connected */
    c->connstate = BMP_CONNECTED;
    return SYS_ERR_OK;
}

/**
 * \brief Register an event handler to be notified when messages can be sent
 *
 * In the future, call the closure on the given waitset when it is likely that
 * a message can be sent on the channel. A channel may only be registered
 * with a single send event handler on a single waitset at any one time.
 *
 * \param c BMP channel
 * \param ws Waitset
 * \param closure Event handler
 */
errval_t bmp_chan_register_send(struct bmp_chan *c, struct waitset *ws,
                                 struct event_closure closure)
{
    assert(c != NULL);
    assert(ws != NULL);

    errval_t err = waitset_chan_register(ws, &c->send_waitset, closure);
    if (err_is_fail(err)) {
        return err;
    }

    // enqueue in list of channels with a registered event to retry sending
    assert(c->next == NULL && c->prev == NULL);
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *dp = get_dispatcher_generic(handle);
    if (dp->bmp_send_events_list == NULL) {
        dp->bmp_send_events_list = c;
        c->next = c->prev = c;
    } else {
        c->prev = dp->bmp_send_events_list->prev;
        c->next = dp->bmp_send_events_list;
        c->prev->next = c;
        c->next->prev = c;
    }
    disp_enable(handle);

    return err;
}

/**
 * \brief Cancel an event registration made with bmp_chan_register_send()
 *
 * \param lc BMP channel
 */
errval_t bmp_chan_deregister_send(struct bmp_chan *c)
{
    assert(c != NULL);
    errval_t err = waitset_chan_deregister(&c->send_waitset);
    if (err_is_fail(err)) {
        return err;
    }

    // dequeue from list of channels with send events
    assert(c->next != NULL && c->prev != NULL);
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *dp = get_dispatcher_generic(handle);
    if (c->next == c->prev) {
        assert_disabled(dp->bmp_send_events_list == c);
        dp->bmp_send_events_list = NULL;
    } else {
        c->prev->next = c->next;
        c->next->prev = c->prev;
        if (dp->bmp_send_events_list == c) {
            dp->bmp_send_events_list = c->next;
        }
    }
#ifndef NDEBUG
    c->prev = c->next = NULL;
#endif

    disp_enable(handle);
    return err;
}

/**
 * \brief Trigger send events for all BMP channels that are registered
 *
 * We don't have a good way to determine when we are likely to be able
 * to send on an BMP channel, so this function just trigger all such
 * pending events every time the dispatcher is rescheduled.
 *
 * Must be called while disabled and from dispatcher logic.
 */
void bmp_channels_retry_send_disabled(dispatcher_handle_t handle)
{
    struct dispatcher_generic *dp = get_dispatcher_generic(handle);
    struct bmp_chan *c, *first = dp->bmp_send_events_list, *next;
    errval_t err;

    for (c = first; c != NULL; c = next) {
        next = c->next;
        assert(next != NULL);
        err = waitset_chan_trigger_disabled(&c->send_waitset, handle);
        assert_disabled(err_is_ok(err)); // shouldn't fail
#ifndef NDEBUG
        c->next = c->prev = NULL;
#endif
        if (next == first) {
            break; // wrapped
        }
    }

    dp->bmp_send_events_list = NULL;
}

/// Initialise the Beehive channel driver
void bmp_init(void)
{
    struct monitor_binding *mcb = get_monitor_binding();
    mcb->rx_vtbl.bind_bmp_reply_client = bind_bmp_reply_handler;
    mcb->rx_vtbl.bind_bmp_service_request = bind_bmp_service_request_handler;
}
