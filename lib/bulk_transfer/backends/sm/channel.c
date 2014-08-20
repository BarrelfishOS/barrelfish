/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_sm.h>

#include "../../helpers.h"
#include "bulk_sm_impl.h" // XXX legal? where to put impl headers?


struct bulk_implementation bulk_sm_implementation = {
    .channel_create  = bulk_sm_channel_create,
    .channel_bind    = bulk_sm_channel_bind,
    .channel_destroy = bulk_sm_channel_destroy,
    .assign_pool     = bulk_sm_assign_pool,
    .remove_pool     = bulk_sm_remove_pool,
    .move            = bulk_sm_move,
    .copy            = bulk_sm_copy,
    .release         = bulk_sm_release,
    .pass            = bulk_sm_pass,
    .request         = NULL, // supported?
};

struct bulk_implementation *bulk_sm_get_implementation(void)
{
    return &bulk_sm_implementation;
}

struct bulk_ctrl_rx_vtbl bulk_ctrl_rx_vtbl = {
    .negotiate_call       = bulk_sm_channel_negotiate_rx_call,
    .negotiate_response   = bulk_sm_channel_negotiate_rx_reply,
    .assign_pool_call     = bulk_sm_assign_pool_rx_call,
    .assign_pool_response = bulk_sm_assign_pool_rx_response,
    .move_untrusted_call  = bulk_sm_move_rx_call,
    .move_trusted_call    = bulk_sm_move_trusted_rx_call,
    .move_response        = bulk_sm_move_rx_response,
    .copy_untrusted_call  = bulk_sm_copy_rx_call,
    .copy_trusted_call    = bulk_sm_copy_trusted_rx_call,
    .copy_response        = bulk_sm_copy_rx_response,
    .pass_untrusted_call  = bulk_sm_pass_rx_call,
    .pass_trusted_call    = bulk_sm_pass_trusted_rx_call,
    .pass_response        = bulk_sm_pass_rx_response,
    .release_call         = bulk_sm_release_rx_call,
    .release_response     = bulk_sm_release_rx_response,
};

// Channel Management -----------------------------------------------------

// Functions involved (C = Creator, B = Binder) (+ = public interface)
// ==================
//
// Endpoint creation:
//   C + bulk_sm_ep_create
//
// Channel creation
//   C + bulk_sm_channel_create
//   C   bulk_sm_channel_create_cb             (Flounder: if.export_cb)
//
// Channel binding 1: establish flounder channel
//   B + bulk_sm_channel_bind
//   C   bulk_sm_channel_connect               (Flounder: if.connect_cb)
//   B   bulk_sm_channel_bind_cb               (Flounder: if.bind_cb)
//
// Channel binding 2: negotiate direction, role and trust level
//   B   bulk_sm_channel_negotiate
//   C   bulk_sm_channel_negotiate_rx_call     (Flounder: if.rx_bind_negotiate_call)
//   C   bulk_sm_channel_negotiate_send_reply
//   C   bulk_sm_channel_negotiate_replied     (Flounder: if.send_bind_negotiate_response_cb)
//   B   bulk_sm_channel_negotiate_rx_reply    (Flounder: if.rx_bind_negotiate_response)
//
// Generalized functions to be used by mulitples (Flounder Helpers):
//   bulk_sm_flounder_msg_sent_debug_cb()

// Channel binding 2: negotiate direction, role and trust level -----------

struct bulk_sm_properties {
    enum bulk_channel_role      role;
    enum bulk_trust_level       trust;
};

void bulk_sm_channel_negotiate_rx_reply(
        struct bulk_ctrl_binding   *b,
        uint64_t                   error,
        enum bulk_ctrl_direction_t match_direction,
        enum bulk_ctrl_role_t      match_role,
        uint64_t                   meta_size)
{
    assert( sizeof(errval_t) == sizeof(uint64_t) );
    errval_t err = (errval_t) error;

    struct bulk_channel      *channel = VOID2CHANNEL(b->st);
    struct bulk_sm_impl_data *data    = CHANNEL_DATA(channel);

    if (err_is_ok(err)) {
        channel->direction = flounder2bulk_direction(match_direction);
        channel->role      = flounder2bulk_role(match_role);
        channel->meta_size = meta_size;
        channel->state     = BULK_STATE_CONNECTED;
    } else {
        channel->state = BULK_STATE_UNINITIALIZED;
    }

    // notify user
    bulk_continuation_call(data->bind_cont, err, channel);
}

static void bulk_sm_channel_negotiate_replied(void *a)
{
    /*
    struct bulk_channel *channel = VOID2CHANNEL(a);

    if (channel->state == BULK_STATE_BIND_NEGOTIATE) {
        // negotiate was successful
        channel->state = BULK_STATE_CONNECTED;

        if (channel->callbacks->bind_received) {
            channel->callbacks->bind_received(channel);
        }
    } else {
        // negotiation failed. go back to wait for binding
        channel->state = BULK_STATE_INITIALIZED;
    }
    */
}

static errval_t bulk_sm_channel_negotiate_send_reply(void *a)
{
    struct bulk_channel      *channel = VOID2CHANNEL(a);
    struct bulk_sm_impl_data *data    = CHANNEL_DATA(channel);
    struct bulk_ctrl_binding *b       = CHANNEL_BINDING(channel);

    struct event_closure txcont = MKCONT(bulk_sm_channel_negotiate_replied,
            channel);

    bulk_ctrl_direction_t peer_direction =
            bulk2flounder_direction(bulk_direction_other(channel->direction));
    bulk_ctrl_role_t peer_role =
            bulk2flounder_role(bulk_role_other(channel->role));

    errval_t err = bulk_ctrl_negotiate_response__tx(b, txcont,
            data->bind_error, peer_direction, peer_role, channel->meta_size);

    if (err_is_ok(err)) {
        // set new channel state. don't do this in
        // bulk_sm_channel_negotiate_replied.
        // Reason: when peer receives negotiate_reply, binding is done. If
        // peer then no longer dispatches events on the waitset, we never get
        // the above notification.
        
        if (channel->state == BULK_STATE_BIND_NEGOTIATE) {
            // negotiate was successful
            channel->state = BULK_STATE_CONNECTED;

            if (channel->callbacks->bind_received) {
                channel->callbacks->bind_received(channel);
            }
        } else {
            // negotiation failed. go back to wait for binding
            channel->state = BULK_STATE_INITIALIZED;
        }
    }

    return err;
}

void bulk_sm_channel_negotiate_rx_call(
        struct bulk_ctrl_binding   *b,
        enum bulk_ctrl_role_t      role,
        enum bulk_ctrl_trust_t     trust)
{
    struct bulk_channel      *channel = VOID2CHANNEL(b->st);
    struct bulk_sm_impl_data *data    = CHANNEL_DATA(channel);

    assert(channel->state == BULK_STATE_BIND_NEGOTIATE);

    // helper structs
    struct bulk_sm_properties me = {
        .role      = channel->role,
        .trust     = channel->trust,
    };

    struct bulk_sm_properties peer = {
        .role      = flounder2bulk_role(role),
        .trust     = flounder2bulk_trust(trust),
    };

    // Let's decide on the properties.
    bool valid = true;

    if (me.role == BULK_ROLE_GENERIC) {
        if (peer.role == BULK_ROLE_GENERIC) {
            me.role   = BULK_ROLE_MASTER;
            peer.role = BULK_ROLE_SLAVE;
        } else {
            me.role   = bulk_role_other(peer.role);
        }
    } else {
        if (peer.role == BULK_ROLE_GENERIC) {
            peer.role = bulk_role_other(me.role);
        } else {
            valid = valid && (me.role == bulk_role_other(peer.role));
        }
    }

    valid = valid && (bulk_trust_compare(me.trust, peer.trust) == 0);

    // Successful?
    if (valid) {
        // update possibly updated role
        channel->role = me.role;
        data->bind_error = SYS_ERR_OK;
    } else {
        // reset binding state
        channel->state = BULK_STATE_BINDING;
        data->bind_error = BULK_TRANSFER_CHAN_BIND;
    }

    bulk_sm_flounder_send_fifo_msg(channel,
                                   bulk_sm_channel_negotiate_send_reply);
}

static errval_t bulk_sm_channel_negotiate(void *a)
{
    struct bulk_channel      *channel = VOID2CHANNEL(a);
    struct bulk_ctrl_binding *b       = CHANNEL_BINDING(channel);

    assert(channel->state == BULK_STATE_BIND_NEGOTIATE);

    struct event_closure txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
            "bulk_sm_channel_negotiate sent");

    errval_t err = bulk_ctrl_negotiate_call__tx(b, txcont,
            bulk2flounder_role(channel->role),
            bulk2flounder_trust(channel->trust)
            );

    return err;
}

// Channel binding 1: establish flounder channel --------------------------

static void bulk_sm_channel_bind_cb(void                     *st,
                                    errval_t                 err,
                                    struct bulk_ctrl_binding *b)
{
    struct bulk_channel      *channel = VOID2CHANNEL(st);
    struct bulk_sm_impl_data *data    = CHANNEL_DATA(channel);
    assert(channel);

    assert(err_is_ok(err)); // current implementation doesn't generate failure

    // mutual pointers
    b->rx_vtbl = bulk_ctrl_rx_vtbl;
    b->st      = channel;
    data->b    = b;

    // channel update
    channel->state     = BULK_STATE_BIND_NEGOTIATE;

    // Flounder channel established. let's negotiate channel properties
    bulk_sm_flounder_send_fifo_msg(channel, bulk_sm_channel_negotiate);
}

static errval_t bulk_sm_channel_connect(void                     *st,
                                        struct bulk_ctrl_binding *b)
{
    struct bulk_channel *channel = VOID2CHANNEL(st);
    assert(channel);

    struct bulk_sm_impl_data *data = CHANNEL_DATA(channel);

    // mutual pointers
    b->rx_vtbl       = bulk_ctrl_rx_vtbl;
    b->error_handler = bulk_sm_error_handler_debug;
    b->st            = channel;
    data->b          = b;

    // channel update
    channel->state     = BULK_STATE_BIND_NEGOTIATE;

    // Let binding side advance channel state and start negotiate properties.
    return SYS_ERR_OK;
}

errval_t bulk_sm_channel_bind(struct bulk_channel      *channel,
                              struct bulk_continuation cont)
{
    assert(channel);
    assert(channel->state == BULK_STATE_UNINITIALIZED);
    assert(channel->waitset);
    assert(channel->ep);

    struct bulk_sm_endpoint_descriptor *ep   = CHANNEL_EP(channel);

    assert(ep->state == BULK_EPSTATE_IREF_EXPORTED);

    // allocate implementation-specific data
    struct bulk_sm_impl_data *data = malloc(sizeof(struct bulk_sm_impl_data));
    channel->impl_data = data;
    if (!data) {
        return BULK_TRANSFER_MEM;
    }
    data->root = NULL;
    thread_mutex_init(&data->mutex);

    thread_mutex_init(&data->resend_lock);
    data->resend_closure = NULL;

    // Bind to iref
    errval_t err = bulk_ctrl_bind(ep->iref,
                                  bulk_sm_channel_bind_cb,
                                  channel,
                                  channel->waitset,
                                  IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bulk_sm_channel_bind");
        free(channel->impl_data);
        channel->impl_data = NULL;
        return BULK_TRANSFER_CHAN_BIND;
    }

    data->bind_cont = cont;
    channel->state = BULK_STATE_BINDING;

    return SYS_ERR_OK;
}

// Channel Creation -------------------------------------------------------

static void bulk_sm_channel_create_cb(void *st, errval_t err, iref_t iref)
{
    struct bulk_channel *channel = VOID2CHANNEL(st);

    assert(channel);

    struct bulk_sm_endpoint_descriptor *ep = CHANNEL_EP(channel);

    assert(ep);
    assert(ep->state == BULK_EPSTATE_CREATED);

    ep->iref  = iref;
    ep->err   = err;
    ep->state = BULK_EPSTATE_IREF_EXPORTED;
}

errval_t bulk_sm_channel_create(struct bulk_channel *channel)
{
    // We cannot use bulk_continuation here as we do not have a channel.
    // Given the interface, we cannot take a barrelfish-style continuation.
    // Mixing different continuation styles in the same library is ugly anyway.
    // Thus, this call is blocking.

    assert(channel);
    assert(channel->state == BULK_STATE_UNINITIALIZED);
    assert(channel->waitset);
    assert(channel->ep);

    struct bulk_sm_endpoint_descriptor *ep = CHANNEL_EP(channel);

    assert(ep->state == BULK_EPSTATE_CREATED); // interface not yet exported

    // export interface and bind iref
    ep->err = bulk_ctrl_export(channel,
                               bulk_sm_channel_create_cb,
                               bulk_sm_channel_connect,
                               channel->waitset,
                               IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(ep->err)) {
        DEBUG_ERR(ep->err, "bulk_sm_channel_create");
        return BULK_TRANSFER_CHAN_CREATE;
    }

    // wait for export to finish
    while (ep->state != BULK_EPSTATE_IREF_EXPORTED) {
        // need to dispatch both, channel waitset and default waitset.
        // explanation very much appreciated.
        // (export uses default?)

        struct bulk_sm_ws_item ws_list[2];
        ws_list[0].ws   = get_default_waitset();
        ws_list[1].ws   = channel->waitset;
        ws_list[0].next = &ws_list[1];
        ws_list[1].next = NULL;

        errval_t err = bulk_sm_multiple_event_dispatch(ws_list);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "bulk_sm_channel_create: event_dispatch");
        }
    }

    if (err_is_fail(ep->err)) {
        DEBUG_ERR(ep->err, "bulk_sm_channel_create");
        return BULK_TRANSFER_CHAN_CREATE;
    }

    // allocate implementation-specific data
    struct bulk_sm_impl_data *data = malloc(sizeof(struct bulk_sm_impl_data));
    channel->impl_data = data;
    if (!data) {
        return BULK_TRANSFER_MEM;
    }
    data->root = NULL;
    thread_mutex_init(&data->mutex);

    thread_mutex_init(&data->resend_lock);
    data->resend_closure = NULL;

    // channel initialized
    channel->state = BULK_STATE_INITIALIZED;
    return SYS_ERR_OK;
}

// Channel destroyal ------------------------------------------------------

errval_t bulk_sm_channel_destroy(struct bulk_channel *channel)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}
