/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/event_queue.h>
#include <string.h>

#include <bulk_transfer/bulk_local.h>

#include "../../helpers.h"


struct local_channel {
    struct bulk_channel *other;
    struct event_queue   events;
};

struct local_event {
    struct event_queue_node eqn;
    struct bulk_channel    *channel;
    void                  (*handler)(struct local_event *);

    union {
        struct {
            errval_t err;
        } bind_done;

        struct {
            struct bulk_pool *pool;
        } pool_assigned;

        struct {
            struct bulk_buffer *buffer;
            void               *meta;
        } move_received;

        struct {
            struct bulk_buffer *buffer;
            void               *meta;
        } buffer_received;

        struct {
            struct bulk_buffer *buffer;
            void               *meta;
        } copy_received;

        struct {
            struct bulk_buffer *buffer;
        } copy_released;
    } params;
};

static errval_t impl_create(struct bulk_channel *channel);
static errval_t impl_bind(struct bulk_channel *channel);
static errval_t impl_assign_pool(struct bulk_channel *channel,
                                 struct bulk_pool    *pool);
static errval_t impl_move(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer,
                          void                *meta,
                          struct bulk_continuation cont);
static errval_t impl_pass(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer,
                          void                *meta,
                          struct bulk_continuation cont);
static errval_t impl_copy(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer,
                          void                *meta,
                          struct bulk_continuation cont);
static errval_t impl_release(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer,
                             struct bulk_continuation cont);

static struct bulk_implementation implementation = {
    .channel_create = impl_create,
    .channel_bind =   impl_bind,
    .assign_pool =    impl_assign_pool,
    .move =           impl_move,
    .pass =           impl_pass,
    .copy =           impl_copy,
    .release =        impl_release,
};

static errval_t init_channel(struct bulk_channel *channel)
{
    struct local_channel *l = malloc(sizeof(*l));
    if (l == NULL) {
        return BULK_TRANSFER_MEM;
    }

    channel->impl_data = l;
    event_queue_init(&l->events, channel->waitset, EVENT_QUEUE_CONTINUOUS);
    return SYS_ERR_OK;
}

static errval_t event_alloc(struct bulk_channel *channel,
                            struct local_event **ev,
                            void               (*handler)(struct local_event *),
                            size_t               extra)
{
    *ev = malloc(sizeof(*ev) + extra);
    if (*ev == NULL) {
        return BULK_TRANSFER_MEM;
    }

    (*ev)->channel = channel;
    (*ev)->handler = handler;
    return SYS_ERR_OK;
}

static void event_handler(void *arg)
{
    struct local_event *lev = arg;
    lev->handler(lev);
    free(lev);
}

static void event_enqueue(struct local_event *lev)
{
    struct local_channel *local = lev->channel->impl_data;
    event_queue_add(&local->events, &lev->eqn,
            MKCLOSURE(event_handler, lev));
}

static void event_bind_done(struct local_event *lev)
{
    lev->channel->callbacks->bind_done(
            lev->channel,
            lev->params.bind_done.err);
}

static void event_pool_assigned(struct local_event *lev)
{
    lev->channel->callbacks->pool_assigned(
            lev->channel,
            lev->params.pool_assigned.pool);
}

static void event_move_received(struct local_event *lev)
{
    lev->channel->callbacks->move_received(
            lev->channel,
            lev->params.move_received.buffer,
            lev->params.move_received.meta);
}

static void event_buffer_received(struct local_event *lev)
{
    lev->channel->callbacks->buffer_received(
            lev->channel,
            lev->params.buffer_received.buffer,
            lev->params.buffer_received.meta);
}

static void event_copy_received(struct local_event *lev)
{
    lev->channel->callbacks->copy_received(
            lev->channel,
            lev->params.copy_received.buffer,
            lev->params.copy_received.meta);
}

static void event_copy_released(struct local_event *lev)
{
    lev->channel->callbacks->copy_released(
            lev->channel,
            lev->params.copy_released.buffer);
}




static errval_t impl_create(struct bulk_channel *channel)
{
    return init_channel(channel);
}

static errval_t impl_bind(struct bulk_channel *channel)
{
    errval_t err;
    struct local_channel *l, *o_l;
    struct bulk_local_endpoint *ep;
    struct local_event *ev, *o_ev;

    err = init_channel(channel);
    if (err_is_fail(err)) {
        return err;
    }

    ep = (struct bulk_local_endpoint *) channel->ep;
    l = channel->impl_data;
    l->other = ep->other_channel;
    o_l = l->other->impl_data;
    o_l->other = channel;

    // Set channel parameters from other side
    channel->role = bulk_role_other(l->other->role);
    channel->direction = bulk_direction_other(l->other->role);
    channel->meta_size = l->other->meta_size;

    // Allocate events
    err = event_alloc(channel, &ev, event_bind_done, 0);
    if (err_is_fail(err)) {
        goto error;
    }
    err = event_alloc(l->other, &o_ev, event_bind_done, 0);
    if (err_is_fail(err)) {
        free(ev);
        goto error;
    }

    // Now we're sure that we can succeed
    channel->state = BULK_STATE_CONNECTED;
    l->other->state = BULK_STATE_CONNECTED;

    // Trigger first event
    ev->params.bind_done.err = SYS_ERR_OK;
    event_enqueue(ev);

    // Trigger second event
    o_ev->params.bind_done.err = SYS_ERR_OK;
    event_enqueue(o_ev);
    return SYS_ERR_OK;

error:
    free(l);
    return err;
}

static errval_t impl_assign_pool(struct bulk_channel *channel,
                                 struct bulk_pool    *pool)
{
    struct local_channel *l = channel->impl_data;
    struct local_event *ev;
    errval_t err;

    err = event_alloc(l->other, &ev, event_pool_assigned, 0);
    if (!err_is_fail(err)) {
        ev->params.pool_assigned.pool = pool;
        event_enqueue(ev);
    }
    return err;
}

static errval_t impl_move(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer,
                          void                *meta,
                          struct bulk_continuation cont)
{
    struct local_channel *l = channel->impl_data;
    struct local_event *ev;
    void *m;
    errval_t err;

    err = event_alloc(l->other, &ev, event_move_received, channel->meta_size);
    if (!err_is_fail(err)) {
        m = ev + 1;
        memcpy(m, meta, channel->meta_size);
        ev->params.move_received.buffer = buffer;
        ev->params.move_received.meta = m;
        event_enqueue(ev);
    }
    return err;
}

static errval_t impl_pass(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer,
                          void                *meta,
                          struct bulk_continuation cont)
{
    struct local_channel *l = channel->impl_data;
    struct local_event *ev;
    void *m;
    errval_t err;

    err = event_alloc(l->other, &ev, event_buffer_received, channel->meta_size);
    if (!err_is_fail(err)) {
        m = ev + 1;
        memcpy(m, meta, channel->meta_size);
        ev->params.buffer_received.buffer = buffer;
        ev->params.buffer_received.meta = m;
        event_enqueue(ev);
    }
    return err;
}

static errval_t impl_copy(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer,
                          void                *meta,
                          struct bulk_continuation cont)
{
    struct local_channel *l = channel->impl_data;
    struct local_event *ev;
    void *m;
    errval_t err;

    err = event_alloc(l->other, &ev, event_copy_received, channel->meta_size);
    if (!err_is_fail(err)) {
        m = ev + 1;
        memcpy(m, meta, channel->meta_size);
        ev->params.copy_received.buffer = buffer;
        ev->params.copy_received.meta = m;
        event_enqueue(ev);
    }
    return err;

}

static errval_t impl_release(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer,
                             struct bulk_continuation cont)
{
    struct local_channel *l = channel->impl_data;
    struct local_event *ev;
    errval_t err;

    err = event_alloc(l->other, &ev, event_copy_released, 0);
    if (!err_is_fail(err)) {
        ev->params.copy_released.buffer = buffer;
        event_enqueue(ev);
    }
    return err;
}


void bulk_local_init_endpoint(struct bulk_local_endpoint *endpoint,
                              struct bulk_channel        *other_channel)
{
    endpoint->generic.f = &implementation;
    endpoint->other_channel = other_channel;
}

