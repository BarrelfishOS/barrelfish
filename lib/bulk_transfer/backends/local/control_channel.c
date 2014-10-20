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

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_allocator.h>
#include <bulk_transfer/bulk_local.h>

#include "../../bulk_pool.h"
#include "../../bulk_buffer.h"

#include "../../helpers.h"

//#define IMPL_DEBUG(fmt, msg...) debug_printf("%s: "fmt"\n", __func__, msg);
#define IMPL_DEBUG(fmt, msg...)

//#define EVENT_DEBUG(fmt, msg...) debug_printf("%s: "fmt"\n", __func__, msg);
#define EVENT_DEBUG(fmt, msg...)
//#define EVENT_DEBUG_TRACE debug_printf("%s\n", __func__);
#define EVENT_DEBUG_TRACE

struct local_channel
{
    struct bulk_channel *other;
    struct event_queue events;
};

struct local_event
{
    struct event_queue_node eqn;
    struct bulk_channel *channel;
    void (*handler)(struct local_event *);
    struct bulk_continuation cont;
    union
    {
        struct
        {
            errval_t err;
        } status;

        struct
        {
            errval_t err;
        } bind_done;

        struct
        {
            errval_t err;
            struct bulk_pool *pool;
        } pool_assigned;

        struct
        {
            struct bulk_pool_id pool_id;
            size_t buf_id;
            struct bulk_buffer *buffer;
            void *meta;
        } move_received;

        struct
        {
            struct bulk_buffer *buffer;
            struct bulk_pool_id pool_id;
            size_t buf_id;
            void *meta;
        } buffer_received;

        struct
        {
            struct bulk_pool_id pool_id;
            size_t buf_id;
            struct bulk_buffer *buffer;
            void *meta;
        } copy_received;

        struct
        {
            struct bulk_pool_id pool_id;
            size_t buf_id;
            struct bulk_buffer *buffer;
        } copy_released;
    } params;
};

static void event_handler(void *arg);

static errval_t impl_create(struct bulk_channel *channel);
static errval_t impl_bind(struct bulk_channel *channel,
                          struct bulk_continuation cont);
static errval_t impl_assign_pool(struct bulk_channel *channel,
                                 struct bulk_pool *pool,
                                 struct bulk_continuation cont);
static errval_t impl_move(struct bulk_channel *channel,
                          struct bulk_buffer *buffer,
                          void *meta,
                          struct bulk_continuation cont);
static errval_t impl_pass(struct bulk_channel *channel,
                          struct bulk_buffer *buffer,
                          void *meta,
                          struct bulk_continuation cont);
static errval_t impl_copy(struct bulk_channel *channel,
                          struct bulk_buffer *buffer,
                          void *meta,
                          struct bulk_continuation cont);
static errval_t impl_release(struct bulk_channel *channel,
                             struct bulk_buffer *buffer,
                             struct bulk_continuation cont);

static struct bulk_implementation implementation = {
    .channel_create = impl_create,
    .channel_bind = impl_bind,
    .assign_pool = impl_assign_pool,
    .move = impl_move,
    .pass = impl_pass,
    .copy = impl_copy,
    .release = impl_release, };

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

/* ----------------------- event management --------------------------------*/

/**
 * allocates a new event
 */
static errval_t event_alloc(struct bulk_channel *channel,
                            struct local_event **ev,
                            void (*handler)(struct local_event *),
                            size_t extra)
{
    *ev = malloc(sizeof(struct local_event) + extra);
    if (*ev == NULL) {
        return BULK_TRANSFER_MEM;
    }

    (*ev)->channel = channel;
    (*ev)->handler = handler;
    return SYS_ERR_OK;
}

/**
 * enqueues the local event to the event queue of the channel
 */
static void event_enqueue(struct local_event *lev)
{
    struct local_channel *local = lev->channel->impl_data;
    event_queue_add(&local->events, &lev->eqn, MKCLOSURE(event_handler, lev));
}

/* ========================= EVENT HANDLERS ================================ */

/* ------------------------- event handlers -------------------------------- */

/**
 *
 */
static void event_handler(void *arg)
{
    struct local_event *lev = arg;
    lev->handler(lev);
    free(lev);
}

static void event_op_done(struct local_event *lev)
{
    EVENT_DEBUG_TRACE
    if (lev->cont.handler) {
        lev->cont.handler(NULL, SYS_ERR_OK, lev->channel);
    } else {
        EVENT_DEBUG("event_op_done(): no handler set...\n");
    }
    free(lev);
}

/* -------------------------- event bind ----------------------------------- */

/**
 * Gets called when the binding procedure is over.
 *
 * Side: Binding Domain.
 */
static void event_bind_done(struct local_event *lev)
{
    EVENT_DEBUG_TRACE;
    assert(lev);

    if (lev->cont.handler == NULL) {
        EVENT_DEBUG("%s", "handler not set");
        return;
    }

    lev->cont.handler(lev->cont.arg, lev->params.bind_done.err, lev->channel);
}

/**
 * Gets called when a bind request has been received
 *
 * Side: Creating Side
 */
static void event_bind_received(struct local_event *lev)
{
    EVENT_DEBUG_TRACE

    errval_t err, reply = SYS_ERR_OK;
    struct local_event *ev;
    struct local_channel *l = lev->channel->impl_data;

    /* do the callback to the application to inform about the binding */
    if (lev->channel->callbacks->bind_received) {
        err = lev->channel->callbacks->bind_received(lev->channel);
        if (err_is_fail(err)) {
            reply = err;
        }
    } else {
        /* XXX: or if no cb set, just say SYS_ERR_OK ? */
        reply = BULK_TRANSFER_NO_CALLBACK;
    }

    /* allocate and trigger event bind_done */
    err = event_alloc(l->other, &ev, event_bind_done, 0);
    if (!err_is_fail(err)) {
        ev->params.bind_done.err = reply;
        ev->cont = lev->cont;
        event_enqueue(ev);
    }
}

/**
 * Implementation specific bind procedure
 *
 * Side: Binding Side
 */
static errval_t impl_bind(struct bulk_channel *channel,
                          struct bulk_continuation cont)
{
    errval_t err;
    struct local_channel *l, *o_l;
    struct bulk_local_endpoint *ep;
    struct local_event *ev;

    /* Initialize the channel */
    err = init_channel(channel);
    if (err_is_fail(err)) {
        return err;
    }

    /* setting the pointers to the other channel */
    ep = (struct bulk_local_endpoint *) channel->ep;
    l = channel->impl_data;
    l->other = ep->other_channel;
    o_l = l->other->impl_data;
    o_l->other = channel;

    /* set channel parameters from the other side */
    channel->role = bulk_role_other(l->other->role);
    channel->direction = bulk_direction_other(l->other->role);
    channel->meta_size = l->other->meta_size;

    /* update the channel state */
    channel->state = BULK_STATE_CONNECTED;
    l->other->state = BULK_STATE_CONNECTED;

    /* allocate and trigger the bind event to the other side */
    err = event_alloc(l->other, &ev, event_bind_received, 0);
    if (err_is_fail(err)) {
        goto error;
    }

    // Trigger first event
    ev->cont = cont;
    event_enqueue(ev);

    return SYS_ERR_OK;

    error: free(l);

    return err;
}

/* -------------------------- event pool assign ---------------------------- */

/**
 * Gets called when the pool assignment on the other side is completed
 *
 * Side: Assigning Side
 */
static void event_pool_assigned(struct local_event *lev)
{
    EVENT_DEBUG_TRACE

    errval_t err;
    errval_t result = lev->params.pool_assigned.err;

    if (lev->cont.handler) {
        if (!err_is_fail(result)) {
            err = bulk_pool_assign(lev->params.pool_assigned.pool,
                                   lev->channel);
            if (err_is_fail(err)) {
                result = err;
            }
        }

        EVENT_DEBUG(" > [%s]", (result==SYS_ERR_OK) ? "Success", "Failure");

        /* call the continuation */
        lev->cont.handler(lev->params.pool_assigned.pool, result, lev->channel);
    } else {
        EVENT_DEBUG("%s", "continuation handler not set");
    }
}

/**
 * Gets called when a pool is assigned to the channel
 *
 * Side: Other
 */
static void event_pool_assign(struct local_event *lev)
{
    EVENT_DEBUG_TRACE

    errval_t err;
    errval_t assigned;
    struct local_event *ev;

    struct bulk_pool *pool = lev->params.pool_assigned.pool;
    struct bulk_channel *chan = lev->channel;
    struct local_channel *l = chan->impl_data;

    if (bulk_pool_is_assigned(pool, chan)) {
        /* channel is already assigned */
        EVENT_DEBUG("pool [%p] is already assigned to channel.", pool);
        err = event_alloc(l->other, &ev, event_pool_assigned, 0);
        if (!err_is_fail(err)) {
            ev->params.pool_assigned.err = BULK_TRANSFER_POOL_ALREADY_ASSIGNED;
            ev->cont = lev->cont;
            event_enqueue(ev);
        }
        return;
    }

    /* allocate the structures for the pool */
    err = bulk_pool_alloc_with_id(&pool, pool->num_buffers, pool->buffer_size,
                                  pool->id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to allocate pool struct\n");
        return;
    }

    /*
     * prepare the cap
     */
    if (lev->params.pool_assigned.pool->trust == BULK_TRUST_FULL) {
        err = slot_alloc(&pool->pool_cap);
        if (err_is_fail(err)) {
            EVENT_DEBUG("could not allocate a new slot for the cap: %s",
                            err_getstring(err));
            assigned = err;
            goto done;
        }

        err = cap_copy(pool->pool_cap,
                       lev->params.pool_assigned.pool->pool_cap);
        if (err_is_fail(err)) {
            EVENT_DEBUG("could not allocate a new slot for the cap: %s",
                            err_getstring(err));
            assigned = err;
            goto done;
        }
    }

    pool->trust = lev->params.pool_assigned.pool->trust;

    /*
     * XXX: we set the trust level to none here to avoid the creation of
     *      the buffer caps. These have already been created and cannot
     *      be created a second time. [SYS_ERR_REVOKE_FIRST]
     */

    err = bulk_pool_map(pool);
    if (err_is_fail(err)) {
        assigned = err;
        goto done;
    }

    assert(lev->channel->callbacks->pool_assigned);
    if (lev->channel->callbacks->pool_assigned) {
        assigned = lev->channel->callbacks->pool_assigned(lev->channel, pool);
    } else {
        /* XXX: or if no cb set, just say SYS_ERR_OK ? */
        assigned = BULK_TRANSFER_NO_CALLBACK;
    }
    done:

    if (err_is_fail(assigned)) {
        bulk_pool_unmap(pool);
        bulk_pool_dealloc(pool);
    } else {
        err = bulk_pool_assign(pool, lev->channel);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Failed to assign the pool to the channel\n");
        }
    }

    err = event_alloc(l->other, &ev, event_pool_assigned, 0);
    if (!err_is_fail(err)) {
        ev->params.pool_assigned.err = assigned;
        ev->params.pool_assigned.pool = lev->params.pool_assigned.pool;
        ev->cont = lev->cont;
        event_enqueue(ev);
    }

}

/**
 * Implementation specific handler for pool assing requests
 *
 * Side: Assigning Side
 */
static errval_t impl_assign_pool(struct bulk_channel *channel,
                                 struct bulk_pool *pool,
                                 struct bulk_continuation cont)
{
    errval_t err;
    struct local_event *ev;
    struct local_channel *l = channel->impl_data;

    /* allocate and trigger the event */
    err = event_alloc(l->other, &ev, event_pool_assign, 0);
    if (!err_is_fail(err)) {
        ev->params.pool_assigned.pool = pool;
        ev->cont = cont;
        event_enqueue(ev);
    }
    return err;
}

/* -------------------------- event buffer move ---------------------------- */

/**
 * Gets called when a buffer arrives via move operation
 *
 * Side: Receiving Side (SINK)
 */
static void event_move_received(struct local_event *lev)
{
    errval_t err;

    struct bulk_pool *pool = bulk_pool_get(&lev->params.move_received.pool_id,
                                           lev->channel);
    size_t bufid = lev->params.copy_released.buf_id;

    EVENT_DEBUG("  > pool=%p, bufid=%x", pool, (unsigned int )bufid);
    assert(pool);

    struct bulk_buffer *buf = pool->buffers[bufid];

    err = bulk_buffer_change_state(buf, BULK_BUFFER_READ_WRITE);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not change the state of the buffer.");
    }

    if (lev->channel->callbacks->move_received) {
        lev->channel->callbacks->move_received(lev->channel, buf,
                                               lev->params.move_received.meta);
    }
}

/**
 * Implementation specific handler of the buffer move operation
 *
 * Side: Sending Side (SOURCE)
 */
static errval_t impl_move(struct bulk_channel *channel,
                          struct bulk_buffer *buffer,
                          void *meta,
                          struct bulk_continuation cont)
{
    errval_t err;
    struct local_event *ev, *ev2;

    struct local_channel *l = channel->impl_data;

    void *m;

    IMPL_DEBUG("  > buffer=%p", buffer->address);

    /* trigger event to other channel */
    size_t meta_size = 0;
    if (meta) {
        meta_size = channel->meta_size;
    }
    err = event_alloc(l->other, &ev, event_move_received, meta_size);
    if (!err_is_fail(err)) {
        ev->params.move_received.meta = NULL;
        if (meta) {
            /* copy the meta data */
            m = ev + 1;
            memcpy(m, meta, channel->meta_size);
            ev->params.move_received.meta = m;
        }
        /* set parameters of the event */
        ev->params.move_received.pool_id = buffer->pool->id;
        ev->params.move_received.buf_id = ((lvaddr_t) buffer->address
                        - buffer->pool->base_address)
                        / buffer->pool->buffer_size;
        ev->params.move_received.buffer = buffer;
        event_enqueue(ev);
    }

    /* trigger operation done event to this channel */
    err = event_alloc(channel, &ev2, event_op_done, 0);
    if (!err_is_fail(err)) {
        ev2->cont = cont;
        event_op_done(ev2);
    }

    return err;
}

/* -------------------------- event buffer pass ---------------------------- */

/**
 * Gets called when a buffer pass event occurs on the sending side
 *
 * Side: Sending Side (SOURCE)
 */
static void event_buffer_received(struct local_event *lev)
{
    errval_t err;

    struct bulk_pool *pool = bulk_pool_get(&lev->params.buffer_received.pool_id,
                                           lev->channel);
    assert(pool);

    size_t bufid = lev->params.buffer_received.buf_id;
    struct bulk_buffer *buf = pool->buffers[bufid];

    assert(bufid < pool->num_buffers);

    EVENT_DEBUG("  > buffer=[%p], bufid=0x%x", buf, (unsigned int )bufid);

    /* we need to change the state of the buffer */
    err = bulk_buffer_change_state(buf, BULK_BUFFER_READ_WRITE);
    if (err_is_fail(err)) {
        /* TODO: error handling */
        USER_PANIC_ERR(err, "could not change the state of the buffer.");
    }

    /* inform the application */
    if (lev->channel->callbacks->buffer_received) {
        lev->channel->callbacks->buffer_received(
                        lev->channel, buf, lev->params.buffer_received.meta);
    }
}

/**
 * Backend specific handler for buffer pass operations
 *
 * Side: Receiving Side (SINK)
 */
static errval_t impl_pass(struct bulk_channel *channel,
                          struct bulk_buffer *buffer,
                          void *meta,
                          struct bulk_continuation cont)
{
    errval_t err;
    struct local_event *ev, *ev2;
    void *m;
    struct local_channel *l = channel->impl_data;

    IMPL_DEBUG("  > buffer=%p", buffer->address);

    size_t meta_size = 0;
    if (meta) {
        meta_size = channel->meta_size;
    }
    /* allocate and trigger event */
    err = event_alloc(l->other, &ev, event_buffer_received, meta_size);
    if (!err_is_fail(err)) {
        ev->params.buffer_received.meta = NULL;

        if (meta) {
            /* copy meta data */
            m = ev + 1;
            memcpy(m, meta, channel->meta_size);
            ev->params.buffer_received.meta = m;

        }
        /* set event params */
        ev->params.buffer_received.pool_id = buffer->pool->id;
        ev->params.buffer_received.buf_id = ((lvaddr_t) buffer->address
                        - buffer->pool->base_address)
                        / buffer->pool->buffer_size;
        ev->params.buffer_received.buffer = buffer;
        event_enqueue(ev);
    }

    /* trigger op done event */
    err = event_alloc(channel, &ev2, event_op_done, 0);
    if (!err_is_fail(err)) {
        ev2->cont = cont;
        event_op_done(ev2);
    }
    return err;
}

/* -------------------------- event buffer copy ---------------------------- */

static void event_copy_received(struct local_event *lev)
{
    struct bulk_pool *pool = bulk_pool_get(&lev->params.copy_received.pool_id,
                                           lev->channel);
    size_t bufid = lev->params.copy_released.buf_id;
    EVENT_DEBUG("  > pool=%p, bufid=%x", pool, (unsigned int )bufid);
    assert(pool);

    struct bulk_buffer *buf = pool->buffers[bufid];

    errval_t err = bulk_buffer_change_state(buf, BULK_BUFFER_READ_ONLY);
    if (err_is_fail(err)) {
        /* TODO: error handling */
        USER_PANIC_ERR(err, "failed to change the state");
    }
    if (lev->channel->callbacks->copy_received) {
        lev->channel->callbacks->copy_received(lev->channel, buf,
                                               lev->params.copy_received.meta);

    }
}

static errval_t impl_copy(struct bulk_channel *channel,
                          struct bulk_buffer *buffer,
                          void *meta,
                          struct bulk_continuation cont)
{
    IMPL_DEBUG("  > buffer=%p", buffer->address);
    struct local_channel *l = channel->impl_data;
    struct local_event *ev, *ev2;
    void *m;
    errval_t err;
    size_t meta_size = 0;
    if (meta) {
        meta_size = channel->meta_size;
    }
    err = event_alloc(l->other, &ev, event_copy_received, meta_size);
    if (!err_is_fail(err)) {
        ev->params.copy_received.meta = NULL;
        if (meta) {
            m = ev + 1;
            memcpy(m, meta, channel->meta_size);
            ev->params.copy_received.meta = m;
        }
        ev->params.copy_received.buffer = buffer;
        ev->params.copy_received.pool_id = buffer->pool->id;
        ev->params.copy_received.buf_id = ((lvaddr_t) buffer->address
                        - buffer->pool->base_address)
                        / buffer->pool->buffer_size;
        event_enqueue(ev);
    }

    /* trigger op done event */
    err = event_alloc(channel, &ev2, event_op_done, 0);
    if (!err_is_fail(err)) {
        ev2->cont = cont;
        event_op_done(ev2);
    }
    return err;

}

/* -------------------------- event copy release --------------------------- */

/**
 * Gets called when a copy release event occurred
 *
 * Side: Sending Side (SOURCE)
 */
static void event_copy_released(struct local_event *lev)
{
    errval_t err;

    struct bulk_pool *pool = bulk_pool_get(&lev->params.copy_released.pool_id,
                                           lev->channel);
    assert(pool);

    size_t bufid = lev->params.copy_released.buf_id;
    struct bulk_buffer *buf = pool->buffers[bufid];

    buf->local_ref_count--;

    EVENT_DEBUG("  > buffer=[%p], bufid=0x%x", buf, (unsigned int )bufid);

    /* change the state of the buffer */
    if (buf->state == BULK_BUFFER_RO_OWNED && bulk_buffer_can_release(buf)) {
        err = bulk_buffer_change_state(buf, BULK_BUFFER_READ_WRITE);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to change the state");
        }
    }

    /* inform the application */
    if (lev->channel->callbacks->copy_released) {
        lev->channel->callbacks->copy_released(lev->channel, buf);
    }
}

/**
 *
 */
static errval_t impl_release(struct bulk_channel *channel,
                             struct bulk_buffer *buffer,
                             struct bulk_continuation cont)
{
    struct local_event *ev, *ev2;
    errval_t err;
    struct local_channel *l = channel->impl_data;

    IMPL_DEBUG("  > buffer=%p", buffer->address);

    /* allocate and trigger event */
    err = event_alloc(l->other, &ev, event_copy_released, 0);
    if (!err_is_fail(err)) {
        ev->params.copy_released.buffer = buffer;
        ev->params.copy_released.pool_id = buffer->pool->id;
        ev->params.copy_released.buf_id = ((lvaddr_t) buffer->address
                        - buffer->pool->base_address)
                        / buffer->pool->buffer_size;

        event_enqueue(ev);
    }

    /* trigger op done event */
    err = event_alloc(channel, &ev2, event_op_done, 0);
    if (!err_is_fail(err)) {
        ev2->cont = cont;
        event_op_done(ev2);
    }
    return err;
}

/* -------------------------- channel creation ----------------------------- */

/**
 * Implementation specific handler for channel creation
 */
static errval_t impl_create(struct bulk_channel *channel)
{
    return init_channel(channel);
}

/**
 * initializes the local endpoint
 */
void bulk_local_init_endpoint(struct bulk_local_endpoint *endpoint,
                              struct bulk_channel *other_channel)
{
    endpoint->generic.f = &implementation;
    endpoint->other_channel = other_channel;
}

