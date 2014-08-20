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

#include <barrelfish/barrelfish.h>

#include <bulk_transfer/bulk_transfer.h>

#include "bulk_pool.h"
#include "bulk_buffer.h"

/**
 * Create a new channel.
 *
 * @param channel   Pointer to unused channel handle
 * @param ep_desc   Description of endpoint to bind to
 * @param callbacks Callbacks for events on this channel
 * @param setup     struct containing the setup information
 */
errval_t bulk_channel_create(struct bulk_channel *channel,
                             struct bulk_endpoint_descriptor *local_ep_desc,
                             struct bulk_channel_callbacks *callbacks,
                             struct bulk_channel_setup *setup)
{
    channel->state = BULK_STATE_UNINITIALIZED;
    /*
     * XXX: do we somehow track that this endpoint has not been assigned to
     *      a channel before?
     */
    channel->ep = local_ep_desc;
    channel->callbacks = callbacks;

    channel->direction = setup->direction;
    channel->role = setup->role;
    channel->trust = setup->trust;
    channel->constraints = setup->constraints;
    channel->meta_size = setup->meta_size;
    channel->waitset = setup->waitset;
    channel->user_state = setup->user_state;
    channel->pools = NULL;

    return local_ep_desc->f->channel_create(channel);
}

/**
 * Bind to an existing unbound channel.
 *
 * @param channel   Pointer to unused channel handle
 * @param ep_desc   Description of the remote endpoint to bind to
 * @param callbacks Callbacks for events on this channel
 * @param params    bind parameters
 */
errval_t bulk_channel_bind(struct bulk_channel *channel,
                           struct bulk_endpoint_descriptor *remote_ep_desc,
                           struct bulk_channel_callbacks *callbacks,
                           struct bulk_channel_bind_params *params,
                           struct bulk_continuation cont)
{
    if (channel->state != BULK_STATE_UNINITIALIZED) {
        return BULK_TRANSFER_CHAN_STATE;
    }

    channel->state = BULK_STATE_UNINITIALIZED;
    channel->ep = remote_ep_desc;
    channel->callbacks = callbacks;

    channel->role = params->role;
    channel->trust = params->trust;
    channel->constraints = params->constraints;
    channel->waitset = params->waitset;
    channel->user_state = params->user_state;
    channel->pools = NULL;

    return remote_ep_desc->f->channel_bind(channel, cont);
}

/**
 * Free a channel
 *
 * @param channel        Channel to be freed
 * @param free_resources Flag if the resources i.e. pools also should be freed
 */
errval_t bulk_channel_destroy(struct bulk_channel *channel,
                              struct bulk_continuation cont)
{
    assert(!"NYI: bulk_channel_destroy");
    switch (channel->state) {
        case BULK_STATE_UNINITIALIZED:
            return SYS_ERR_OK;
            break;
        case BULK_STATE_INITIALIZED:

            break;
        case BULK_STATE_BINDING:

            break;

        case BULK_STATE_CONNECTED:
            break;

        case BULK_STATE_TEARDOWN:
            break;

        case BULK_STATE_CLOSED:

            break;
        default:
            return BULK_TRANSFER_CHAN_STATE;
            break;
    }
    return SYS_ERR_OK;
}

/**
 * Assign a pool to a channel.
 *
 * @param channel Channel
 * @param pool    Pool to assign (must not be assigned to this channel yet)
 */
errval_t bulk_channel_assign_pool(struct bulk_channel *channel,
                                  struct bulk_pool *pool,
                                  struct bulk_continuation cont)
{
    assert(channel);
    assert(pool);

    if (channel->state != BULK_STATE_CONNECTED || !(channel->ep)) {
        return BULK_TRANSFER_CHAN_STATE;
    }

    if (pool->trust == BULK_TRUST_UNINITIALIZED) {
        /* this pool  has never been assigned to a channel */
        pool->trust = channel->trust;
    }

    /* a channel must not span trusted and non trusted channels */
    if (channel->trust != pool->trust) {
        return BULK_TRANSFER_CHAN_TRUST;
    }

    if (bulk_pool_is_assigned(pool, channel)) {
        debug_printf("bulk_channel_assign_pool: pool already assigned to channel\n");
        /*
         * XXX: do we treat this as a no-op or should we return an
         *      BULK_TRANSFER_POOL_ALREADY_ASSIGNED ?
         *      - RA
         */
        return SYS_ERR_OK;
    }

    /*
     * Note, the pool can only be added to the list of pools, once the other
     * side has acked the assignment request.
     */

    return channel->ep->f->assign_pool(channel, pool, cont);
}

/**
 * Remove a pool from a channel
 *
 * @param channel Channel
 * @param pool    Pool to remove (must be previously assigned to the channel)
 *
 */
errval_t bulk_channel_remove_pool(struct bulk_channel *channel,
                                  struct bulk_pool *pool,
                                  struct bulk_continuation cont)
{
    assert(!"NYI: removing a pool from a channel");

    assert(channel);
    assert(pool);

    if (channel->state != BULK_STATE_CONNECTED) {
        return BULK_TRANSFER_CHAN_STATE;
    }

    if (!bulk_pool_is_assigned(pool, channel)) {
        /*
         * XXX: if there is no such pool on this channel simply return
         *      or do we want to indicate an error here?
         *      BULK_TRANSFER_POOL_NOT_ASSIGNED
         *      - RA
         */
        return SYS_ERR_OK;
    }

    struct bulk_pool_list *list = channel->pools;
    struct bulk_pool_list *prev = NULL;
    while (list) {
        if (bulk_pool_cmp_id(&list->pool->id, &pool->id) == 0) {
            break;
        }
        prev = list;
        list = list->next;
    }
    if (prev == NULL) {
        channel->pools = list->next;
    } else {
        prev->next = list->next;
    }

    free(list);

    /*
     * TODO: we may want to track the channels which this pool was used,
     *       so if the last reference is removed, we can unmap the pool
     */

    return channel->ep->f->remove_pool(channel, pool, cont);
}

errval_t bulk_channel_move(struct bulk_channel *channel,
                           struct bulk_buffer *buffer,
                           void *meta,
                           struct bulk_continuation cont)
{
    assert(channel);
    assert(buffer);

    errval_t err;

    if (channel->state != BULK_STATE_CONNECTED) {
        return BULK_TRANSFER_CHAN_STATE;
    }

    if (channel->direction != BULK_DIRECTION_TX) {
        return BULK_TRANSFER_CHAN_DIRECTION;
    }

    if (!bulk_pool_is_assigned(buffer->pool, channel)) {
        return BULK_TRANSFER_POOL_NOT_ASSIGNED;
    }

    if (!bulk_buffer_is_owner(buffer)) {
        return BULK_TRANSFER_BUFFER_NOT_OWNED;
    }

    err = bulk_buffer_change_state(buffer, BULK_BUFFER_INVALID);
    if (err_is_fail(err)) {
        /*
         * XXX: what do we do if the unmap fails?
         */
        USER_PANIC_ERR(err, "failed to change the buffer state");
    }

    return channel->ep->f->move(channel, buffer, meta, cont);
}

/**
 *
 */
errval_t bulk_channel_pass(struct bulk_channel *channel,
                           struct bulk_buffer *buffer,
                           void *meta,
                           struct bulk_continuation cont)
{
    assert(channel);
    assert(buffer);

    errval_t err;

    if (channel->state != BULK_STATE_CONNECTED) {
        return BULK_TRANSFER_CHAN_STATE;
    }

    if (!bulk_pool_is_assigned(buffer->pool, channel)) {
        return BULK_TRANSFER_POOL_NOT_ASSIGNED;
    }

    if (!bulk_buffer_is_owner(buffer)) {
        return BULK_TRANSFER_BUFFER_NOT_OWNED;
    }

    err = bulk_buffer_change_state(buffer, BULK_BUFFER_INVALID);
    if (err_is_fail(err)) {
        /*
         * XXX: what do we do if the unmap fails?
         */
        USER_PANIC_ERR(err, "failed to change the buffer state");
    }

    return channel->ep->f->pass(channel, buffer, meta, cont);
}

/**
 *
 */
errval_t bulk_channel_copy(struct bulk_channel *channel,
                           struct bulk_buffer *buffer,
                           void *meta,
                           struct bulk_continuation cont)
{
    assert(channel);
    assert(buffer);

    errval_t err;

    if (channel->state != BULK_STATE_CONNECTED) {
        return BULK_TRANSFER_CHAN_STATE;
    }

    if (channel->direction != BULK_DIRECTION_TX) {
        return BULK_TRANSFER_CHAN_DIRECTION;
    }

    if (!bulk_pool_is_assigned(buffer->pool, channel)) {
        return BULK_TRANSFER_POOL_NOT_ASSIGNED;
    }

    if (!bulk_buffer_is_valid(buffer)) {
        return BULK_TRANSFER_BUFFER_INVALID;
    }

    enum bulk_buffer_state new_state = BULK_BUFFER_READ_ONLY;
    if (bulk_buffer_is_owner(buffer)) {
        new_state = BULK_BUFFER_RO_OWNED;
    }

    buffer->local_ref_count++;

    err = bulk_buffer_change_state(buffer, new_state);
    if (err_is_fail(err)) {
        return BULK_TRANSFER_BUFFER_STATE;
    }

    return channel->ep->f->copy(channel, buffer, meta, cont);
}

/**
 *
 */
errval_t bulk_channel_release(struct bulk_channel *channel,
                              struct bulk_buffer *buffer,
                              struct bulk_continuation cont)
{
    assert(channel);
    assert(buffer);

    errval_t err;

    if (channel->state != BULK_STATE_CONNECTED) {
        return BULK_TRANSFER_CHAN_STATE;
    }

    if (!bulk_buffer_is_copy(buffer)) {
        return BULK_TRANSFER_BUFFER_NOT_A_COPY;
    }

    if (!bulk_buffer_is_owner(buffer) && !bulk_buffer_can_release(buffer)) {
        return BULK_TRANSFER_BUFFER_REFCOUNT;
    }

    err = bulk_buffer_change_state(buffer, BULK_BUFFER_INVALID);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to change the buffer state");
    }

    return channel->ep->f->release(channel, buffer, cont);
}

