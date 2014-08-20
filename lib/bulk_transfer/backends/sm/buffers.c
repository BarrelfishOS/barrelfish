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
#include "../../bulk_pool.h"
#include "../../bulk_buffer.h"
#include "bulk_sm_impl.h"
#include "pending_msg.h"

#if 0
#define BULK_DEBUG_PRINT(fmt, msg...) debug_printf(fmt, msg)
#else
#define BULK_DEBUG_PRINT(fmt, msg...)
#endif

//the same values are necessary for move, pass and copy operations
struct pass_data {
    uint32_t tid;
    void *meta;
    size_t metasize;
    struct capref cap;
    struct bulk_buffer *buffer;
    struct bulk_channel *channel;
    bulk_ctrl_poolid_t poolid;
};

static errval_t bulk_sm_move_send_request(void *a)
{
    struct pass_data         *d       = (struct pass_data*) a;
    struct bulk_buffer       *buffer  = d->buffer;
    struct bulk_channel      *channel = d->channel;
    struct bulk_ctrl_binding *b       = CHANNEL_BINDING(channel);

    struct event_closure txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
            "bulk_sm_move sent");

    errval_t err;
    if (channel->trust == BULK_TRUST_NONE) {
        err = bulk_ctrl_move_untrusted_call__tx(b, txcont, d->poolid, buffer->bufferid,
                                                d->tid, d->cap, d->meta, d->metasize);
    } else {
        err = bulk_ctrl_move_trusted_call__tx(b, txcont, d->poolid, buffer->bufferid,
                                              d->tid, d->meta, d->metasize);
    }

    if (err_is_ok(err)) {
        free(d);
    } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
        //sending this message will never work, do not retry
        //notify user the same way as if the other side had an error
        bulk_sm_move_rx_response(b, err, d->tid);
        free(d);
    }
    return err;
}

errval_t bulk_sm_move(struct bulk_channel      *channel,
                      struct bulk_buffer       *buffer,
                      void                     *meta,
                      struct bulk_continuation cont)
{
    errval_t err;
    uint32_t tid;
    size_t   metasize = (meta) ? channel->meta_size : 0;//tolerate null pointers

    //store the arguments for when we get the reply later
    union pending_msg_data pmsg = {
        .move.continuation = cont,
        };
    err = pending_msg_add(channel, &tid, pmsg);
    assert(err_is_ok(err));//adding should actually never fail

    //if fully trusted, the other side already has the cap, so don't resend it
    struct capref cap;
    if (channel->trust == BULK_TRUST_FULL){
        cap = NULL_CAP;
    } else {
        cap = buffer->cap;
    }

    //send message
    struct pass_data *d = malloc(sizeof(*d));
    assert(d);
    d->tid = tid;
    d->meta = meta;
    d->metasize = metasize;
    d->cap = cap;
    d->buffer = buffer;
    d->channel = channel;
    fill_pool_id_for_flounder(&buffer->pool->id, &d->poolid);

    //send message (automatically retries if channel busy)
    bulk_sm_flounder_send_fifo_msg_with_arg(channel,bulk_sm_move_send_request,d);
    //if the transmission fails, the user will be notified through the continuation
    return err;
}

static errval_t bulk_sm_copy_send_request(void *a)
{
    struct pass_data         *d       = (struct pass_data*) a;
    struct bulk_buffer       *buffer  = d->buffer;
    struct bulk_channel      *channel = d->channel;
    struct bulk_ctrl_binding *b       = CHANNEL_BINDING(channel);

    struct event_closure txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
            "bulk_sm_copy sent");
    errval_t err;
    if (channel->trust == BULK_TRUST_NONE) {
        err = bulk_ctrl_copy_untrusted_call__tx(b, txcont, d->poolid, buffer->bufferid,
                                                d->tid, d->cap, d->meta, d->metasize);
    } else {
        err = bulk_ctrl_copy_trusted_call__tx(b, txcont, d->poolid, buffer->bufferid,
                                              d->tid, d->meta, d->metasize);
    }

    if (err_is_ok(err)) {
        free(d);
    } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
        //sending this message will never work, do not retry
        //notify user the same way as if the other side had an error
        bulk_sm_copy_rx_response(b, err, d->tid);
        free(d);
    }
    return err;
}

errval_t bulk_sm_copy(struct bulk_channel      *channel,
                      struct bulk_buffer       *buffer,
                      void                     *meta,
                      struct bulk_continuation cont)
{
    errval_t err;
    uint32_t tid;
    size_t   metasize = (meta) ? channel->meta_size : 0;//tolerate null pointers

    //store the arguments for when we get the reply later
    union pending_msg_data pmsg = {
        .copy.continuation = cont,
        };
    err = pending_msg_add(channel, &tid, pmsg);
    assert(err_is_ok(err));//adding should actually never fail

    //if fully trusted, the other side already has the cap, so don't resend it
    struct capref cap;
    if (channel->trust == BULK_TRUST_FULL){
        cap = NULL_CAP;
    } else {
        cap = buffer->cap;
    }

    //send message
    struct pass_data *d = malloc(sizeof(*d));
    assert(d);
    d->tid = tid;
    d->meta = meta;
    d->metasize = metasize;
    d->cap = cap;
    d->buffer = buffer;
    d->channel = channel;
    fill_pool_id_for_flounder(&buffer->pool->id, &d->poolid);

    //send message (automatically retries if channel busy)
    bulk_sm_flounder_send_fifo_msg_with_arg(channel,bulk_sm_copy_send_request,d);
    //if the transmission fails, the user will be notified through the continuation
    return err;
}

static errval_t bulk_sm_release_send_request(void *a)
{
    struct pass_data         *d       = (struct pass_data*) a;
    struct bulk_buffer       *buffer  = d->buffer;
    struct bulk_channel      *channel = d->channel;
    struct bulk_ctrl_binding *b       = CHANNEL_BINDING(channel);

    struct event_closure txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
            "bulk_sm_release sent");
    errval_t err = bulk_ctrl_release_call__tx(b, txcont,
            d->poolid, buffer->bufferid, d->tid);

    if (err_is_ok(err)) {
        free(d);
    } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
        //sending this message will never work, do not retry
        //notify user the same way as if the other side had an error
        bulk_sm_release_rx_response(b, err, d->tid);
        free(d);
    }
    return err;
}


errval_t bulk_sm_release(struct bulk_channel      *channel,
                         struct bulk_buffer       *buffer,
                         struct bulk_continuation cont)
{
    errval_t err;
    uint32_t tid;

    //store the arguments for when we get the reply later
    union pending_msg_data pmsg = {
        .release.continuation = cont,
        };
    err = pending_msg_add(channel, &tid, pmsg);
    assert(err_is_ok(err));//adding should actually never fail


    //send message
    struct pass_data *d = malloc(sizeof(*d));//could use smaller struct
    assert(d);
    d->tid = tid;
    d->buffer = buffer;
    d->channel = channel;
    fill_pool_id_for_flounder(&buffer->pool->id, &d->poolid);

    //send message (automatically retries if channel busy)
    bulk_sm_flounder_send_fifo_msg_with_arg(channel,bulk_sm_release_send_request,d);
    //if the transmission fails, the user will be notified through the continuation
    return err;
}

static errval_t bulk_sm_pass_send_request(void *a)
{
    struct pass_data         *d       = (struct pass_data*) a;
    struct bulk_buffer       *buffer  = d->buffer;
    struct bulk_channel      *channel = d->channel;
    struct bulk_ctrl_binding *b       = CHANNEL_BINDING(channel);

    struct event_closure txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
            "bulk_sm_pass sent");

    errval_t err;
    if (channel->trust == BULK_TRUST_NONE) {
        err = bulk_ctrl_pass_untrusted_call__tx(b, txcont, d->poolid, buffer->bufferid,
                                                d->tid, d->cap, d->meta, d->metasize);
    } else {
        err = bulk_ctrl_pass_trusted_call__tx(b, txcont, d->poolid, buffer->bufferid,
                                              d->tid, d->meta, d->metasize);
    }

    if (err_is_ok(err)) {
        free(d);
    } else if (err_no(err) != FLOUNDER_ERR_TX_BUSY) {
        //sending this message will never work, do not retry
        //notify user the same way as if the other side had an error
        bulk_sm_pass_rx_response(b, err, d->tid);
        free(d);
    }
    return err;
}

errval_t bulk_sm_pass(struct bulk_channel      *channel,
                      struct bulk_buffer       *buffer,
                      void                     *meta,
                      struct bulk_continuation cont)
{
    errval_t err;
    uint32_t tid;
    size_t   metasize = (meta) ? channel->meta_size : 0;//tolerate null pointers

    //store the arguments for when we get the reply later
    union pending_msg_data pmsg = {
        .pass.continuation = cont,
        };
    err = pending_msg_add(channel, &tid, pmsg);
    assert(err_is_ok(err));//adding should actually never fail

    //if fully trusted, the other side already has the cap, so don't resend it
    struct capref cap;
    if (channel->trust == BULK_TRUST_FULL){
        cap = NULL_CAP;
    } else {
        cap = buffer->cap;
    }


    struct pass_data *d = malloc(sizeof(*d));
    assert(d);
    d->tid = tid;
    d->meta = meta;
    d->metasize = metasize;
    d->cap = cap;
    d->buffer = buffer;
    d->channel = channel;
    fill_pool_id_for_flounder(&buffer->pool->id, &d->poolid);

    //send message (automatically retries if channel busy)
    bulk_sm_flounder_send_fifo_msg_with_arg(channel,bulk_sm_pass_send_request, d);
    //if the transmission fails, the user will be notified through the continuation
    return err;
}


//--------------- flounder RPC handlers:

//move, copy, pass and release replies all have the same format
struct bulk_sm_reply_data {
    struct bulk_channel      *channel;
    struct event_closure     cb;
    bulk_ctrl_error_t        error;
    uint32_t                 tid;
};

static errval_t bulk_sm_move_send_reply(void *a)
{
    struct bulk_sm_reply_data   *rdata = a;
    struct bulk_ctrl_binding    *b     = CHANNEL_BINDING(rdata->channel);

    errval_t err = bulk_ctrl_move_response__tx(b, rdata->cb,
                                                    rdata->error, rdata->tid);

    if (err_is_ok(err)) {
        free(rdata);
    }
    return err;
}


void bulk_sm_move_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        struct capref            cap,
        uint8_t                  *meta,
        size_t                   metasize)
{
    errval_t err = SYS_ERR_OK;
    struct event_closure txcont;
    struct bulk_buffer *buffer;
    struct bulk_channel *channel = VOID2CHANNEL(b->st);

    assert(metasize == channel->meta_size || metasize == 0);

    struct bulk_pool_id b_poolid;
    fill_pool_id_from_flounder(&b_poolid, &poolid);

    struct bulk_pool *pool = bulk_pool_get(&b_poolid, channel);
    if (pool == NULL){
        err = BULK_TRANSFER_POOL_INVALD;
    } else if (pool->num_buffers < bufferid){
        err = BULK_TRANSFER_BUFFER_INVALID;
    } else {
        buffer = pool->buffers[bufferid];

        //in the untrusted case, we also received the cap for this buffer
        if (channel->trust == BULK_TRUST_NONE){
            //make sure transmitter does not keep a copy for himself
            err = cap_revoke(cap);
            assert(err_is_ok(err));
            err = bulk_buffer_assign_cap(buffer, cap, 0);
        }

        if (err_is_ok(err)){
            //automatically remaps if necessary
            err = bulk_buffer_change_state(buffer, BULK_BUFFER_READ_WRITE);
        }
    }

    //send reply & inform user
    if (err_is_ok(err)){
        if (channel->callbacks->move_received) {
            channel->callbacks->move_received(channel, buffer, meta);
        }

        txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
                "bulk_sm_move_rx_call: reply sent.");
    } else {
        txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
                "bulk_sm_move_rx_call: reply to invalid move sent");
    }

    struct bulk_sm_reply_data *rdata = malloc(sizeof(*rdata));
    assert(rdata);
    rdata->channel = channel;
    rdata->cb      = txcont;
    rdata->error   = err;
    rdata->tid     = tid;

    bulk_sm_flounder_send_fifo_msg_with_arg(channel,
                                            bulk_sm_move_send_reply, rdata);
}

void bulk_sm_move_trusted_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        uint8_t                  *meta,
        size_t                   metasize)
{
    //call normal handler with a NULL_CAP
    bulk_sm_move_rx_call(b, poolid, bufferid, tid, NULL_CAP, meta, metasize);
}


void bulk_sm_move_rx_response(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_error_t        error,
        uint32_t                 tid)
{
    //find data associated with this RPC call
    union pending_msg_data data;
    errval_t err = pending_msg_get(VOID2CHANNEL(b->st), tid, &data, true);
    if (err_is_fail(err)){
        //no such message data -> ignore?
        DEBUG_ERR(err, "bulk_sm_move_rx_response");
        return;
    }

    //TODO: clean up if error is fail

    //call continuation
    bulk_continuation_call(data.move.continuation, (errval_t) error,
            VOID2CHANNEL(b->st));
}

static errval_t bulk_sm_copy_send_reply(void *a)
{
    struct bulk_sm_reply_data   *rdata = a;
    struct bulk_ctrl_binding    *b     = CHANNEL_BINDING(rdata->channel);

    errval_t err = bulk_ctrl_copy_response__tx(
                        b, rdata->cb, rdata->error, rdata->tid);

    if (err_is_ok(err)) {
        free(rdata);
    }
    return err;
}

void bulk_sm_copy_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        struct capref            cap,
        uint8_t                  *meta,
        size_t                   metasize)
{
    errval_t err = SYS_ERR_OK;
    struct event_closure txcont;
    struct bulk_buffer *buffer;
    struct bulk_channel *channel = VOID2CHANNEL(b->st);

    assert(metasize == channel->meta_size || metasize == 0);

    struct bulk_pool_id b_poolid;
    fill_pool_id_from_flounder(&b_poolid, &poolid);

    struct bulk_pool *pool = bulk_pool_get(&b_poolid, channel);
    if (pool == NULL){
        err = BULK_TRANSFER_POOL_INVALD;
    } else if (pool->num_buffers < bufferid){
        err = BULK_TRANSFER_BUFFER_INVALID;
    } else {
        buffer = pool->buffers[bufferid];
        //in the untrusted case, we also received the cap for this buffer
        if (channel->trust == BULK_TRUST_NONE){
            //TODO: make sure there is no rw cap in transmitters cspace
            //      the way to do this would be to check that this is a shared_readonly cap
            err = bulk_buffer_assign_cap(buffer, cap, 0);
        }

        if (err_is_ok(err)){
            //automatically remaps if necessary
            err = bulk_buffer_change_state(buffer, BULK_BUFFER_READ_ONLY);
            //TODO: keep track of copies? adjust refcount? do we let the user do that?
        }
    }

    if (err_is_ok(err)){
        if (channel->callbacks->copy_received) {
            channel->callbacks->copy_received(channel, buffer, meta);
        }

        txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
                "bulk_sm_copy_rx_call: reply sent.");
    } else {
        txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
                "bulk_sm_copy_rx_call: reply to invalid copy sent");
    }

    struct bulk_sm_reply_data *rdata = malloc(sizeof(*rdata));
    assert(rdata);
    rdata->channel = channel;
    rdata->cb      = txcont;
    rdata->error   = err;
    rdata->tid     = tid;


    bulk_sm_flounder_send_fifo_msg_with_arg(channel,
                                            bulk_sm_copy_send_reply, rdata);
}

void bulk_sm_copy_trusted_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        uint8_t                  *meta,
        size_t                   metasize)
{
    //call normal handler with a NULL_CAP
    bulk_sm_copy_rx_call(b, poolid, bufferid, tid, NULL_CAP, meta, metasize);
}


void bulk_sm_copy_rx_response(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_error_t        error,
        uint32_t                 tid)
{
    //find data associated with this RPC call
    union pending_msg_data data;
    errval_t err = pending_msg_get(VOID2CHANNEL(b->st), tid, &data, true);
    if (err_is_fail(err)){
        //no such message data -> ignore?
        DEBUG_ERR(err, "bulk_sm_copy_rx_response");
        return;
    }

    //TODO: clean up if error is fail

    //call continuation
    bulk_continuation_call(data.copy.continuation, (errval_t) error,
            VOID2CHANNEL(b->st));
}

static errval_t bulk_sm_pass_send_reply(void *a)
{
    struct bulk_sm_reply_data   *rdata = a;
    struct bulk_ctrl_binding    *b     = CHANNEL_BINDING(rdata->channel);

    errval_t err = bulk_ctrl_pass_response__tx(b, rdata->cb,
                                               rdata->error, rdata->tid);

    if (err_is_ok(err)) {
        free(rdata);
    }
    return err;
}


void bulk_sm_pass_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        struct capref            cap,
        uint8_t                  *meta,
        size_t                   metasize)
{
    BULK_DEBUG_PRINT("%s", "bulk_sm_pass_rx_call called\n");

    errval_t err = SYS_ERR_OK;
    struct event_closure txcont;
    struct bulk_buffer *buffer;
    struct bulk_channel *channel = VOID2CHANNEL(b->st);

    assert(metasize == channel->meta_size || metasize == 0);

    struct bulk_pool_id b_poolid;
    fill_pool_id_from_flounder(&b_poolid, &poolid);

    struct bulk_pool *pool = bulk_pool_get(&b_poolid, channel);
    if (pool == NULL){
        err = BULK_TRANSFER_POOL_INVALD;
    } else if (pool->num_buffers < bufferid){
        err = BULK_TRANSFER_BUFFER_INVALID;
    } else {
        buffer = pool->buffers[bufferid];

        //in the untrusted case, we also received the cap for this buffer
        if (channel->trust == BULK_TRUST_NONE){
            //make sure transmitter does not keep a copy for himself
            err = cap_revoke(cap);
            assert(err_is_ok(err));
            err = bulk_buffer_assign_cap(buffer, cap, 0);
        }

        if (err_is_ok(err)){
            //automatically remaps if necessary
            err = bulk_buffer_change_state(buffer, BULK_BUFFER_READ_WRITE);
        }
    }

    //send reply & inform user
    if (err_is_ok(err)){
        if (channel->callbacks->buffer_received) {
            channel->callbacks->buffer_received(channel, buffer, meta);
        }

        txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
                "bulk_sm_pass_rx_call: reply sent.");
    } else {
        txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
                "bulk_sm_pass_rx_call: reply to invalid pass sent");
    }

    struct bulk_sm_reply_data *rdata = malloc(sizeof(*rdata));
    assert(rdata);
    rdata->channel = channel;
    rdata->cb      = txcont;
    rdata->error   = err;
    rdata->tid     = tid;

    bulk_sm_flounder_send_fifo_msg_with_arg(channel,
                                            bulk_sm_pass_send_reply, rdata);
}

void bulk_sm_pass_trusted_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        uint8_t                  *meta,
        size_t                   metasize)
{
    //call normal handler with a NULL_CAP
    bulk_sm_pass_rx_call(b, poolid, bufferid, tid, NULL_CAP, meta, metasize);
}


void bulk_sm_pass_rx_response(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_error_t        error,
        uint32_t                 tid)
{
    BULK_DEBUG_PRINT("bulk_sm_pass_rx_response called. TID = %u\n", tid);
    //find data associated with this RPC call
    union pending_msg_data data;
    errval_t err = pending_msg_get(VOID2CHANNEL(b->st), tid, &data, true);
    if (err_is_fail(err)){
        //no such message data -> ignore?
        DEBUG_ERR(err, "bulk_sm_copy_rx_response");
        return;
    }

    //TODO: clean up if error is fail

    //call continuation
    bulk_continuation_call(data.pass.continuation, (errval_t) error,
            VOID2CHANNEL(b->st));
}

static errval_t bulk_sm_release_send_reply(void *a)
{
    struct bulk_sm_reply_data   *rdata = a;
    struct bulk_ctrl_binding    *b     = CHANNEL_BINDING(rdata->channel);

    errval_t err = bulk_ctrl_release_response__tx(b, rdata->cb,
                                                    rdata->error, rdata->tid);

    if (err_is_fail(err)) {
        free(rdata);
    }
    return err;
}


void bulk_sm_release_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid)
{
    errval_t err = SYS_ERR_OK;
    struct event_closure txcont;
    struct bulk_buffer *buffer;
    struct bulk_channel *channel = VOID2CHANNEL(b->st);

    struct bulk_pool_id b_poolid;
    fill_pool_id_from_flounder(&b_poolid, &poolid);

    struct bulk_pool *pool = bulk_pool_get(&b_poolid, channel);
    if (pool == NULL){
        err = BULK_TRANSFER_POOL_INVALD;
    } else if (pool->num_buffers < bufferid){
        err = BULK_TRANSFER_BUFFER_INVALID;
    } else {
        buffer = pool->buffers[bufferid];
        buffer->local_ref_count--;
        //TODO: make the decrease atomic? (so only the last decrease reclaims it)
        //TODO: find out what the refcount should be to take action (0 or 1?)
        if (buffer->local_ref_count == 0 && buffer->state == BULK_BUFFER_RO_OWNED){
            //retake ownership
            if (channel->trust == BULK_TRUST_NONE){
                err = cap_revoke(buffer->cap);
                assert(err_is_ok(err));
            }
            if (err_is_ok(err)){
                //automatically remaps if necessary
                err = bulk_buffer_change_state(buffer, BULK_BUFFER_READ_WRITE);
            }
        }
        //TODO: what if refcount became 0 but we are not the owner?
        //should we just let the user callback handle that? (we probably have to)
    }

    //send reply & inform user
    if (err_is_ok(err)){
        if (channel->callbacks->copy_released) {
            channel->callbacks->copy_released(channel, buffer);
        }

        txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
                "bulk_sm_release_rx_call: reply sent.");
    } else {
        txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
                "bulk_sm_release_rx_call: reply to invalid release sent");
    }

    struct bulk_sm_reply_data *rdata = malloc(sizeof(*rdata));
    assert(rdata);
    rdata->channel = channel;
    rdata->cb      = txcont;
    rdata->error   = err;
    rdata->tid     = tid;

    bulk_sm_flounder_send_fifo_msg_with_arg(channel,
                                            bulk_sm_release_send_reply, rdata);
}


void bulk_sm_release_rx_response(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_error_t        error,
        uint32_t                 tid)
{
    //find data associated with this RPC call
    union pending_msg_data data;
    errval_t err = pending_msg_get(VOID2CHANNEL(b->st), tid, &data, true);
    if (err_is_fail(err)){
        //no such message data -> ignore?
        DEBUG_ERR(err, "bulk_sm_release_rx_response");
        return;
    }

    //TODO: clean up if error is fail

    //call continuation
    bulk_continuation_call(data.release.continuation, (errval_t) error,
            VOID2CHANNEL(b->st));
}
