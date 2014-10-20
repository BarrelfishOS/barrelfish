/**
 * \file
 * \brief Network server thread of the bulk server
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_sm.h>

#include <if/block_service_defs.h>

#include "block_storage.h"
#include "block_server.h"
#include "network_client.h"
#include "local_server.h"

/* ---------------------------  Server State  ---------------------------- */
static uint8_t server_state = SERVICE_STATE_UNINITIALIZED;

static iref_t server_iref = 0;

static uint8_t is_client = 0;

static struct block_net_service *block_server;

static uint8_t is_bound = false;

static struct block_local_service local_server;

static void send_status_reply(struct block_service_binding *binding,
                              enum block_err_code error_code,
                              uint32_t seqn,
                              enum block_net_msg_type reqid);

#if BLOCK_ENABLE_NETWORKING
// condition used to singal controlling code to wait for a condition
static bool wait_cond;

static inline void wait_for_condition(volatile bool *cond)
{
    while (*cond) {
        messages_wait_and_handle_next();
    }
}

struct pool_assign_arg
{
    struct bulk_channel *chan_server;
    struct bulk_channel *chan_client;
    struct bulk_pool *pool;
    volatile bool wait_cond;
    errval_t err;
};

/* ------------------------  Bulk Transfer Callbacks ----------------------*/

static void pool_assigned_handler(void *arg,
                                  errval_t err,
                                  struct bulk_channel *chan)
{
    struct pool_assign_arg *pa = arg;
    pa->err = err;
    pa->wait_cond = false;
}

#endif

static errval_t bulk_pool_assigned_cb(struct bulk_channel *channel,
                                      struct bulk_pool *pool)
{

#if !BLOCK_ENABLE_NETWORKING
    return SYS_ERR_OK;
#else

    errval_t err = SYS_ERR_OK;

    debug_printf("Pool assign request received. \n");

    struct pool_assign_arg *arg = malloc(sizeof(struct pool_assign_arg));
    assert(arg);
    arg->chan_client = channel;
    arg->pool = pool;
    arg->wait_cond = 1;

    struct bulk_continuation cont = {
        .handler = pool_assigned_handler,
        .arg = arg, };

    /* forward the pool assignment */
    if (channel->direction == BULK_DIRECTION_RX) {
        arg->chan_server = &block_server->tx_chan;
        err = bulk_channel_assign_pool(&block_server->tx_chan, pool, cont);
    } else {
        arg->chan_server = &block_server->rx_chan;
        err = bulk_channel_assign_pool(&block_server->rx_chan, pool, cont);
    }

    if (err_is_fail(err)) {
        return err;
    }

    wait_cond = true;
    wait_for_condition(&arg->wait_cond);

    assert(pool == arg->pool);
    assert(arg->chan_client == channel);
    err = arg->err;
    free(arg);

    return err;
#endif
}

static errval_t bulk_pool_removed_cb(struct bulk_channel *channel,
                                     struct bulk_pool *pool)
{
    return SYS_ERR_OK;
}


static void bulk_move_received(struct bulk_channel *channel,
                               struct bulk_buffer *buffer,
                               void *meta)
{
    BS_LOCAL_DEBUG_TRACE

    errval_t err;

    struct bs_meta_data *bs = (struct bs_meta_data *) meta;

    if (is_client) {
        err = block_net_write(block_server, 1, &buffer, meta, BULK_CONT_NOP);
        if (err_is_fail(err)) {
            debug_printf("ERROR: block net write. %s", err_getstring(err));
        }
    } else {
        err = block_storage_write(bs->block_id, buffer->address);
        if (err_is_fail(err)) {
            BS_LOCAL_DEBUG("%s", "ERROR: block could not be written");
        }
        err = bulk_channel_pass(channel, buffer, meta,
        BULK_CONT_NOP);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to pass the buffer back");
        };
        send_status_reply(local_server.binding, err, bs->req_id,
                          BLOCK_NET_MSG_WRITE);
    }
}

static void bulk_copy_received(struct bulk_channel *channel,
                               struct bulk_buffer *buffer,
                               void *meta)
{
    BS_LOCAL_DEBUG_TRACE

    assert(!"Block server does not deal with copies for now..");

}

static void bulk_buffer_received(struct bulk_channel *channel,
                                 struct bulk_buffer *buffer,
                                 void *meta)
{
    BS_LOCAL_DEBUG_TRACE

    if (is_client) {
        /* forward buffer to the server */
        if (channel->direction == BULK_DIRECTION_RX) {
            bulk_channel_pass(&block_server->tx_chan, buffer, meta,
            BULK_CONT_NOP);
        } else {
            bulk_channel_pass(&block_server->rx_chan, buffer, meta,
            BULK_CONT_NOP);
        }

    } else {
        /* put it into the buffer store */
        block_server_insert_buffer(&bs_bulk_buffers, buffer, channel);
    }
}

static void bulk_copy_released(struct bulk_channel *channel,
                               struct bulk_buffer *buffer)
{
    if (is_client) {

    } else {
        block_server_insert_buffer(&bs_bulk_buffers, buffer, NULL);
    }
}

static void bulk_bind_done_cb(void *arg,
                              errval_t err,
                              struct bulk_channel *channel)
{
    BS_LOCAL_DEBUG_TRACE
    struct block_local_service *ls = (struct block_local_service *) arg;

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "callback: Binding failed.\n");
        return;
    }

    ls->bound++;
}

static struct bulk_channel_callbacks bulk_rx_cb = {
    .pool_assigned = bulk_pool_assigned_cb,
    .pool_removed = bulk_pool_removed_cb,
    .move_received = bulk_move_received,
    .copy_received = bulk_copy_received };

static struct bulk_channel_callbacks bulk_tx_cb = {
    .pool_assigned = bulk_pool_assigned_cb,
    .pool_removed = bulk_pool_removed_cb,
    .buffer_received = bulk_buffer_received,
    .copy_released = bulk_copy_released };

/* ------------------------ Sending Status Reply ---------------------------*/

struct bs_status_reply
{
    enum block_net_msg_type req;
    uint32_t seqn;
    enum block_net_err err;
    struct block_service_binding *binding;
};

static void status_reply_sent_cb(void *a)
{
    free(a);
}

static void status_reply_do_send(void *a)
{
    errval_t err;

    struct bs_status_reply *req = (struct bs_status_reply*) a;

    struct event_closure txcont = MKCONT(status_reply_sent_cb, req);
    err = block_service_status__tx(req->binding, txcont, req->err, req->seqn,
                                   req->req);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
            txcont = MKCONT(status_reply_do_send, req);
            err = req->binding->register_send(req->binding, ws, txcont);
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
            }

        }
    }
}

static void send_status_reply(struct block_service_binding *binding,
                              enum block_err_code error_code,
                              uint32_t seqn,
                              enum block_net_msg_type reqid)
{
    struct bs_status_reply *req = malloc(sizeof(struct bs_status_reply));

    req->seqn = seqn;
    req->binding = binding;
    req->err = error_code;
    req->req = reqid;

    status_reply_do_send(req);
}

/* -------------------------  Request Callbacks  ------------------------- */

/**
 * \brief callback for read requests on the flounder channel
 */
static void rx_read_request(struct block_service_binding *binding,
                            uint32_t start_block,
                            uint32_t count,
                            uint32_t seqn)
{
    BS_LOCAL_DEBUG_TRACE

    errval_t err;
    /*
     * if is server, then serve the request locally
     * else forward it to the network server
     */

    if (is_client) {
        err = block_net_read(block_server, start_block, count, seqn,
        BULK_CONT_NOP);
        if (err_is_fail(err)) {
            debug_printf("failed to issue read request to server.\n");
        }
        return;
    }

    // send_status_reply(binding, SYS_ERR_OK, seqn, BLOCK_NET_MSG_READ);

    /* we're the server, so we can serve it locally */

    struct block_local_service *ls = (struct block_local_service *) binding->st;
    struct bulk_channel        *chan = &ls->tx_chan;

    struct bs_meta_data *meta_data = malloc(
                    count * sizeof(struct bs_meta_data));
    assert(meta_data);

    for (uint32_t i = 0; i < count; ++i) {
        /* TODO: specify a pool */
        struct bulk_buffer *buf = block_server_get_buffer(&bs_bulk_buffers,
                                                          chan);
        if (!buf) {
            debug_printf("ERROR: Has no buffers left...\n");
            send_status_reply(binding, BLOCK_NET_MSG_READ, seqn,
                    BLOCK_NET_ERR_NO_BUFS);
            free(meta_data);
            return ;
        }

        err = block_storage_read(start_block + i, buf->address);
        if (err_is_fail(err)) {
            debug_printf("ERROR: block id is out of range: %i",
                         (uint32_t) (start_block + count));
            send_status_reply(binding, BLOCK_NET_MSG_READ, seqn,
                    BLOCK_NET_ERR_BLOCK_ID);
        }
        meta_data[i].block_id = start_block + i;
        meta_data[i].req_id = 0; // XXX not available. not used by bs_user.
        meta_data[i].cont = BULK_CONT_NOP;
        BS_LOCAL_DEBUG("bulk_channel_move: chan=%p, buf=%p\n", chan, buf);
        err = bulk_channel_move(chan, buf, meta_data + i, BULK_CONT_NOP);
        if (err_is_fail(err)) {
            send_status_reply(binding, BLOCK_NET_MSG_READ, seqn, err);
            debug_printf("channel move failed");
        }
    }

    /* XXX: assume that the meta data has been copied... */
    free(meta_data);
}

static void rx_setup_request(struct block_service_binding *binding,
                             iref_t tx_iref,
                             iref_t rx_iref)
{
    BS_LOCAL_DEBUG("tx_iref=%i, rx_iref=%i", (uint32_t )tx_iref,
                   (uint32_t )rx_iref);

    errval_t err;

    struct block_local_service *ls = (struct block_local_service *) binding->st;

    assert(ls);

    if (ls->bound != 0) {
        /* binding already in progress */
        debug_printf("already bound\n");
        return;
    }

    err = bulk_sm_ep_create_remote(&ls->rx_ep, rx_iref);
    assert(!err_is_fail(err));

    err = bulk_sm_ep_create_remote(&ls->tx_ep, tx_iref);
    assert(!err_is_fail(err));

    struct bulk_channel_bind_params params = {
        .role = BULK_ROLE_GENERIC,
        .waitset = get_default_waitset(),
        .user_state = ls,
        .trust = BULK_TRUST_FULL, };

    struct bulk_continuation cont = {
        .arg = ls,
        .handler = bulk_bind_done_cb, };

    err = bulk_channel_bind(&ls->rx_chan,
                            (struct bulk_endpoint_descriptor *) &ls->rx_ep,
                            &bulk_rx_cb, &params, cont);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "binding failed.");
    }

    err = bulk_channel_bind(&ls->tx_chan,
                            (struct bulk_endpoint_descriptor *) &ls->tx_ep,
                            &bulk_tx_cb, &params, cont);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "binding failed.");
    }
}

/* ---------------------  Connection Initialization  --------------------- */

/**
 * \brief accepts new connections to the local  via the flounder interface
 */
static errval_t block_local_accept_cb(void *st, struct block_service_binding *b)
{
    BS_LOCAL_DEBUG_TRACE

    debug_printf("> New connection on the flounder interface\n");

    // do the channel initialization
    b->rx_vtbl.read = rx_read_request;
    b->rx_vtbl.setup = rx_setup_request;

    assert(!is_bound);

    is_bound = true;

    b->st = &local_server;
    local_server.binding = b;

    return SYS_ERR_OK;
}

/* ----------------------  Channel Initialization  ----------------------- */

/**
 * \brief callback for interface export
 */
static void block_local_export_cb(void *st, errval_t err, iref_t iref)
{
    BS_LOCAL_DEBUG_TRACE
    if (err_is_fail(err)) {
        /* TODO: Error handling */
        server_state = SERVICE_STATE_FAILURE;
    }

    server_iref = iref;

    server_state = SERVICE_STATE_EXPORTED;
}

/**
 * \brief initializes the machine local block server
 */
errval_t block_local_init(struct block_net_service *server, uint32_t flags)
{
    BS_LOCAL_DEBUG_TRACE

    errval_t err;

    is_client = (flags & SERVICE_FLAG_CLIENT) && 1;

    block_server = server;

    // export the interface
    err = block_service_export(NULL, block_local_export_cb,
                               block_local_accept_cb, get_default_waitset(),
                               IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    while (server_state != SERVICE_STATE_EXPORTED) {
        if (server_state == SERVICE_STATE_FAILURE) {
            USER_PANIC("export resulted in a failure condition");
        }
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}

/* -------------------------  Server Management  ------------------------- */

/**
 * \brief starts the machine local server of block service to accept requests
 *
 * This function should not return until stopped.
 */
errval_t block_local_start(void)
{
    BS_LOCAL_DEBUG_TRACE

    if (server_state != SERVICE_STATE_EXPORTED) {
        assert(server_state == SERVICE_STATE_EXPORTED);
        /* TODO: return with error */
    }

    errval_t err = nameservice_register(BLOCK_SERVER_NAME, server_iref);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}
/**
 * \brief stops the request handling of the machine local block service requests
 */
errval_t block_local_stop(void)
{
    // set the stop flag.

    // tear down all bulk channels

    // stop the flounder service

    // free up resources

    assert(!"NYI: block_local_stop");
    return SYS_ERR_OK;
}

/* ------------------ callbacks for the network server --------------------- */

/**
 * forwards the received buffer to the requesting client
 */
void block_local_data_ready(struct bulk_buffer *buffer, void *meta)
{
    BS_LOCAL_DEBUG_TRACE

    errval_t err;

    err = bulk_channel_move(&local_server.tx_chan, buffer, meta, BULK_CONT_NOP);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not do the move");
    }
}
/**
 * forwards the buffer ot the originating client
 */
errval_t block_local_return_buffer(struct bulk_channel *chan,
                                   struct bulk_buffer *buffer,
                                   void *meta)
{
    BS_LOCAL_DEBUG_TRACE

    errval_t err;
    struct bulk_channel *local_chan;
    if (chan->direction == BULK_DIRECTION_RX) {
        local_chan = &local_server.tx_chan;
    } else {
        local_chan = &local_server.rx_chan;
    }
    err = bulk_channel_pass(local_chan, buffer, meta, BULK_CONT_NOP);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not pass the buffer");
    }

    return SYS_ERR_OK;
}

/**
 *
 */
errval_t block_local_release_copy(struct bulk_buffer *buffer)
{
    //struct bs_meta_data *data = (struct bs_meta_data *)meta;
    assert(!"NYI");

    return SYS_ERR_OK;
}

errval_t block_local_send_status(enum block_net_msg_type req,
                                 uint32_t reqid,
                                 enum block_net_err stats)
{
    BS_LOCAL_DEBUG_TRACE
    send_status_reply(local_server.binding, stats, reqid, req);
    return SYS_ERR_OK;
}
