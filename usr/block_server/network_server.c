/**
 * \file
 * \brief Network server thread of the bulk server
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_net.h>

#include <lwip/tcp.h>
#include <lwip/init.h>

#include "block_server.h"
#include "network_common.h"
#include "network_server.h"
#include "block_storage.h"

#if BULK_NET_BACKEND_PROXY
#include <bulk_transfer/bulk_allocator.h>
#include <bulk_transfer/bulk_net_proxy.h>
#include <bulk_transfer/bulk_local.h>

static struct bulk_allocator allocator_tx;
static struct bulk_allocator allocator_rx;

#endif

static errval_t bulk_pool_assigned_cb(struct bulk_channel *channel,
                                      struct bulk_pool *pool);
static errval_t bulk_pool_removed_cb(struct bulk_channel *channel,
                                     struct bulk_pool *pool);
static void bulk_move_received(struct bulk_channel *channel,
                               struct bulk_buffer *buffer,
                               void *meta);
static void bulk_buffer_received(struct bulk_channel *channel,
                                 struct bulk_buffer *buffer,
                                 void *meta);
static void bulk_copy_received(struct bulk_channel *channel,
                               struct bulk_buffer *buffer,
                               void *meta);
static void bulk_copy_released(struct bulk_channel *channel,
                               struct bulk_buffer *buffer);
static errval_t bulk_bind_received(struct bulk_channel *channel);

static struct bulk_channel_callbacks bulk_rx_cb = {
    .bind_received = bulk_bind_received,
    .pool_assigned = bulk_pool_assigned_cb,
    .pool_removed = bulk_pool_removed_cb,
    .move_received = bulk_move_received,
    .copy_received = bulk_copy_received };

static struct bulk_channel_callbacks bulk_tx_cb = {
    .bind_received = bulk_bind_received,
    .pool_assigned = bulk_pool_assigned_cb,
    .pool_removed = bulk_pool_removed_cb,
    .buffer_received = bulk_buffer_received,
    .copy_released = bulk_copy_released };

static void block_send_status_msg(struct block_net_service *c,
                                  enum block_net_msg_type req,
                                  uint32_t reqid,
                                  enum block_net_err stats)
{

    debug_printf("Sending status message\n");

    uint16_t size = sizeof(struct block_net_msg);
    struct block_net_msg *msg = malloc(size);
    assert(msg);

    msg->size = size;
    msg->type = BLOCK_NET_MSG_STATUS;
    msg->msg.status.code = stats;
    msg->msg.status.req = req;
    msg->msg.status.reqid = reqid;
    err_t err;

    err = tcp_write(c->tpcb, msg, size, TCP_WRITE_FLAG_COPY);
    if (err != ERR_OK) {
        /* TODO error handling */
        debug_printf("ERROR: tcp_write returned with error %i\n", err);
    }

    err = tcp_output(c->tpcb);
    if (err != ERR_OK) {
        /* TODO: Error handling */
        debug_printf("ERROR: tcp_output returned with error %i\n", err);
    }

    /* XXX: assume that the data has already been copied */
    free(msg);
}

#if !BULK_NET_BACKEND_PROXY
/**
 *
 */
static void chan_bind_cb(void *arg, errval_t err, struct bulk_channel *channel)
{
    BS_NET_DEBUG_BULK("chan=%p, sucecss=%i", channel, (err == SYS_ERR_OK));

    struct block_net_service *c = (struct block_net_service *) arg;

    if (err_is_fail(err)) {
        /* there is some thing wrong */

    }
    c->bound++;

    if (c->bound != 2) {
        return;
    }
    /* both channels are bound we can send a reply back */
    block_send_status_msg(c, BLOCK_NET_MSG_INIT, 0, SYS_ERR_OK);
}
#endif

static errval_t bulk_pool_assigned_cb(struct bulk_channel *channel,
                                      struct bulk_pool *pool)
{
    BS_NET_DEBUG_BULK("chan=%p, pool=%p", channel, pool);

    /* buffers must hold a whole block a once */
    if (pool->buffer_size < block_storage_get_block_size()) {
        return BULK_TRANSFER_ALLOC_BUFFER_SIZE;
    }

    return SYS_ERR_OK;
}

static errval_t bulk_pool_removed_cb(struct bulk_channel *channel,
                                     struct bulk_pool *pool)
{
    BS_NET_DEBUG_BULK("chan=%p, pool=%p", channel, pool);
    return SYS_ERR_OK;
}

static errval_t bulk_bind_received(struct bulk_channel *channel)
{
    debug_printf("APP: bind received");
    return SYS_ERR_OK;
}

static void bulk_move_received(struct bulk_channel *channel,
                               struct bulk_buffer *buffer,
                               void *meta)
{
    BS_NET_DEBUG_BULK("chan=%p, buf=%p", channel, buffer);

    struct block_net_service*c = channel->user_state;

    errval_t err;

    struct bs_meta_data *bs_meta = (struct bs_meta_data*) meta;
    err = block_storage_write(bs_meta->block_id, buffer->address);
    if (err_is_fail(err)) {
        block_send_status_msg(c, BLOCK_NET_MSG_WRITE, bs_meta->req_id, err);
        debug_printf("Failed to update the block!");
    }
    err = bulk_channel_pass(channel, buffer, meta, BULK_CONT_NOP);
    if (err_is_fail(err)) {
        debug_printf("could not pass back the buffer");
    }
#if !BLOCK_BENCH_ENABLE
    block_send_status_msg(c, BLOCK_NET_MSG_WRITE, bs_meta->req_id, SYS_ERR_OK);
#endif
}

static void bulk_buffer_received(struct bulk_channel *channel,
                                 struct bulk_buffer *buffer,
                                 void *meta)
{
    BS_NET_DEBUG_BULK("chan=%p, buf=%p", channel, buffer);
    /* add the buffer to the list */
#if BULK_NET_BACKEND_PROXY
    errval_t err = bulk_alloc_return_buffer(&allocator_tx, buffer);
    if (err_is_fail(err)) {
        debug_printf("ERROR: failed to return buffer");
    }
#else
    block_server_insert_buffer(&bs_bulk_buffers, buffer, channel);
#endif
}

/**
 * callback when a buffer arrives on the bulk channel. this triggers the update
 * of the block in the block store
 */
static void bulk_copy_received(struct bulk_channel *channel,
                               struct bulk_buffer *buffer,
                               void *meta)
{
    BS_NET_DEBUG_BULK("chan=%p, buf=%p", channel, buffer);

    errval_t err;

    struct bs_meta_data *bs_meta = (struct bs_meta_data*) meta;
    err = block_storage_write(bs_meta->block_id, buffer->address);
    if (err_is_fail(err)) {
        debug_printf("Failed to update the block!");
    }
    err = bulk_channel_release(channel, buffer, BULK_CONT_NOP);
    if (err_is_fail(err)) {
        debug_printf("could not pass back the buffer");
    }
}

static void bulk_copy_released(struct bulk_channel *channel,
                               struct bulk_buffer *buffer)
{
    BS_NET_DEBUG_BULK("chan=%p, buf=%p", channel, buffer);

    assert(!"this shoud not happen... ");
}

/**
 * \brief handles block read request of a connected client
 */
static err_t handle_block_read(struct block_net_service*c,
                               struct tcp_pcb *tpcb,
                               size_t start_block,
                               size_t count,
                               uint32_t reqid,
                               struct bulk_continuation cont)
{
    BS_NET_DEBUG_TRACE

    errval_t err;

    struct bulk_channel *chan = &c->tx_chan;

    struct bs_meta_data *meta_data = malloc(
                    count * sizeof(struct bs_meta_data));
    assert(meta_data);

    for (uint32_t i = 0; i < count; ++i) {
        /* TODO: specify a pool */
#if BULK_NET_BACKEND_PROXY
        struct bulk_buffer *buf = bulk_alloc_new_buffer(&allocator_tx);
#else
        struct bulk_buffer *buf = block_server_get_buffer(&bs_bulk_buffers,
                        &c->tx_chan);
#endif
        if (!buf) {
            debug_printf("ERROR: Has no buffers left...(i=%i)\n", i);
            block_send_status_msg(c, BLOCK_NET_MSG_READ, reqid,
                                  BLOCK_NET_ERR_NO_BUFS);
            free(meta_data);
            return ERR_BUF;
        }

        err = block_storage_read(start_block + i, buf->address);
        if (err_is_fail(err)) {
            debug_printf("ERROR: block id is out of range: %i",
                         (uint32_t) (start_block + count));
            block_send_status_msg(c, BLOCK_NET_MSG_READ, reqid,
                                  BLOCK_NET_ERR_BLOCK_ID);
        }
        meta_data[i].block_id = start_block + i;
        meta_data[i].req_id = reqid;
        meta_data[i].cont = cont;
        BS_NET_DEBUG_BULK("bulk_channel_move: chan=%p, buf=%p\n", chan, buf);
        err = bulk_channel_move(chan, buf, meta_data + i, BULK_CONT_NOP);
        if (err_is_fail(err)) {
            block_send_status_msg(c, BLOCK_NET_MSG_READ, reqid, err);
            debug_printf("channel move failed");
        }
    }

    /* XXX: assume that the meta data has been copied... */
    free(meta_data);

    return ERR_OK;
}

#if BULK_NET_BACKEND_PROXY

static uint32_t pools_assigned = 0;
static uint32_t tx_bufs_other_side = 0;

static volatile uint32_t wait_cond = 0;

static inline void wait_for_condition(void)
{
    while (wait_cond) {
        messages_wait_and_handle_next();
    }
}

static void pool_assigned_cb(void *arg,
                             errval_t err,
                             struct bulk_channel *channel)
{
    debug_printf("pool_assigned_cb: sucess=%i\n", (err == SYS_ERR_OK));
    pools_assigned++;

    if (pools_assigned == 2) {
        wait_cond = 0;
    }
}

static void buffer_passed_cb(void *arg,
                             errval_t err,
                             struct bulk_channel *channel)
{
    tx_bufs_other_side++;
    if (tx_bufs_other_side == BULK_NET_PROXY_NUMBUFS) {
        wait_cond = 0;
    }
}

static void net_proxy_connected_cb(struct bulk_net_proxy *proxy)
{
    errval_t err;

    struct block_net_service *c = (struct block_net_service *) proxy->user_state;
    c->bound++;

    BS_NET_DEBUG_BULK(" > Channel Connected. #conn=%i", c->bound);

    if (c->bound != 2) {
        return;
    }

    BS_NET_DEBUG_BULK("%s", "all channels connected. allocating pools\n");
    err = bulk_alloc_init(&allocator_tx, BULK_NET_PROXY_NUMBUFS,
                          BULK_NET_PROXY_BUFSIZE, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to allocate pool\n");
    }

    err = bulk_alloc_init(&allocator_rx, BULK_NET_PROXY_NUMBUFS,
                          BULK_NET_PROXY_BUFSIZE, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to allocate pool");
    }
    struct bulk_continuation cont = {
        .handler = pool_assigned_cb,
        .arg = NULL, };

    BS_NET_DEBUG_BULK("%s", " > Assigning pools to channels");
    err = bulk_channel_assign_pool(&c->rx_chan, allocator_rx.pool, cont);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to assign pool to channel");
    }
    err = bulk_channel_assign_pool(&c->tx_chan, allocator_tx.pool, cont);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to assign pool to channel");
    }

    wait_cond = 1;
    wait_for_condition();

    BS_NET_DEBUG_BULK("%s", "Passing buffer to RX channel");
    cont.handler = buffer_passed_cb;
    struct bulk_buffer *buf = bulk_alloc_new_buffer(&allocator_rx);
    wait_cond = 1;
    while (buf) {
        err = bulk_channel_pass(&c->rx_chan, buf, NULL, cont);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to pass the buffer");
        }
        buf = bulk_alloc_new_buffer(&allocator_rx);
        thread_yield();
    }

    wait_for_condition();

    BS_NET_DEBUG_BULK("%s", "All buffers passed.");

    block_send_status_msg(c, BLOCK_NET_MSG_INIT, 0, BLOCK_NET_ERR_OK);

    BS_NET_DEBUG_BULK("%s", "Initialization done.\n\n\n");
}
#endif

/**
 * \brief handles the initialization of a new bulk channel
 */
static err_t handle_init(struct block_net_service *c,
                         struct tcp_pcb *tpcb,
                         struct bulk_net_endpoint_descriptor* rx_ep,
                         struct bulk_net_endpoint_descriptor* tx_ep)
{
    BS_NET_DEBUG_TRACE

    errval_t err;

    if (c->rx_chan.state != BULK_STATE_UNINITIALIZED

    || c->tx_chan.state != BULK_STATE_UNINITIALIZED) {
        /* this is an error, already initialized */
        debug_printf("Notice: channels already initialized.\n");
        return ERR_OK;
    }
    c->bound = 0;
#if BULK_NET_BACKEND_PROXY
    BS_NET_DEBUG_NET("%s", "creating bulk net proxy channel");

    debug_printf("initializing endpoints\n");
    bulk_local_init_endpoint(&c->rx_ep, NULL);
    bulk_local_init_endpoint(&c->tx_ep, NULL);

    struct bulk_channel_setup chan_setup = {
        .direction = BULK_DIRECTION_TX,
        .role = BULK_ROLE_MASTER,
        .trust = BULK_TRUST_FULL,
        .meta_size = sizeof(struct bs_meta_data),
        .waitset = get_default_waitset(),
        .user_state = c, };

    err = bulk_channel_create(&c->tx_chan,
                              (struct bulk_endpoint_descriptor *) &c->tx_ep,
                              &bulk_tx_cb, &chan_setup);
    if (err_is_fail(err)) {
        bulk_channel_destroy(&c->tx_chan, BULK_CONT_NOP);
        debug_printf("ERROR: Failed to create the TX channel\n");
        return err;
    }

    chan_setup.direction = BULK_DIRECTION_RX;
    err = bulk_channel_create(&c->rx_chan,
                              (struct bulk_endpoint_descriptor *) &c->rx_ep,
                              &bulk_rx_cb, &chan_setup);
    if (err_is_fail(err)) {
        bulk_channel_destroy(&c->tx_chan, BULK_CONT_NOP);
        debug_printf("ERROR: Failed to create the RX channel\n");
        return err;
    }

    bulk_local_init_endpoint(&c->rx_p_ep, &c->rx_chan);
    bulk_local_init_endpoint(&c->tx_p_ep, &c->tx_chan);

    c->tx_proxy.user_state = c;
    c->rx_proxy.user_state = c;
    /* XXX: tx_ep->ip.addr */

    BS_NET_DEBUG_BULK("bulk net proxy connect RX port=%i\n", tx_ep->port);
    err = bulk_net_proxy_connect(&c->tx_proxy, &c->tx_p_ep.generic,
                                 c->tx_chan.waitset, BLOCK_SIZE, "e10k",
                                 BLOCK_NET_TX_QUEUE,
                                 ntohl(tx_ep->ip.addr), tx_ep->port,
                                 net_proxy_connected_cb);
    if (err_is_fail(err)) {
        debug_printf("ERROR: failed to create net proxy\n");
        return err;
    }

    BS_NET_DEBUG_BULK("bulk net proxy connect RX port=%i\n", rx_ep->port);
    err = bulk_net_proxy_connect(&c->rx_proxy, &c->rx_p_ep.generic,
                                 c->rx_chan.waitset, BLOCK_SIZE, "e10k",
                                 BLOCK_NET_RX_QUEUE,
                                 ntohl(tx_ep->ip.addr), rx_ep->port,
                                 net_proxy_connected_cb);
    if (err_is_fail(err)) {
        debug_printf("ERROR: failed to create net proxy\n");
        return err;
    }

#else
    struct bulk_net_ep_setup ep_setup = {
        .port = rx_ep->port,
        .ip.addr = ntohl(rx_ep->ip.addr),
        .queue = BLOCK_NET_RX_QUEUE,
        .max_queues = BLOCK_NET_MAX_QUEUES,
        .buffer_size = BLOCK_SIZE,
        .buffer_count = BLOCK_NET_BUFFER_COUNT,
        .no_copy = BULK_NET_BACKEND_NOCOPY};

    /* create the RX endpoint */
    err = bulk_net_ep_create_remote(&c->rx_ep, &ep_setup);
    assert(!err_is_fail(err));

    ep_setup.port = tx_ep->port;
    ep_setup.queue = BLOCK_NET_TX_QUEUE;

    /* create the TX endpoint */
    err = bulk_net_ep_create_remote(&c->tx_ep, &ep_setup);
    assert(!err_is_fail(err));

    struct bulk_channel_bind_params params = {
        .role = BULK_ROLE_GENERIC,
        .user_state = c,
        .waitset = get_default_waitset(),
        .trust = BULK_TRUST_FULL};

    struct bulk_continuation cont = {
        .arg = c,
        .handler = chan_bind_cb};

    err = bulk_channel_bind(&c->rx_chan,
                    (struct bulk_endpoint_descriptor *) &c->rx_ep,
                    &bulk_rx_cb, &params, cont);
    if (err_is_fail(err)) {
        return err;
    }

    err = bulk_channel_bind(&c->tx_chan,
                    (struct bulk_endpoint_descriptor *) &c->tx_ep,
                    &bulk_tx_cb, &params, cont);
    if (err_is_fail(err)) {
        /* TODO: teardown channel */
        return err;
    }
#endif
    BS_NET_DEBUG_NET("%s", "handle init done.");
    return ERR_OK;
}

/**
 * \brief handles the reply of an error in case of unkown request
 */
static err_t handle_bad_request(struct block_net_service *c,
                                struct tcp_pcb *tpcb)
{
    BS_NET_DEBUG_TRACE

    return ERR_OK;
}

#if 0
/**
 * \brief handler for disconnect requests
 */
static err_t handle_disconnect(struct block_net_service *c, struct tcp_pcb *tpcb)
{
    // free up resources

    // close the network connection
    assert(!"NYI: block_net_init");
    return ERR_OK;
}

/**
 * \brief handles the connection event of a new network block server clients
 */
static err_t client_connect_cb(void)
{
    // setup data structurs for the newly connected client

    assert(!"NYI: block_net_init");
    return SYS_ERR_OK;
}
#endif

static struct tcp_pcb *server_pcb;

static err_t bs_net_recv(void *arg,
                         struct tcp_pcb *tpcb,
                         struct pbuf *p,
                         err_t err)
{
    BS_NET_DEBUG_TRACE

    if (p == NULL) {
        /* connection closed. clean up and EXIT the program. */
        tcp_close(tpcb);
        assert(!"NYI: cleaning up the resources!");
        exit(EXIT_SUCCESS);
    }

    if ((err != ERR_OK) || !p) {
        /* there was an error.. */
    }

    struct block_net_msg *msg = (struct block_net_msg *) p->payload;

    if (p->tot_len != msg->size) {
        /* some thing wrong... */
    }

    struct block_net_service *c = (struct block_net_service *) arg;
    err_t reterr;

    switch (msg->type) {
        case BLOCK_NET_MSG_INIT:
            reterr = handle_init(c, tpcb, &msg->msg.setup.rx_ep,
                                 &msg->msg.setup.tx_ep);
            break;
        case BLOCK_NET_MSG_READ:
            reterr = handle_block_read(c, tpcb, msg->msg.read.block_id,
                                       msg->msg.read.count,
                                       msg->msg.read.req_id,
                                       msg->msg.read.cont);
            break;
        default:
            debug_printf("Received unknown request.");
            reterr = handle_bad_request(c, tpcb);
            break;
    }

    tcp_recved(tpcb, p->tot_len);

    pbuf_free(p);

    return ERR_OK;
}

/*
 * This function is called periodically from TCP.
 * and is also responsible for taking care of stale connections.
 */
static err_t bs_net_poll(void *arg, struct tcp_pcb *tpcb)
{
    // BS_NET_DEBUG_TRACE

    return ERR_OK;
}

static void bs_net_err(void *arg, err_t err)
{
    BS_NET_DEBUG_TRACE

    assert(!"NYI: bs_net_err");
}

static err_t bs_net_accept(void *arg, struct tcp_pcb *tpcb, err_t err)
{
    BS_NET_DEBUG_TRACE

#if TCP_LISTEN_BACKLOG
    /* Decrease the listen backlog counter */
    struct tcp_pcb_listen *lpcb = (struct tcp_pcb_listen*)arg;
    tcp_accepted(lpcb);
#endif
    tcp_setprio(tpcb, TCP_PRIO_NORMAL);

    struct block_net_service *c = malloc(sizeof(struct block_net_service));
    if (!c) {
        debug_printf("Failed to allocate memory for client struct\n");
        return ERR_MEM;
    }

    memset(c, 0, sizeof(*c));

    c->tpcb = tpcb;

    tcp_arg(tpcb, c);

    tcp_recv(tpcb, bs_net_recv);
    tcp_err(tpcb, bs_net_err);
    tcp_poll(tpcb, bs_net_poll, 4);

    return ERR_OK;
}

/**
 * \brief initializes the network server of the block service
 */
errval_t block_net_init(uint16_t port)
{
    BS_NET_DEBUG_TRACE

    BS_NET_DEBUG_NET("%s", "initializing lwip...");
    if (lwip_init("e10k", 1) == false) {
        debug_printf("ERROR: lwip_init_auto failed!\n");
        return 1;
    }

    server_pcb = tcp_new();
    if (server_pcb == NULL) {
        return LWIP_ERR_MEM;
    }

    BS_NET_DEBUG_NET("binding to port %i", port);
    err_t e = tcp_bind(server_pcb, IP_ADDR_ANY, port);
    if (e != ERR_OK) {
        if (e == ERR_USE) {
            assert(!"TODO: Change the port. port is in use\n");
        }
        printf("ERROR: tcp_bind failed!\n");
        return 2;
    }

    return SYS_ERR_OK;
}

static bool server_running = false;

/**
 * \brief starts the network server of block service to accept requests
 */
errval_t block_net_start(void)
{
    BS_NET_DEBUG_TRACE

    errval_t err;

    server_pcb = tcp_listen(server_pcb);
    if (server_pcb == NULL) {
        printf("ERROR: tcp_listen failed!\n");
        return 1;
    }

    tcp_arg(server_pcb, server_pcb);
    tcp_accept(server_pcb, bs_net_accept);

    server_running = true;

    struct waitset *ws = get_default_waitset();
    while (true) {
        err = event_dispatch_non_block(ws);
        if (err != LIB_ERR_NO_EVENT) {
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch");
                break;
            }
        }

        wrapper_perform_lwip_work();
        /*err = event_dispatch(ws);
         if (err_is_fail(err)) {
         DEBUG_ERR(err, "in event_dispatch");
         break;
         }*/
    }

    return SYS_ERR_OK;
}

/**
 * \brief stops the request handling of the network block service
 */
errval_t block_net_stop(void)
{
    BS_NET_DEBUG_TRACE

    server_running = false;
    return SYS_ERR_OK;
}

/**
 * \brief lookup of the block server connection based on the requested block
 *
 * The client may be connected to multiple network block servers. The request
 * needs to be forwarded to the correct block server based in the requested
 * block id.
 *
 * XXX: Supply the block server ID instead? or just say there is one block server?
 */
struct block_net_server *block_net_server_lookup(size_t block_start)
{
    assert(!"NYI: block_net_server_lookup");
    return NULL;
}

