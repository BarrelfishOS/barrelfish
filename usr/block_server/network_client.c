/**
 * \file
 * \brief Network client of the block service
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

#include <lwip/init.h>
#include <lwip/tcp.h>
#include <lwip/ip_addr.h>

#include "block_server.h"
#include "network_common.h"
#include "network_client.h"
#include "local_server.h"

#if BULK_NET_BACKEND_PROXY
#include <bulk_transfer/bulk_net_proxy.h>
#include <bulk_transfer/bulk_local.h>
#endif

/* number of bulk connections */
static volatile uint32_t bulk_connections = 0;

// condition used to singal controlling code to wait for a condition
static bool wait_cond;

static inline void wait_for_condition(void)
{
    while (wait_cond) {
        messages_wait_and_handle_next();
    }
}

/* ----------------------bulk transfer callbacks ----------------------------*/

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
    .copy_received = bulk_copy_received,
    .buffer_received = bulk_buffer_received,
    .copy_released = bulk_copy_released, };

static struct bulk_channel_callbacks bulk_tx_cb = {
    .bind_received = bulk_bind_received,
    .pool_assigned = bulk_pool_assigned_cb,
    .pool_removed = bulk_pool_removed_cb,
    .buffer_received = bulk_buffer_received,
    .copy_released = bulk_copy_released,
    .move_received = bulk_move_received,
    .copy_received = bulk_copy_received,

};

/* --------------------- lwip/tcp callbacks ---------------------------- */

/**
 * callback when a send event has been completed
 */
static err_t block_net_sent_cb(void *arg, struct tcp_pcb *pcb, u16_t len)
{
    BS_NET_DEBUG_TRACE

    assert(pcb != NULL);

    return ERR_OK;
}

/**
 * callback when a new packet has been arrived
 */
static err_t block_net_recv_cb(void *arg,
                               struct tcp_pcb *pcb,
                               struct pbuf *pb,
                               err_t err)
{
    BS_NET_DEBUG_TRACE

    assert(pcb != NULL);
    assert(pb);
    if (pb == NULL) {
        /* connection closed. clean up and EXIT the program. */
        tcp_close(pcb);
        assert(!"NYI: cleaning up the resources!");
        exit(EXIT_SUCCESS);
    }

    if ((err != ERR_OK) || !pb) {
        /* there was an error... TODO */
        debug_printf("there was an error...\n");
        return err;
    }

    struct block_net_msg *msg = (struct block_net_msg *) pb->payload;
    assert(msg);
    if (pb->tot_len != msg->size) {
        /* some thing wrong... */
    }

    struct block_net_service *c = (struct block_net_service *) arg;

    enum block_net_msg_type req;

    switch (msg->type) {
        case BLOCK_NET_MSG_STATUS:

            req = msg->msg.status.req;
            uint32_t reqid = msg->msg.status.reqid;
            enum block_net_err status = msg->msg.status.code;
            tcp_recved(pcb, pb->tot_len);

            pbuf_free(pb);

            switch (msg->msg.status.req) {
                case BLOCK_NET_MSG_INIT:
                    if (status == BLOCK_NET_ERR_OK) {
                        debug_printf("/* all fine, connection fully established */\n");
                        wait_cond = 0;
                        c->bound = 1;
                    }
                    break;
                case BLOCK_NET_MSG_READ:
                    block_local_send_status(req, reqid, status);
                    break;
                case BLOCK_NET_MSG_WRITE:
#if BLOCK_BENCH_ENABLE
                    testrun_handle_status(req, reqid, status);
#else
                    block_local_send_status(req, reqid, status);
#endif
                    break;
                default:
                    break;
            }
            return ERR_OK;
            break;
        default:
            debug_printf("got an unknown reply...");
            break;
    }

    tcp_recved(pcb, pb->tot_len);

    pbuf_free(pb);

    return ERR_OK;
}

/**
 * callback when error occures
 */
static void block_net_err_cb(void *arg, err_t err)
{
    BS_NET_DEBUG_TRACE

    debug_printf("tcp is err: %d\n", (int) err);
}

/**
 * callback for polling the connection
 */
static err_t block_net_poll_cb(void *arg, struct tcp_pcb *pcb)
{

    return ERR_OK;
}

/**
 * callback when the connection to the server is established.
 */
static err_t block_net_connected_cb(void *arg, struct tcp_pcb *pcb, err_t err)
{
    BS_NET_DEBUG_TRACE

    if (err != ERR_OK) {
        fprintf(stderr, "tcp connection failed\n");
        if (err == ERR_TIMEOUT) {
            debug_printf("connection attempt timed out..\n");
            /* TODO: try again */
        }
        wait_cond = false;
        return err;
    }

    tcp_sent(pcb, block_net_sent_cb);
    tcp_recv(pcb, block_net_recv_cb);
    tcp_err(pcb, block_net_err_cb);
    tcp_poll(pcb, block_net_poll_cb, 10);

    wait_cond = false;

    return ERR_OK;
}

/* ------------------------- connect / disconnect ---------------------------*/

static errval_t block_net_send_ep(struct block_net_service *server)
{
    BS_NET_DEBUG_TRACE

    assert(server->rx_chan.state != BULK_STATE_UNINITIALIZED);
    assert(server->tx_chan.state != BULK_STATE_UNINITIALIZED);
    err_t err;

    uint16_t size = sizeof(struct block_net_msg);
    struct block_net_msg *msg = malloc(size);
    if (!msg) {
        return LWIP_ERR_MEM; /* ERROR CODE ?? */
    }

    msg->type = BLOCK_NET_MSG_INIT;
    msg->size = size;
#if BULK_NET_BACKEND_PROXY
    msg->msg.setup.tx_ep.ip = server->tpcb->local_ip;
    msg->msg.setup.tx_ep.port = BLOCK_NET_PORT_RX;
    msg->msg.setup.rx_ep.ip = server->tpcb->local_ip;
    msg->msg.setup.rx_ep.port = BLOCK_NET_PORT_TX;
#else
    msg->msg.setup.rx_ep = *((struct bulk_net_endpoint_descriptor*) server
                    ->tx_chan.ep);
    msg->msg.setup.do_bind = 1;
    msg->msg.setup.tx_ep = *((struct bulk_net_endpoint_descriptor*) server
                    ->rx_chan.ep);
#endif
    /* TODO: check if there is enough space left... */

    err = tcp_write(server->tpcb, msg, size, TCP_WRITE_FLAG_COPY);
    if (err != ERR_OK) {
        /* abort the sending */
        fprintf(stderr, "error writing %d\n", err);
        return LWIP_ERR_MEM;
    }

    err = tcp_output(server->tpcb);

    if (err != ERR_OK) {
        fprintf(stderr, "error in tcp_output %d\n", err);
        return LWIP_ERR_MEM;
    }

    wait_cond = 1;

    BS_NET_DEBUG_BULK("%s", "waiting for bulk binding\n");

    wait_for_condition();

    BS_NET_DEBUG_BULK("%s", "binding complete. \n");

    return SYS_ERR_OK;
}

#if BULK_NET_BACKEND_PROXY

static void proxy_connected_cb(struct bulk_net_proxy *proxy)
{
    debug_printf("APPL: cb_rx_connected\n");

    bulk_connections++;

    if (bulk_connections == 2) {
        wait_cond = 0;
    }
}
#endif

/**
 * \brief connects to a network block service
 */
errval_t block_net_connect(struct block_net_service *server,
                           struct ip_addr *ip,
                           uint16_t port)
{
    BS_NET_DEBUG_TRACE

    err_t err;
    struct tcp_pcb *pcb;

    memset(server, 0, sizeof(*server));
    memcpy(&server->ip, ip, sizeof(*ip));
    server->port = port;

    BS_NET_DEBUG_NET("%s", "lwip initializing... ");
    if (lwip_init("e10k", 1) == false) {
        debug_printf("ERROR: lwip_init_auto failed!\n");
        return 1;
    }

    BS_NET_DEBUG_NET("%s", "lwip init done.");

    // initialize network connection to the block server
    pcb = tcp_new();
    if (pcb == NULL) {
        return LWIP_ERR_CONN;
    }

    wait_cond = true;
    err = tcp_connect(pcb, &server->ip, server->port, block_net_connected_cb);
    // obtain basic information about the block server

    BS_NET_DEBUG_NET("%s", "waiting for condition");
    wait_for_condition();

    if (err != ERR_OK) {
        /* there was something wrong */
        return err;
    }

    tcp_arg(pcb, server);
    server->tpcb = pcb;

    // initialize the bulk channel to the block server
    errval_t b_err;

#if BULK_NET_BACKEND_PROXY
    BS_NET_DEBUG_NET("%s", "creating bulk net proxy channel");

    bulk_local_init_endpoint(&server->rx_ep, NULL);
    bulk_local_init_endpoint(&server->tx_ep, NULL);

#else
    BS_NET_DEBUG_NET("%s", "creating bulk channels");

    struct bulk_net_ep_setup ep_setup = {
        .port = BLOCK_NET_PORT_RX,
        .ip = pcb->local_ip,    /// XXX: is this correct?
        .queue = BLOCK_NET_RX_QUEUE,
        .max_queues = BLOCK_NET_MAX_QUEUES,
        .buffer_size = BLOCK_SIZE,
        .buffer_count = BLOCK_NET_BUFFER_COUNT,
        .no_copy = BULK_NET_BACKEND_NOCOPY };
    b_err = bulk_net_ep_create(&server->rx_ep, &ep_setup);
    assert(!err_is_fail(b_err));

    ep_setup.port = BLOCK_NET_PORT_TX;
    ep_setup.queue = BLOCK_NET_TX_QUEUE;
    b_err = bulk_net_ep_create(&server->tx_ep, &ep_setup);
    assert(!err_is_fail(b_err));
#endif

    struct bulk_channel_setup chan_setup = {
        .direction = BULK_DIRECTION_TX,
        .role = BULK_ROLE_MASTER,
        .trust = BULK_TRUST_FULL,
        .meta_size = sizeof(struct bs_meta_data),
        .waitset = get_default_waitset(),
        .user_state = server, };

    b_err = bulk_channel_create(
                    &server->tx_chan,
                    (struct bulk_endpoint_descriptor *) &server->tx_ep,
                    &bulk_tx_cb, &chan_setup);
    if (err_is_fail(b_err)) {
        bulk_channel_destroy(&server->tx_chan, BULK_CONT_NOP);
        debug_printf("Failed to create the TX channel\n");
        return b_err;
    }

    chan_setup.direction = BULK_DIRECTION_RX;
    b_err = bulk_channel_create(
                    &server->rx_chan,
                    (struct bulk_endpoint_descriptor *) &server->rx_ep,
                    &bulk_rx_cb, &chan_setup);
    if (err_is_fail(b_err)) {
        bulk_channel_destroy(&server->tx_chan, BULK_CONT_NOP);
        debug_printf("Failed to create the RX channel\n");
        return b_err;
    }
#if BULK_NET_BACKEND_PROXY

    bulk_local_init_endpoint(&server->rx_p_ep, &server->rx_chan);
    bulk_local_init_endpoint(&server->tx_p_ep, &server->tx_chan);

    err = bulk_net_proxy_listen(&server->rx_proxy, &server->rx_p_ep.generic,
                    server->rx_chan.waitset, BLOCK_SIZE, "e10k",
                    BLOCK_NET_RX_QUEUE,
                    BLOCK_NET_PORT_RX, proxy_connected_cb);
    if (err_is_fail(err)) {
        return err;
    }
    server->rx_proxy.user_state = server;

    err = bulk_net_proxy_listen(&server->tx_proxy, &server->tx_p_ep.generic,
                    server->tx_chan.waitset, BLOCK_SIZE, "e10k",
                    BLOCK_NET_TX_QUEUE,
                    BLOCK_NET_PORT_TX, proxy_connected_cb);
    server->tx_proxy.user_state = server;
    if (err_is_fail(err)) {
        return err;
    }

#endif
    return block_net_send_ep(server);
}

/**
 * \brief disconnects form the network block service
 */
errval_t block_net_disconnect(struct block_net_service *server)
{
    BS_NET_DEBUG_TRACE

    if (server->tx_chan.state == BULK_STATE_CONNECTED) {
        /* free up the bulk channel */
    }

    if (server->rx_chan.state == BULK_STATE_CONNECTED) {
        /* free up the bulk channel */
    }

    // tear down the bulk channel
    if (server->tpcb) {
        // tear down the service level network connection
        tcp_close(server->tpcb);
        tcp_arg(server->tpcb, NULL);
        tcp_sent(server->tpcb, NULL);
        tcp_recv(server->tpcb, NULL);
    }
    /*TODO:  do we have to free up something from lwip ? */

    return SYS_ERR_OK;
}

/* ------------------------- read / write  ---------------------------*/

/**
 * \brief issues a new retrieval request to the network block server
 *
 * \param block_start   the id of the first block
 * \param count         the number of blocks to read
 */
errval_t block_net_read(struct block_net_service *server,
                        size_t block_start,
                        size_t count,
                        uint32_t seqn,
                        struct bulk_continuation cont)
{
    BS_NET_DEBUG_TRACE

    err_t err;

    if (server->rx_chan.state != BULK_STATE_CONNECTED || !server->tpcb) {
        return BLOCK_ERR_NOT_CONNECTED;
    }

    if (server->bound != 1) {
        /* the connection has not yet been fully established yet */
        return BLOCK_ERR_NOT_CONNECTED;
    }

    uint16_t size = sizeof(struct block_net_msg);
    struct block_net_msg *msg = malloc(size);
    if (!msg) {
        return LWIP_ERR_MEM; /* ERROR CODE ?? */
    }

    msg->type = BLOCK_NET_MSG_READ;
    msg->size = size;
    msg->msg.read.block_id = block_start;
    msg->msg.read.count = count;
    msg->msg.read.req_id = seqn;
    msg->msg.read.cont = cont;

    server->tpcb->flags |= TF_NODELAY;

    /* TODO: check if there is enough space left... */

    err = tcp_write(server->tpcb, msg, size, TCP_WRITE_FLAG_COPY);
    if (err != ERR_OK) {
        /* abort the sending */
        fprintf(stderr, "error writing %d\n", err);
        return LWIP_ERR_MEM;
    }

    err = tcp_output(server->tpcb);

    if (err != ERR_OK) {
        fprintf(stderr, "error in tcp_output %d\n", err);
        return LWIP_ERR_MEM;
    }

    /* XXX: assume that the data hase been copied into the pbuf */
    free(msg);

    return SYS_ERR_OK;
}

/**
 * \brief forwards the write request to the network block server
 *
 * \param block_start   the id of the first block
 * \param count         the numbers of blocks to write
 * \param buf           pointer to an array of buffers
 * \param cont          continuation for the callback when the buffer is sent
 *
 */
errval_t block_net_write(struct block_net_service *server,
                         size_t count,
                         struct bulk_buffer **buf,
                         struct bs_meta_data *meta,
                         struct bulk_continuation cont)
{
    BS_NET_DEBUG_TRACE

    assert(server->tx_chan.state == BULK_STATE_CONNECTED);

    if (server->bound != 1) {
        /* the connection has not yet been fully established */
        return 1;
    }

    /*
     * Note: we just need to send the buffers over the bulk channel with the
     *       corresponding meta data.
     */
    errval_t err;
    for (uint32_t i = 0; i < count; ++i) {
        err = bulk_channel_move(&server->tx_chan, buf[i], meta + i, cont);
        if (err_is_fail(err)) {
            if (err != BULK_TRANSFER_BUFFER_NOT_OWNED) {
                /* this indicates a serious problem and all will fail */
                return err;
            }
        }
    }

    // return status code
    return SYS_ERR_OK;
}

errval_t block_net_pass(struct block_net_service *server,
                        size_t count,
                        struct bulk_buffer **buf,
                        struct bs_meta_data *meta,
                        struct bulk_continuation cont)
{
    BS_NET_DEBUG_TRACE

    assert(server->rx_chan.state == BULK_STATE_CONNECTED);

    if (server->bound != 1) {
        /* the connection has not yet been fully established */
        return 1;
    }

    /*
     * Note: we just need to send the buffers over the bulk channel with the
     *       corresponding meta data.
     */
    errval_t err;
    for (uint32_t i = 0; i < count; ++i) {
        err = bulk_channel_pass(&server->rx_chan, buf[i], meta + i, cont);
        if (err_is_fail(err)) {
            if (err != BULK_TRANSFER_BUFFER_NOT_OWNED) {
                /* this indicates a serious problem and all will fail */
                return err;
            }
        }
    }

    // return status code
    return SYS_ERR_OK;
}

errval_t block_net_release(struct block_net_service *server,
                           size_t count,
                           struct bulk_buffer **buf,
                           struct bs_meta_data *meta,
                           struct bulk_continuation cont)
{
    BS_NET_DEBUG_TRACE

    assert(server->rx_chan.state == BULK_STATE_CONNECTED);

    if (server->bound != 1) {
        /* the connection has not yet been fully established */
        return 1;
    }

    /*
     * Note: we just need to send the buffers over the bulk channel with the
     *       corresponding meta data.
     */
    errval_t err;
    for (uint32_t i = 0; i < count; ++i) {
        err = bulk_channel_release(&server->rx_chan, buf[i], cont);
        if (err_is_fail(err)) {
            if (err != BULK_TRANSFER_BUFFER_NOT_OWNED) {
                /* this indicates a serious problem and all will fail */
                return err;
            }
        }
    }

    // return status code
    return SYS_ERR_OK;
}

/* ------------------------ bulk transfer callbacks --------------------------*/

static errval_t bulk_bind_received(struct bulk_channel *channel)
{
    BS_NET_DEBUG_TRACE

    bulk_connections++;

    if (bulk_connections == 2) {
        wait_cond = 0;
    }
    return SYS_ERR_OK;
}

static errval_t bulk_pool_assigned_cb(struct bulk_channel *channel,
                                      struct bulk_pool *pool)
{
    BS_NET_DEBUG_TRACE

    return SYS_ERR_OK;
}

static errval_t bulk_pool_removed_cb(struct bulk_channel *channel,
                                     struct bulk_pool *pool)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

static void bulk_move_received(struct bulk_channel *channel,
                               struct bulk_buffer *buffer,
                               void *meta)
{
    BS_NET_DEBUG_TRACE

#if BLOCK_BENCH_ENABLE
    testrun_bulk_move_received(channel, buffer,meta);
#else
    /* todo directly forward.. */
    block_local_data_ready(buffer, meta);
#endif
}

static void bulk_buffer_received(struct bulk_channel *channel,
                                 struct bulk_buffer *buffer,
                                 void *meta)
{
#if BLOCK_BENCH_ENABLE
    testrun_bulk_buffer_received(channel,
                                    buffer,
                                    meta);
#else
    block_local_return_buffer(channel, buffer, meta);
#endif
}

static void bulk_copy_received(struct bulk_channel *channel,
                               struct bulk_buffer *buffer,
                               void *meta)
{
#if BLOCK_BENCH_ENABLE
    struct bs_meta_data *bsmeta = (struct bs_meta_data *) meta;

    struct bs_callback_arg arg = {
        .binding = bsmeta->cont.arg,
        .buf = buffer,
        .block_id = bsmeta->block_id,
        .req_id = bsmeta->req_id};

    if (bsmeta->cont.handler) {
        bsmeta->cont.handler(&arg, SYS_ERR_OK, channel);
    }
#else
    /* todo directly forward.. */
#endif
}

static void bulk_copy_released(struct bulk_channel *channel,
                               struct bulk_buffer *buffer)
{
#if BLOCK_BENCH_ENABLE

#else
    /* todo directly forward.. */
    block_local_release_copy(buffer);
#endif
}
