#include <stdio.h>
#include <string.h>
#include <ipv4/lwip/inet.h>

#include<bulk_transfer/bulk_transfer.h>
#include<bulk_transfer/bulk_allocator.h>
#include<bulk_transfer/bulk_local.h>
#include<bulk_transfer/bulk_net_proxy.h>

#include "sleep.h"

#define BUFSZ 0x1000
#define PORT 1234

static void dummy_cont_cb(void *arg, errval_t err, struct bulk_channel *channel)
{
    //printf("DUMMY CONT\n");
}

struct bulk_continuation dummy_cont = { .arg = NULL, .handler = dummy_cont_cb, };

/******************************************************************************/
/* Receiver */

#define RXNBUFS 4

static errval_t cb_rx_bind_received(struct bulk_channel *channel);
static void cb_rx_move_received(struct bulk_channel *channel,
                                struct bulk_buffer *buffer,
                                void *meta);
static void cb_rx_copy_received(struct bulk_channel *channel,
                                struct bulk_buffer *buffer,
                                void *meta);
static errval_t cb_rx_pool_assigned(struct bulk_channel *channel,
                                    struct bulk_pool *pool);

static struct bulk_channel rx_channel;
static struct bulk_allocator rx_allocator;
static struct bulk_channel_callbacks rx_callbacks = { .bind_received =
    cb_rx_bind_received, .move_received = cb_rx_move_received, .copy_received =
    cb_rx_copy_received, .pool_assigned = cb_rx_pool_assigned };
static struct bulk_net_proxy rx_proxy;

static errval_t cb_rx_bind_received(struct bulk_channel *channel)
{
    debug_printf("APPL: cb_rx_bind_received\n");
    return SYS_ERR_OK;
}

static errval_t cb_rx_pool_assigned(struct bulk_channel *channel,
                                    struct bulk_pool *pool)
{
    debug_printf("APPL: cb_rx_pool_assigned. Checking and reply OK.\n");
    return SYS_ERR_OK;
}

static void cb_rx_move_received(struct bulk_channel *channel,
                                struct bulk_buffer *buffer,
                                void *meta)
{
    uint32_t meta_val = *((uint32_t*) meta);
    debug_printf("APPL: cb_rx_move_received: %p, meta=%x\n", buffer->address,
                    meta_val);
    meta_val++;

    volatile char *e = buffer->address;
    for (int i = 0; i < buffer->pool->buffer_size; ++i) {
        assert(e[i] == 123);
    }

    errval_t err = bulk_channel_pass(channel, buffer, &meta_val, dummy_cont);
    assert(!err_is_fail(err));
}

static void cb_rx_copy_received(struct bulk_channel *channel,
                                struct bulk_buffer *buffer,
                                void *meta)
{
    debug_printf("APPL: cb_rx_copy_received: meta=%x\n", *((uint32_t*) meta));
    volatile char *e = buffer->address;
    for (int i = 0; i < buffer->pool->buffer_size; ++i) {
        assert(e[i] == 101);
    }
    errval_t err = bulk_channel_release(channel, buffer, dummy_cont);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "channel release\n");
        assert(!err_is_fail(err));
    }

}

static void cb_rx_pool_assign_done(void *arg,
                                   errval_t err,
                                   struct bulk_channel *channel)
{
    size_t i;
    struct bulk_buffer *b;
    uint32_t meta = 0;

    debug_printf("APPL: cb_rx_pool_assign_done()\n");
    assert(arg == rx_allocator.pool);
    assert(err_is_ok(err));

    // Register buffers
    debug_printf("Registering buffers...\n");
    for (i = 0; i < RXNBUFS; i++) {
        b = bulk_alloc_new_buffer(&rx_allocator);
        assert(b != NULL);
        debug_printf("  b[%p]->p=%"PRIx64"\n", b, b->phys);
        err = bulk_channel_pass(channel, b, &meta, dummy_cont);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Chan pass failed %d\n", i);
        }
        assert(!err_is_fail(err));
        event_dispatch_non_block(channel->waitset);
        event_dispatch_non_block(channel->waitset);
        event_dispatch_non_block(channel->waitset);
    }
    debug_printf("Buffers registered\n");
}

static void cb_rx_connected(struct bulk_net_proxy *proxy)
{
    errval_t err;
    debug_printf("APPL: cb_rx_connected\n");

    struct bulk_continuation cont = { .handler = cb_rx_pool_assign_done, .arg =
        rx_allocator.pool };

    err = bulk_channel_assign_pool(&rx_channel, rx_allocator.pool, cont);
    assert(!err_is_fail(err));
}

static void init_receiver(struct waitset *waitset)
{
    static struct bulk_local_endpoint ep, p_ep;
    errval_t err;
    struct bulk_channel_setup setup = { .direction = BULK_DIRECTION_RX,
                    .role = BULK_ROLE_MASTER,
                    .meta_size = sizeof(uint32_t),
                    .waitset = waitset,
                    .trust = BULK_TRUST_FULL, };

    err = bulk_alloc_init(&rx_allocator, RXNBUFS, BUFSZ, NULL);
    assert(!err_is_fail(err));
    bulk_local_init_endpoint(&ep, NULL);
    err = bulk_channel_create(&rx_channel, &ep.generic, &rx_callbacks, &setup);
    assert(!err_is_fail(err));

    bulk_local_init_endpoint(&p_ep, &rx_channel);
    err = bulk_net_proxy_listen(&rx_proxy, &p_ep.generic, waitset, BUFSZ,
            "e10k", 3, PORT, cb_rx_connected);
    assert(err_is_ok(err));

}

/******************************************************************************/
/* Sender */

static errval_t cb_tx_bind_received(struct bulk_channel *channel);
static void cb_tx_pool_assign_done(void *arg,
                                   errval_t err,
                                   struct bulk_channel *channel);
static void cb_tx_buffer_received(struct bulk_channel *channel,
                                  struct bulk_buffer *buffer,
                                  void *meta);
static void cb_tx_copy_released(struct bulk_channel *channel,
                                struct bulk_buffer *buffer);
static errval_t cb_tx_pool_assigned(struct bulk_channel *channel,
                                    struct bulk_pool *pool);

static struct bulk_channel tx_channel;
static int tx_phase = 0;
static struct bulk_allocator tx_allocator;
static struct bulk_channel_callbacks tx_callbacks = {
                .bind_received = cb_tx_bind_received,
                .buffer_received = cb_tx_buffer_received,
                .copy_released = cb_tx_copy_released,
                .pool_assigned = cb_tx_pool_assigned, };
static struct bulk_net_proxy tx_proxy;

static errval_t cb_tx_bind_received(struct bulk_channel *channel)
{
    debug_printf("APPL: cb_tx_bind_received\n");
    return SYS_ERR_OK;
}

static void cb_tx_pool_assign_done(void *arg,
                                   errval_t err,
                                   struct bulk_channel *channel)
{
    debug_printf("APPL: cb_tx_pool_assign_done()\n");
    assert(tx_allocator.pool == arg);
    debug_printf("Waiting for some time to make sure other side is ready\n");
    milli_sleep(2000);
    debug_printf("Sleep done\n");
    tx_phase = 2;
}


static errval_t cb_tx_pool_assigned(struct bulk_channel *channel,
                                    struct bulk_pool *pool)
{
    debug_printf("APPL: cb_tx_pool_assigned()\n");

    return SYS_ERR_OK;
}

static void cb_tx_buffer_received(struct bulk_channel *channel,
                                  struct bulk_buffer *buffer,
                                  void *meta)
{
    debug_printf("APPL: cp_tx_buffer_received: %p, meta=%x\n", buffer->address,
                    *((uint32_t*) meta));
    errval_t err = bulk_alloc_return_buffer(&tx_allocator, buffer);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Returning the buffer\n");
        assert(!err_is_fail(err));
    }

    tx_phase = 4;
}

static void cb_tx_copy_released(struct bulk_channel *channel,
                                struct bulk_buffer *buffer)
{
    debug_printf("APPL: cp_tx_copy_released\n");
    tx_phase = 6;
    errval_t err = bulk_alloc_return_buffer(&tx_allocator, buffer);
    assert(!err_is_fail(err));

}

static void cb_tx_connected(struct bulk_net_proxy *proxy)
{
    errval_t err;
    debug_printf("APPL: cb_tx_connected\n");
    tx_phase = 1;

    struct bulk_continuation cont = { .handler = cb_tx_pool_assign_done, .arg =
        tx_allocator.pool };

    err = bulk_channel_assign_pool(&tx_channel, tx_allocator.pool, cont);
    assert(!err_is_fail(err));
}

static void init_sender(struct waitset *waitset)
{
    static struct bulk_local_endpoint ep, p_ep;
    errval_t err;
    struct bulk_channel_setup setup = { .direction = BULK_DIRECTION_TX,
                    .role = BULK_ROLE_MASTER,
                    .meta_size = sizeof(uint32_t),
                    .waitset = waitset,
                    .trust = BULK_TRUST_FULL, };
    err = bulk_alloc_init(&tx_allocator, 4, BUFSZ, NULL);
    assert(!err_is_fail(err));

    bulk_local_init_endpoint(&ep, NULL);
    err = bulk_channel_create(&tx_channel, &ep.generic, &tx_callbacks, &setup);
    assert(!err_is_fail(err));

    bulk_local_init_endpoint(&p_ep, &tx_channel);
    err = bulk_net_proxy_connect(&tx_proxy, &p_ep.generic, waitset, BUFSZ,
            "e10k", 3, ntohl(inet_addr("192.168.99.2")), PORT, cb_tx_connected);
    assert(!err_is_fail(err));
}

static void tx_process(void)
{
    errval_t err;
    uint32_t meta;
    struct bulk_buffer *buffer;

    if (tx_phase == 2) {
        meta = 42;
        debug_printf("APPL: Allocating buffer...\n");
        buffer = bulk_alloc_new_buffer(&tx_allocator);
        assert(buffer);
        memset(buffer->address, 123, buffer->pool->buffer_size);
        debug_printf("APPL: Starting move... meta=%x\n", meta);
        err = bulk_channel_move(&tx_channel, buffer, &meta, dummy_cont);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Bulk Channel Move Failed...\n");
            assert(!"FOOFO");
        }

        tx_phase++;
    } else if (tx_phase == 4) {
        meta = 44;
        debug_printf("APPL: Allocating buffer...\n");
        buffer = bulk_alloc_new_buffer(&tx_allocator);
        assert(buffer);
        memset(buffer->address, 101, buffer->pool->buffer_size);
        debug_printf("APPL: Starting copy... meta=%x\n", meta);
        err = bulk_channel_copy(&tx_channel, buffer, &meta, dummy_cont);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Bulk Channel Move Failed...\n");
            assert(!"FOOFO");
        }

        tx_phase++;
    } else if (tx_phase == 6) {
        debug_printf("DONE.\n");
        exit(0);
    }

}

/******************************************************************************/
/* Test control */

static void print_usage(void)
{
    printf("Usage: bulk_netproxy (rx|tx)\n");
    exit(0);
}

int main(int argc, char *argv[])
{
    if (argc != 2) {
        print_usage();
    }
    bool is_tx = !strcmp(argv[1], "tx");
    bool is_rx = !strcmp(argv[1], "rx");
    if (!is_tx && !is_rx) {
        print_usage();
    }

    struct waitset *ws = get_default_waitset();
    debug_printf("bulk_mini: enter\n");
    if (is_tx) {
        init_sender(ws);
        debug_printf("bulk_mini: tx_init done\n");
    } else if (is_rx) {
        init_receiver(ws);
        debug_printf("bulk_mini: rx_init done\n");
    }
    while (true) {
        if (is_tx) {
            tx_process();
        }
        event_dispatch(ws);
    }
    return 0;
}

