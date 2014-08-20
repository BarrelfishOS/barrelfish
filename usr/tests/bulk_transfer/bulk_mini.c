#include <stdio.h>
#include <string.h>

#include<bulk_transfer/bulk_transfer.h>
#include<bulk_transfer/bulk_allocator.h>
#include<bulk_transfer/bulk_local.h>

#define BUFSZ 0x1000

static void dummy_cont_cb(void *arg, errval_t err, struct bulk_channel *channel)
{
    //printf("DUMMY CONT\n");
}

struct bulk_continuation dummy_cont = { .arg = NULL, .handler = dummy_cont_cb, };

/******************************************************************************/
/* Receiver */

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
static struct bulk_channel_callbacks rx_callbacks = { .bind_received =
    cb_rx_bind_received, .move_received = cb_rx_move_received, .copy_received =
    cb_rx_copy_received, .pool_assigned = cb_rx_pool_assigned };

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

static void init_receiver(struct waitset *waitset)
{
    static struct bulk_local_endpoint ep;
    errval_t err;
    struct bulk_channel_setup setup = { .direction = BULK_DIRECTION_RX,
                    .role = BULK_ROLE_SLAVE,
                    .meta_size = sizeof(uint32_t),
                    .waitset = waitset,
                    .trust = BULK_TRUST_FULL, };

    bulk_local_init_endpoint(&ep, NULL);
    err = bulk_channel_create(&rx_channel, &ep.generic, &rx_callbacks, &setup);
    assert(!err_is_fail(err));
}

/******************************************************************************/
/* Sender */

static void cb_tx_bind_done(void *arg,
                            errval_t err,
                            struct bulk_channel *channel);
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
static struct bulk_channel_callbacks tx_callbacks = { .buffer_received =
    cb_tx_buffer_received,
                .copy_released = cb_tx_copy_released,
                .pool_assigned = cb_tx_pool_assigned, };

static void cb_tx_pool_assign_done(void *arg,
                                   errval_t err,
                                   struct bulk_channel *channel)
{
    debug_printf("APPL: cb_tx_pool_assign_done()\n");
    assert(tx_allocator.pool == arg);
    tx_phase = 2;
}

static void cb_tx_bind_done(void *arg,
                            errval_t err,
                            struct bulk_channel *channel)
{
    debug_printf("APPL: cb_tx_bind_done\n");
    tx_phase = 1;

    struct bulk_continuation cont = { .handler = cb_tx_pool_assign_done, .arg =
        NULL };

    err = bulk_channel_assign_pool(channel, tx_allocator.pool, cont);
    assert(!err_is_fail(err));
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

static void init_sender(struct waitset *waitset)
{
    static struct bulk_local_endpoint ep;
    errval_t err;
    struct bulk_channel_bind_params params = { .waitset = waitset, .trust =
        BULK_TRUST_FULL, };
    err = bulk_alloc_init(&tx_allocator, 4, BUFSZ, NULL);
    assert(!err_is_fail(err));
    bulk_local_init_endpoint(&ep, &rx_channel);
    struct bulk_continuation cont = { .handler = cb_tx_bind_done, .arg = NULL, };
    err = bulk_channel_bind(&tx_channel, &ep.generic, &tx_callbacks, &params,
                    cont);
    assert(!err_is_fail(err));

}
static int count = 0;
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
        debug_printf("DONE.\n\n");
        if (3 > count++)
            tx_phase=2;

    }

}

/******************************************************************************/
/* Test control */

int main(int argc, char *argv[])
{
    struct waitset *ws = get_default_waitset();
    debug_printf("bulk_mini: enter\n");
    init_receiver(ws);
    debug_printf("bulk_mini: rx_init done\n");
    init_sender(ws);
    debug_printf("bulk_mini: tx_init done\n");
    while (true) {
        tx_process();
        event_dispatch(ws);
    }
    return 0;
}

