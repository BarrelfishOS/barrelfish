#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include "common.h"

#define USE_DEFWAITSET 1

#define NO_COPY_MOVE_BACK 1
#define NO_COPY_MOVE_BACK_WITH_COPY 0

#define DEBUG(x...) do {} while (0)
//#define DEBUG(x...) debug_printf("echo: " x)

#define BUFSZ 0x1000
#define NUMBUFS 500

static struct bulk_allocator txalloc;

static uint32_t ctrl_value = 0;

static volatile uint8_t wait_flag = 0;

static void panic_handler(void *arg, errval_t err, struct bulk_channel *chan)
{
    expect_success(err);
}

static void wait_handler(void *arg, errval_t err, struct bulk_channel *chan)
{
    if (!err_is_ok(err)) {
        err_print_calltrace(err);
    }
    expect_success(err);
    wait_flag = 1;
}

static struct bulk_continuation panic_cont = {
    .handler = panic_handler,
    .arg = NULL, };

static struct bulk_continuation wait_cont = {
    .handler = wait_handler,
    .arg = NULL, };

static struct bulk_channel rxc, txc;

static errval_t cb_pool_assigned(struct bulk_channel *channel,
                                 struct bulk_pool *pool)
{
    if (channel == &rxc) {
        debug_printf("pool_assigned: RX %p [%d,%d,%d]\n", pool,
                     pool->id.machine, pool->id.dom, pool->id.local);
        //there is a race condition between the two channels, so we have to check it here
        while (txc.state != BULK_STATE_CONNECTED) {
            event_dispatch(txc.waitset);
        }
        wait_flag = 0;
        if (!is_no_copy) {
            expect_success(bulk_channel_assign_pool(&txc, pool, wait_cont));
            //XXX: there is still a possible race condition, if we are in receive master mode,
            //but in that case, we don't expect to get a pool over this channel anyway
            while (!wait_flag)
                event_dispatch(txc.waitset);       //wait until pool is assigned
        }
    } else {
        debug_printf("pool_assigned: TX %p [%d,%d,%d]\n", pool,
                     pool->id.machine, pool->id.dom, pool->id.local);
    }

    if (is_no_copy) {
        struct bulk_pool_constraints pool_constraints = {
            .range_min = 0,
            .range_max = 0,
            .alignment = 0,
            .trust = txc.trust, };
        expect_success(bulk_alloc_init(&txalloc, NUMBUFS, BUFSZ, &pool_constraints));
        DEBUG("TX Pool alloc: %p\n", txalloc.pool);

        wait_flag = 0;
        expect_success(bulk_channel_assign_pool(&txc, txalloc.pool, wait_cont));
        while (!wait_flag)
            event_dispatch(txc.waitset);       //wait until pool is assigned
    }

    return SYS_ERR_OK;
}

static void cb_move_received(struct bulk_channel *channel,
                             struct bulk_buffer *buffer,
                             void *meta)
{
    static unsigned count = 0;
    DEBUG("move_received: %d b->p=%p\n", count, buffer->pool);
    count++;
    ctrl_value += *((uint32_t *) buffer->address);
    assert(channel == &rxc);
    if (is_no_copy) {
        if (NO_COPY_MOVE_BACK) {
            struct bulk_buffer *reply = bulk_alloc_new_buffer(&txalloc);
            assert(reply);
            if (NO_COPY_MOVE_BACK_WITH_COPY) {
                memcpy(reply->address, buffer->address,
                       buffer->pool->buffer_size);
            }
            *((uint32_t *) reply->address) = *((uint32_t *) buffer->address) +1;
            expect_success(bulk_channel_move(&txc, reply, meta, panic_cont));
        }
        expect_success(bulk_channel_pass(&rxc, buffer, meta, panic_cont));
    } else {
        *((uint32_t *) buffer->address) = *((uint32_t *) buffer->address) +1;
        expect_success(bulk_channel_move(&txc, buffer, meta, panic_cont));
    }
}

static void cb_buffer_received(struct bulk_channel *channel,
                               struct bulk_buffer *buffer,
                               void *meta)
{
    static unsigned count = 0;
    DEBUG("buffer_received: %d b->p=%p\n", count, buffer->pool);
    count++;
    assert(channel == &txc);
    if (is_no_copy) {
        expect_success(bulk_alloc_return_buffer(&txalloc, buffer));
    } else {
        expect_success(bulk_channel_pass(&rxc, buffer, meta, panic_cont));
    }
}

static void init(void)
{
    static struct bulk_allocator rxalloc;
    struct bulk_buffer *buf;
    size_t i;
    debug_printf("init: enter\n");

    if (rxc.role == BULK_ROLE_MASTER) {
        // If we're in receive master mode, we need to allocate and pass buffers
        //set the trust level we want in our pool from the start
        struct bulk_pool_constraints pool_constraints = {
            .range_min = 0,
            .range_max = 0,
            .alignment = 0,
            .trust = rxc.trust, };
        expect_success(bulk_alloc_init(&rxalloc, NUMBUFS, BUFSZ, &pool_constraints));
        DEBUG("RX Pool alloc: %p\n", rxalloc.pool);

        wait_flag = 0;
        expect_success(bulk_channel_assign_pool(&rxc, rxalloc.pool, wait_cont));
        while (!wait_flag)
            event_dispatch(rxc.waitset);

        wait_flag = 0;
        expect_success(bulk_channel_assign_pool(&txc, rxalloc.pool, wait_cont));
        while (!wait_flag)
            event_dispatch(txc.waitset);

        for (i = 0; i < NUMBUFS; i++) {
            buf = bulk_alloc_new_buffer(&rxalloc);
            assert(buf != NULL);
            expect_success(bulk_channel_pass(&rxc, buf, NULL, panic_cont));
        }
    }
    debug_printf("init: done\n");
}

static struct bulk_channel_callbacks cb = {
    .bind_received = cb_bind_received,
    .pool_assigned = cb_pool_assigned,
    .move_received = cb_move_received,
    .buffer_received = cb_buffer_received, };

int main(int argc, char *argv[])
{
    struct waitset *ws;
#if !USE_DEFWAITSET
    struct waitset l_ws;
    waitset_init(&l_ws);
    ws = &l_ws;
#else
    ws = get_default_waitset();
#endif

    bool rx_done = false, tx_done = false;

    debug_printf("bulk echo service starting\n");
    assert(argc == 3);
    debug_printf("Initialzing RX channel... [%s]\n", argv[1]);
    initialize_channel(argv[1], &rxc, &cb, ws, BULK_DIRECTION_RX, BUFSZ, 0,
                       &rx_done);
    debug_printf("Initialzing TX channel... [%s]\n", argv[2]);
    initialize_channel(argv[2], &txc, &cb, ws, BULK_DIRECTION_TX, BUFSZ, 0,
                       &tx_done);

    printf("Benchmark Server Ready!\n");
    while (!rx_done || !tx_done) {
        event_dispatch(ws);
    }

    init();
    while (1) {
        event_dispatch(ws);
    }

    return 0;
}

