#include <stdio.h>

#include<bulk_transfer/bulk_transfer.h>
#include<bulk_transfer/bulk_allocator.h>
#include<bulk_transfer/bulk_local.h>

#define BUFSZ 0x1000

/******************************************************************************/
/* Receiver */

static void cb_rx_bind_done(struct bulk_channel *channel,
                            errval_t err);
static void cb_rx_move_received(struct bulk_channel *channel,
                                struct bulk_buffer  *buffer,
                                void                *meta);
static void cb_rx_copy_received(struct bulk_channel *channel,
                                struct bulk_buffer  *buffer,
                                void                *meta);

static struct bulk_channel rx_channel;
static struct bulk_channel_callbacks rx_callbacks = {
    .bind_done = cb_rx_bind_done,
    .move_received = cb_rx_move_received,
    .copy_received = cb_rx_copy_received,
};


static void cb_rx_bind_done(struct bulk_channel *channel,
                            errval_t err)
{
    printf("cb_rx_bind_done\n");
}

static void cb_rx_move_received(struct bulk_channel *channel,
                                struct bulk_buffer  *buffer,
                                void                *meta)
{
    uint32_t meta_val = *((uint32_t*) meta);
    printf("cb_rx_move_received: meta=%x\n", meta_val);
    meta_val++;
    errval_t err = bulk_channel_pass(channel, buffer, &meta_val);
    assert(!err_is_fail(err));
}

static void cb_rx_copy_received(struct bulk_channel *channel,
                                struct bulk_buffer  *buffer,
                                void                *meta)
{
    printf("cb_rx_copy_received: meta=%x\n", *((uint32_t*) meta));
    errval_t err = bulk_channel_release(channel, buffer);
    assert(!err_is_fail(err));
}



static void init_receiver(struct waitset *waitset)
{
    static struct bulk_local_endpoint ep;
    errval_t err;
    struct bulk_channel_setup setup = {
        .direction = BULK_DIRECTION_RX,
        .role = BULK_ROLE_SLAVE,
        .meta_size = sizeof(uint32_t),
        .waitset = waitset,
        .trust = BULK_TRUST_FULL,
    };

    bulk_local_init_endpoint(&ep, NULL);
    err = bulk_channel_create(&rx_channel, &ep.generic, &rx_callbacks, &setup);
    assert(!err_is_fail(err));
}


/******************************************************************************/
/* Sender */

static void cb_tx_bind_done(struct bulk_channel *channel,
                            errval_t err);
static void cb_tx_buffer_received(struct bulk_channel *channel,
                                  struct bulk_buffer  *buffer,
                                  void                *meta);
static void cb_tx_copy_released(struct bulk_channel *channel,
                                struct bulk_buffer  *buffer);

static struct bulk_channel tx_channel;
static int tx_phase = 0;
static struct bulk_pool_allocator tx_allocator;
static struct bulk_channel_callbacks tx_callbacks = {
    .bind_done = cb_tx_bind_done,
    .buffer_received = cb_tx_buffer_received,
    .copy_released = cb_tx_copy_released,
};


static void cb_tx_bind_done(struct bulk_channel *channel,
                            errval_t err)
{
    printf("cb_tx_bind_done\n");
    tx_phase = 1;

    err = bulk_channel_assign_pool(channel, tx_allocator.pool);
    assert(!err_is_fail(err));
}

static void cb_tx_buffer_received(struct bulk_channel *channel,
                                  struct bulk_buffer  *buffer,
                                  void                *meta)
{
    printf("cp_tx_buffer_received: meta=%x\n", *((uint32_t*) meta));
    errval_t err = bulk_buffer_free(&tx_allocator, buffer);
    assert(!err_is_fail(err));

    tx_phase = 3;
}

static void cb_tx_copy_released(struct bulk_channel *channel,
                                struct bulk_buffer  *buffer)
{
    printf("cp_tx_copy_released\n");
    tx_phase = 5;
    errval_t err = bulk_buffer_free(&tx_allocator, buffer);
    assert(!err_is_fail(err));

}


static void init_sender(struct waitset *waitset)
{
    static struct bulk_local_endpoint ep;
    errval_t err;
    struct bulk_channel_bind_params params = {
        .waitset = waitset,
        .trust = BULK_TRUST_FULL,
    };

    err = bulk_pool_alloc_init_new(&tx_allocator, 4, BUFSZ, NULL);
    assert(!err_is_fail(err));

    bulk_local_init_endpoint(&ep, &rx_channel);
    err = bulk_channel_bind(&tx_channel, &ep.generic, &tx_callbacks, &params);
    assert(!err_is_fail(err));

}

static void tx_process(void)
{
    errval_t err;
    uint32_t meta;
    struct bulk_buffer *buffer;

    if (tx_phase == 1) {
        meta = 42;
        printf("Allocating buffer...\n");
        err = bulk_buffer_alloc(&tx_allocator, &buffer);
        assert(!err_is_fail(err));
        printf("Starting move... meta=%x\n", meta);
        err = bulk_channel_move(&tx_channel, buffer, &meta);
        assert(!err_is_fail(err));

        tx_phase++;
    } else if (tx_phase == 3) {
        meta = 44;
        printf("Allocating buffer...\n");
        err = bulk_buffer_alloc(&tx_allocator, &buffer);
        assert(!err_is_fail(err));
        printf("Starting copy... meta=%x\n", meta);
        err = bulk_channel_copy(&tx_channel, buffer, &meta);
        assert(!err_is_fail(err));

        tx_phase++;
    }
}



/******************************************************************************/
/* Test control */


int main(int argc, char *argv[])
{
    struct waitset *ws = get_default_waitset();
    printf("bulk_mini: enter\n");
    init_receiver(ws);
    printf("bulk_mini: rx_init done\n");
    init_sender(ws);
    printf("bulk_mini: tx_init done\n");
    while (true) {
        tx_process();
        event_dispatch(ws);
    }
    return 0;
}


