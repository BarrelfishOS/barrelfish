/**
 * \file
 * \brief block_server client process.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_allocator.h>

#include <if/block_service_defs.h>

#include "benchmark.h"
#include "bs_connector.h"

static struct bs_connection *service;

static struct bulk_allocator tx_alloc;
static struct bulk_allocator rx_alloc;

enum bs_bench_state
{
    BS_BENCH_STATE_INIT,
    BS_BENCH_STATE_POOL_1,
    BS_BENCH_STATE_POOL_2,
    BS_BENCH_STATE_READY,
    BS_BENCH_STATE_READ,
    BS_BENCH_STATE_WRITE
};

volatile uint32_t num_buf_otherside = 0;

static uint32_t req_id = 0;

volatile enum bs_bench_state bench_state;

static inline void wait_cond(enum bs_bench_state state)
{
    struct bulk_sm_ws_item ws_list[3];

    ws_list[0].ws   = get_default_waitset();
    ws_list[1].ws   = (service ? service->tx_channel.waitset : NULL);
    ws_list[2].ws   = (service ? service->rx_channel.waitset : NULL);
    ws_list[0].next = &ws_list[1];
    ws_list[1].next = &ws_list[2];
    ws_list[2].next = NULL;

    while (bench_state != state) {
        errval_t err = bulk_sm_multiple_event_dispatch(ws_list);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "wait_for_condition: event_dispach");
        }
    }
}

/* ---------------------- bulk channel callback ------------------------------*/

static void move_recvd_cb(struct bulk_channel *channel,
                          struct bulk_buffer *buffer,
                          void *meta)
{
    errval_t err;
    struct bs_meta_data *bs = meta;

    BS_BENCH_DEBUG(" Block Received: id=[%i] data=[0x%x]", (uint32_t)bs->block_id, *(uint32_t*)buffer->address);



    char *data = buffer->address;
    for (uint32_t i = 0; i < buffer->pool->buffer_size; ++i) {
        assert(data[i] == (((bs->block_id % BS_BENCH_MAX_BLOCKS)+5) % 255));
    }

    err = bulk_channel_pass(channel, buffer, meta, BULK_CONT_NOP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not pass the buffer");
    }

    bench_state = BS_BENCH_STATE_WRITE;
}

static void buffer_recvd_cb(struct bulk_channel *channel,
                            struct bulk_buffer *buffer,
                            void *meta)
{
    BS_BENCH_DEBUG("Buffer: %p", buffer);

    bulk_alloc_return_buffer(&tx_alloc, buffer);
}

static void copy_recvd_cb(struct bulk_channel *channel,
                          struct bulk_buffer *buffer,
                          void *meta)
{
    assert(!"NYI: copy");
}

static void copy_released_cb(struct bulk_channel *channel,
                             struct bulk_buffer *buffer)
{
    assert(!"NYI: copy");
}

static struct bulk_channel_callbacks bulk_rx_cb = {
    .move_received = move_recvd_cb,
    .copy_received = copy_recvd_cb, };

static struct bulk_channel_callbacks bulk_tx_cb = {
    .copy_released = copy_released_cb,
    .buffer_received = buffer_recvd_cb, };

struct bulk_channel_callbacks *bench_get_rx_cb(void)
{
    return &bulk_rx_cb;
}

struct bulk_channel_callbacks *bench_get_tx_cb(void)
{
    return &bulk_tx_cb;
}

static void buff_passed_cb(void *arg,
                           errval_t err,
                           struct bulk_channel *channel)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed passing buffer");
        return;
    }

    num_buf_otherside++;
    if (bench_state == BS_BENCH_STATE_POOL_2
                    && num_buf_otherside == BS_BENCH_NUM_BUFS) {
        bench_state = BS_BENCH_STATE_READY;
    }
}

static void pool_assigned_cb(void *arg,
                             errval_t err,
                             struct bulk_channel *channel)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error while assigning pool");
        return;
    }
    if (bench_state == BS_BENCH_STATE_INIT) {
        BS_BENCH_DEBUG("%s", "first pool assigned\n\n");
        bench_state = BS_BENCH_STATE_POOL_1;
    } else if (bench_state == BS_BENCH_STATE_POOL_1) {
        BS_BENCH_DEBUG("%s", "second pool assigned\n\n");
        bench_state = BS_BENCH_STATE_POOL_2;
    }
}

errval_t bench_init(struct bs_connection *conn)
{
    errval_t err;

    service = conn;

    BS_BENCH_DEBUG("Allocating pools: size=%i",
                   BS_BENCH_NUM_BUFS * BS_BENCH_BUF_SIZE)

    err = bulk_alloc_init(&tx_alloc, BS_BENCH_NUM_BUFS, BS_BENCH_BUF_SIZE,
    NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "allocating buffer");
    }

    err = bulk_alloc_init(&rx_alloc, BS_BENCH_NUM_BUFS, BS_BENCH_BUF_SIZE,
    NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "allocating buffer");
    }

    bench_state = BS_BENCH_STATE_INIT;

    struct bulk_continuation cont = {
        .arg = NULL,
        .handler = pool_assigned_cb };

    BS_BENCH_DEBUG("%s", "Assigning first... ");

    err = bulk_channel_assign_pool(&service->rx_channel, rx_alloc.pool, cont);
    if (err_is_fail(err)) {
        return err;
    }

    BS_BENCH_DEBUG("%s", "Assigning second pool... ");

    err = bulk_channel_assign_pool(&service->tx_channel, tx_alloc.pool, cont);
    if (err_is_fail(err)) {
        return err;
    }
    wait_cond(BS_BENCH_STATE_POOL_2);

    BS_BENCH_DEBUG("%s", "Passing buffers");

    cont.handler = buff_passed_cb;

    for (uint32_t i = 0; i < BS_BENCH_NUM_BUFS; ++i) {
        // BS_BENCH_DEBUG("Passing buffer %u\n", i);
        struct bulk_buffer *buf = bulk_alloc_new_buffer(&rx_alloc);
        assert(buf);
        err = bulk_channel_pass(&service->rx_channel, buf, NULL, cont);
    }


    wait_cond(BS_BENCH_STATE_READY);

    BS_BENCH_DEBUG("%s", "Benchmark initiated.")
    return SYS_ERR_OK;
}

static void free_cont(void *arg, errval_t err, struct bulk_channel *channel)
{
   // bench_state = BS_BENCH_STATE_READ;
}

void bench_signal(errval_t err,
                  uint32_t seqn,
                  uint32_t req)
{
    if (err != SYS_ERR_OK) {
        BS_BENCH_DEBUG("SIGNAL: error = %i", (uint32_t)err);
    }

    // bench_state = BS_BENCH_STATE_READ;
    if (req == 4) { // usr/block_server/network_common.h:block_net_msg_type:BLOCK_NET_MSG_WRITE
        // this is messy business.
        bench_state = BS_BENCH_STATE_READ;
    }

}


static errval_t bench_do_single_run(uint32_t i)
{
    errval_t err;

    printf("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n");

    wait_cond(BS_BENCH_STATE_WRITE);

    struct bulk_buffer *buf = bulk_alloc_new_buffer(&tx_alloc);
    assert(buf);

    struct bs_meta_data meta;
    meta.block_id = i % BS_BENCH_MAX_BLOCKS;
    meta.req_id = req_id++;

    memset(buf->address, (((i % BS_BENCH_MAX_BLOCKS)+5) % 255),  buf->pool->buffer_size);

    err = bulk_channel_move(&service->tx_channel, buf, &meta,
                            MK_BULK_CONT(free_cont, NULL));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to write to channel");
    }

    /* TODO: Wait for status reply here */
    wait_cond(BS_BENCH_STATE_READ);

    err = bs_service_read(service, i % BS_BENCH_MAX_BLOCKS, 1, BULK_CONT_NOP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not read request");
    }

    return SYS_ERR_OK;
}

errval_t bench_run(void)
{
    BS_BENCH_DEBUG("Starting benchmark. NRUNS=%i", BS_BENCH_NUM_RUNS);

    bench_state = BS_BENCH_STATE_WRITE;

    for (uint32_t i=0; i < BS_BENCH_NUM_RUNS; ++i) {
        if(err_is_fail(bench_do_single_run(i))) {
            USER_PANIC("Benchmark crashed.");
            break;
        }
    }

    wait_cond(BS_BENCH_STATE_WRITE);

    BS_BENCH_DEBUG("%s", "Benchmark completed: ");
    return SYS_ERR_OK;
}
