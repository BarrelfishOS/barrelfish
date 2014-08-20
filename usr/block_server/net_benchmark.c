/**
 * \file  block_server_client.c
 * \brief block server client domain
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
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/sys_debug.h>
#include <bench/bench.h>

/* includes for ip address and lwip wrappers */
#include <ipv4/lwip/inet.h>
#include <lwip/init.h>

/* bulk transfer facilities */
#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_allocator.h>

/* local includes of block server client */
#include "block_server.h"
#include "local_server.h"
#include "network_common.h"
#include "network_client.h"


/**
 * \brief Contains the connection parameters to the network block server
 */



/* ======================= run net test facility =========================== */
#if BLOCK_ENABLE_NETWORKING
#if BLOCK_BENCH_ENABLE

static struct bulk_channel *tx_chan;
static struct bulk_channel *rx_chan;

// switches for test control

volatile static uint32_t num_read = 0;
volatile static uint32_t num_written = 0;

//#define BS_TEST_DEBUG(fmt, msg...) debug_printf("%s: "fmt"\n", __func__, msg);
#define BS_TEST_DEBUG(fmt, msg...) do{}while(0);
#define BS_TEST_CTRL(fmt, msg...) debug_printf("TEST-CTRL: "fmt"\n", msg);

/* the pool allocators of the two channels */
static struct bulk_allocator allocator_tx;
static struct bulk_allocator allocator_rx;

/* zero meta */
struct bs_meta_data zero_meta;

/* state variables for the events */
static uint32_t tx_bufs_other_side = 0;
static uint32_t pools_assigned = 0;

/* conditional waiting facility */
static volatile uint32_t wait_cond = 0;

static inline void wait_for_condition(void)
{
    BS_TEST_DEBUG("%s", "Waiting for condition...\n");
    struct waitset *ws = get_default_waitset();
    while (wait_cond) {
        event_dispatch_non_block(rx_chan->waitset);
        event_dispatch_non_block(tx_chan->waitset);
        event_dispatch_non_block(ws);
        wrapper_perform_lwip_work();
    }
}

/* ------------------------ callbacks for bulk events ---------------------- */

static void pool_assigned_cb(void *arg,
                             errval_t err,
                             struct bulk_channel *channel)
{
    BS_TEST_DEBUG(" > channel=%p, success=%i", channel, (err == SYS_ERR_OK));

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to assign the pool. cannot run test");
    }

    pools_assigned++;

    wait_cond = 0;

    if (pools_assigned == 2) {
        BS_TEST_CTRL("%s", "both pools added. Signaling condition");
        wait_cond = 0;
    }
}

static void buffer_passed_cb(void *arg,
                             errval_t err,
                             struct bulk_channel *channel)
{

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to pass the buffer. ");
    }

    tx_bufs_other_side++;

    BS_TEST_DEBUG(" > channel=%p, success=%i, tx=%i", channel,
                  (err == SYS_ERR_OK), tx_bufs_other_side);

    if (tx_bufs_other_side == BLOCK_BENCH_NUMBUFS) {
        BS_TEST_CTRL("%s", "all bufs passed. Signaling condition");
        wait_cond = 0;
    }
}


errval_t testrun_handle_status(enum block_net_msg_type req,
                               uint32_t reqid,
                               enum block_net_err stats)
{
    if (stats != BLOCK_NET_ERR_OK || req != BLOCK_NET_MSG_WRITE) {
        BS_TEST_DEBUG(" > WRONG STATUS REPLY: reqid=%i", reqid);
        return SYS_ERR_OK;
    }

    return SYS_ERR_OK;
}

void testrun_bulk_buffer_received(struct bulk_channel *channel,
                                  struct bulk_buffer *buffer,
                                  void *meta)
{
    BS_TEST_DEBUG(" buffer = %p", buffer);
    num_written++;
    bulk_alloc_return_buffer(&allocator_tx, buffer);
}

void testrun_bulk_move_received(struct bulk_channel *channel,
                                struct bulk_buffer *buffer,
                                void *meta)
{
    errval_t err;

    struct bs_meta_data *bsmeta = (struct bs_meta_data *) meta;
    unsigned char *data = buffer->address;
    for (uint32_t i = 0; i < BLOCK_BENCH_BUFSIZE; ++i) {
        if (data[i] != (unsigned char) ((bsmeta->block_id + 1) % 256)) {
            BS_TEST_CTRL(" > ERROR: wrong data [%i | %i ]", data[i],
                         (unsigned char )((bsmeta->block_id + 1) % 256));
            break;
        }
    }

    num_read++;
    BS_TEST_DEBUG(" > TEST passed. value=%i", data[0]);

    err = bulk_channel_pass(channel, buffer, &zero_meta, BULK_CONT_NOP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to pass back the buffer");
    }
}

/* ------------------------ test control ----------------------------------- */
void run_test(struct bulk_channel *txc, struct bulk_channel *rxc, void *block_service)
{
    tx_chan = txc;
    rx_chan = rxc;

    BS_TEST_CTRL("%s", "test started...");

    errval_t err;

    BS_TEST_DEBUG("Allocating two pools of size %i", BLOCK_BENCH_NUMBUFS*BLOCK_BENCH_BUFSIZE);
    err = bulk_alloc_init(&allocator_tx, BLOCK_BENCH_NUMBUFS, BLOCK_BENCH_BUFSIZE, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to allocate pool.");
    }

    err = bulk_alloc_init(&allocator_rx, BLOCK_BENCH_NUMBUFS, BLOCK_BENCH_BUFSIZE, NULL);
    if (err_is_fail(err)) {
        debug_printf("failed to allocate the pool");
    }

    BS_TEST_DEBUG("%s", "Assigning pools to the channels");

    struct bulk_continuation cont = {
        .handler = pool_assigned_cb,
        .arg = NULL, };

    wait_cond = 1;

    err = bulk_channel_assign_pool(rx_chan, allocator_rx.pool,
                                   cont);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to assign pool to the RX channel.");
        debug_printf("Failed to assign pool to RX channel\n");
    }

    wait_for_condition();

    wait_cond = 1;

    err = bulk_channel_assign_pool(tx_chan, allocator_tx.pool,
                                   cont);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to assign pool to the TX channel.");
    }

    wait_for_condition();

    BS_TEST_CTRL("%s", "moving buffers over the RX channel.");

    /* setting the new callback handler */
    cont.handler = buffer_passed_cb;

    wait_cond = 1;

    struct bulk_buffer *buf;
    for (uint32_t i = 0; i < BLOCK_BENCH_NUMBUFS; ++i) {
        buf = bulk_alloc_new_buffer(&allocator_rx);
        assert(buf); /// should not fail
        err = bulk_channel_pass(rx_chan, buf, NULL, cont);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to pass the buffer");
        }
        do {
            err = event_dispatch_non_block(rx_chan->waitset);
        } while (err_is_ok(err));
    }

    wait_for_condition();

    BS_TEST_CTRL("%s", "All prepared.\n");

    BS_TEST_CTRL("%s", "Start with Benchmarks (Writing)");

    cycles_t tsc_start;
    cycles_t results[2];
    uint64_t tscperus;
    bench_ctl_t *ctl;

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 2, BLOCK_BENCH_NUMRUNS);

    struct bs_meta_data meta = {
        .block_id = 0,
        .req_id = 1 };

    do {
        BS_TEST_DEBUG("%s", ">>  Starting run");
        num_written = 0;
        tsc_start = rdtsc();
        for (uint32_t i = 0; i < BLOCK_BENCH_NUMREQUESTS; ++i) {
            meta.block_id = (i % BLOCK_COUNT);
            BS_TEST_DEBUG("Writing of block %i, data=%i",
                          (uint32_t )meta.block_id, (i + 1) % 256);
            buf = bulk_alloc_new_buffer(&allocator_tx);
            if (buf == NULL) {
                BS_TEST_DEBUG("%s", "no buffers. waiting.");
                --i;
                thread_yield();
                continue;
            }
            memset(buf->address, (i + 1) % 256, BLOCK_BENCH_BUFSIZE);
            err = bulk_channel_move(tx_chan, buf, &meta, cont);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "Failed to move block");
            }
            do {
                err = event_dispatch_non_block(tx_chan->waitset);
            } while (err_is_ok(err));
            meta.req_id++;

        }
        results[0] = rdtsc() - tsc_start;
        while (num_written < BLOCK_BENCH_NUMREQUESTS) {
            event_dispatch(tx_chan->waitset);
        }
        results[1] = rdtsc() - tsc_start;
        do {
            err = event_dispatch_non_block(tx_chan->waitset);
        } while (err_is_ok(err));
    } while (!bench_ctl_add_run(ctl, results));

    //bench_ctl_dump_csv(ctl, "", tscperus);
    bench_ctl_dump_analysis(ctl, 0, "", tscperus);
    bench_ctl_dump_analysis(ctl, 1, "", tscperus);
    BS_TEST_CTRL("%s", "Benchmark Finished");

    printf("\n\n");
    BS_TEST_CTRL("%s", "Start with Benchmarks (READING)");

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 2, BLOCK_BENCH_NUMRUNS);

    do {
        num_read = 0;
        BS_TEST_DEBUG("%s", ">>  Starting run");
        wrapper_perform_lwip_work();
        tsc_start = rdtsc();
        for (uint32_t i = 0; i < BLOCK_BENCH_NUMBATCH_REQUESTS; ++i) {
            meta.block_id = (i % BLOCK_COUNT);
            BS_TEST_DEBUG("Reading of blocks [%i, %i]", (uint32_t )meta.block_id,
                         (uint32_t )meta.block_id + BATCH_READ);
#if BLOCK_ENABLE_NETWORKING
            err = block_net_read(block_service, (uint32_t) meta.block_id,
                                 BLOCK_BENCH_READ_BATCHSIZE,
                                 meta.req_id, cont);
#else

#endif
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "Failed to move block");
            }
            meta.req_id++;
            break;
        }
        results[0] = rdtsc() - tsc_start;

        while (num_read < (BLOCK_BENCH_READ_BATCHSIZE * BLOCK_BENCH_NUMBATCH_REQUESTS)) {
            event_dispatch(rx_chan->waitset);
        }
        results[1] = rdtsc() - tsc_start;

        do {
            err = event_dispatch_non_block(rx_chan->waitset);
        } while (err_is_ok(err));
    } while (!bench_ctl_add_run(ctl, results));

    // bench_ctl_dump_csv(ctl, "", tscperus);
    bench_ctl_dump_analysis(ctl, 0, "", tscperus);
    bench_ctl_dump_analysis(ctl, 1, "", tscperus);
    BS_TEST_CTRL("%s", "Test run finished.");
}
#endif //< BLOCK_BENCH_ENABLE
#endif //< RUN_NET

