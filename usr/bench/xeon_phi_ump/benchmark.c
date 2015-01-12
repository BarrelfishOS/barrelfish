/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/ump_chan.h>
#include <bench/bench.h>
#include <barrelfish/sys_debug.h>
#include <dma/dma.h>
#include <dma/dma_request.h>
#include <dma/client/dma_client_device.h>
#include <dma/dma_manager_client.h>

#include "benchmark.h"


static inline cycles_t calculate_time(cycles_t tsc_start,
                                      cycles_t tsc_end)
{
    cycles_t result;
    if (tsc_end < tsc_start) {
        result = (LONG_MAX - tsc_start) + tsc_end - bench_tscoverhead();
    } else {
        result = (tsc_end - tsc_start - bench_tscoverhead());
    }
    return result;
}


void xphi_bench_start_echo(struct ump_chan *chan)
{
    errval_t err;

    volatile struct ump_message *msg;
    volatile struct ump_message *msg_recv;

    struct ump_control ctrl;
    msg = ump_chan_get_next(chan, &ctrl);

    // send initiator message
    debug_printf("signal ready.\n");
    msg->data[0] = 123;
    msg->header.control = ctrl;

    debug_printf("xphi_bench_start_echo: receiving messages.\n");
#ifdef XPHI_BENCH_CHECK_STOP
    uint64_t data = 0x0;
    while (data != XPHI_BENCH_STOP_FLAG) {
#else
    while(true) {
#endif
        err = ump_chan_recv(chan, &msg_recv);
        if (err_is_ok(err)) {
            XPHI_BENCH_DBG("received ump message [%p]\n", msg_recv);
            msg = ump_chan_get_next(chan, &ctrl);
            msg->header.control = ctrl;
#ifdef XPHI_BENCH_CHECK_STOP
            data = msg_recv->data[0];
#endif
        }
    }
    if (data == XPHI_BENCH_STOP_FLAG) {
        debug_printf("xphi_bench_start_echo: received stop flag.\n");
    }
}


errval_t xphi_bench_start_initator_rtt(struct ump_chan *chan)
{
    errval_t err;
    cycles_t tsc_start, tsc_end;
    cycles_t result;
    uint64_t tscperus;
    bench_ctl_t *ctl;

    volatile struct ump_message *msg;

    bench_init();

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, XPHI_BENCH_RTT_NUM_ROUNDS);

    debug_printf("RTT benchmark: waiting for ready signal.\n");
    while (1) {
        err = ump_chan_recv(chan, &msg);
        if (err_is_ok(err)) {
            break;
        }
    }

    struct ump_control ctrl;

    debug_printf("Starting RTT benchmark tsc/us=%lu\n", tscperus);
    uint32_t rep_counter = 0;
    do {
        if (!(rep_counter++ % (XPHI_BENCH_RTT_NUM_ROUNDS / 10))) {
            debug_printf("  > run %u of %u...\n", rep_counter,
                         XPHI_BENCH_RTT_NUM_ROUNDS);
        }
        tsc_start = bench_tsc();
        msg = ump_chan_get_next(chan, &ctrl);
        msg->header.control = ctrl;
        do {
            err = ump_chan_recv(chan, &msg);
        } while (err_is_fail(err));
        tsc_end = bench_tsc();
        result = calculate_time(tsc_start, tsc_end);

    } while (!bench_ctl_add_run(ctl, &result));

#ifdef XPHI_BENCH_CHECK_STOP
    msg = ump_chan_get_next(chan, &ctrl);
    msg->data[0] = XPHI_BENCH_STOP_FLAG;
    msg->header.control = ctrl;
#endif
    bench_ctl_dump_analysis(ctl, 0, "RTT", tscperus);

    return SYS_ERR_OK;
}

#if 0
errval_t xphi_bench_start_initator_sync(struct bench_bufs *bufs,
                                        struct ump_chan *uc)
{
    errval_t err;

    cycles_t tsc_start, tsc_end;
    cycles_t result;
    uint64_t tscperus;
    bench_ctl_t *ctl;

    volatile struct ump_message *msg;
    uint64_t buf_idx;

    bench_init();

    uint32_t n_recv = 0;

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, XPHI_BENCH_NUM_REPS);

    debug_printf("Sync Throughput Benchmark: waiting for ready signal...\n");
    while (1) {
        err = ump_chan_recv(uc, &msg);
        if (err_is_ok(err)) {
            break;
        }
    }

    struct ump_control ctrl;

    debug_printf("Starting sync throughput benchmark. tsc/us=%lu\n", tscperus);
    uint32_t rep_counter = 0;
    do {
        uint64_t b_idx = 0;

        debug_printf("  > run %u of %u with %u moves...\n", rep_counter++,
        XPHI_BENCH_NUM_REPS,
                     XPHI_BENCH_NUM_RUNS);

        tsc_start = bench_tsc();

        msg = ump_chan_get_next(uc, &ctrl);
        struct bench_buf *buf = &bufs->buf[b_idx];
        xphi_bench_fill_buffer(buf, 1);

        // send initiator message
        XPHI_BENCH_DBG("sending message [%lu]\n", b_idx);
        msg->data[0] = b_idx;
        msg->header.control = ctrl;
        n_recv = 0;
        for (uint32_t irun = 0; irun < (XPHI_BENCH_NUM_RUNS - 1); ++irun) {
            do {
                err = ump_chan_recv(uc, &msg);
            } while (err_is_fail(err));

            n_recv++;
            buf_idx = msg->data[0];
            uint32_t ret_count = 0;
            buf = &bufs->buf[b_idx];
            xphi_bench_read_buffer(buf, 1, &ret_count);
            XPHI_BENCH_DBG("received message [%lu]\n", buf_idx);
            assert(buf_idx == b_idx);
            b_idx = (b_idx + 1) & (bufs->num - 1);

            buf = &bufs->buf[b_idx];
            xphi_bench_fill_buffer(buf, 1);

            XPHI_BENCH_DBG("sending message [%lu]\n", b_idx);
            msg = ump_chan_get_next(uc, &ctrl);
            assert(msg);
            msg->data[0] = b_idx;
            msg->header.control = ctrl;
        }

        while (n_recv < XPHI_BENCH_NUM_RUNS) {
            err = ump_chan_recv(uc, &msg);
            if (err_is_ok(err)) {
                buf_idx = msg->data[0];
                XPHI_BENCH_DBG("received message [%"PRIu64"]\n", buf_idx);
                buf = &bufs->buf[buf_idx];
                uint32_t ret_count = 0;
                xphi_bench_read_buffer(buf, 1, &ret_count);
                n_recv++;
            }
        }
        tsc_end = bench_tsc();
        result = calculate_time(tsc_start, tsc_end);
    } while (!bench_ctl_add_run(ctl, &result));

#ifdef XPHI_BENCH_CHECK_STOP
    msg = ump_chan_get_next(uc, &ctrl);
    msg->data[0] = XPHI_BENCH_STOP_FLAG;
    msg->header.control = ctrl;
#endif

    double avg_s = bench_avg(ctl->data, ctl->result_count) / tscperus;
    avg_s /= 1000000;

// bench_ctl_dump_csv(ctl, "", tscperus);
    bench_ctl_dump_analysis(ctl, 0, "Sync Throughput", tscperus);
    printf("Average seconds: %f\n", avg_s);
    printf("Average throughput: %f GByte/s\n",
           (((double) (XPHI_BENCH_NUM_RUNS * XPHI_BENCH_BUF_SIZE)) / 1024 / 1024
            / 1024)
           / (avg_s));
    printf("Average throughput (with processing): %f GByte/s\n",
           (XPHI_BENCH_NUM_RUNS * ((double) (XPHI_BENCH_NUM_RUNS
                           * XPHI_BENCH_BUF_SIZE))
            / 1024 / 1024 / 1024)
           / (avg_s));

    return SYS_ERR_OK;
}

errval_t xphi_bench_start_initator_async(struct bench_bufs *bufs,
                                         struct ump_chan *uc)
{
    volatile struct ump_message *msg;
    uint64_t buf_idx;
    uint32_t in_transit = 0;

    errval_t err;

    bench_init();

    cycles_t tsc_start;
    cycles_t result;
    uint64_t tscperus;
    bench_ctl_t *ctl;

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    debug_printf("tscperus = %lu\n", tscperus);

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 1, XPHI_BENCH_NUM_REPS);

    debug_printf("waiting for ready signal\n");
    while (1) {
        err = ump_chan_recv(uc, &msg);
        if (err_is_ok(err)) {
            break;
        }
    }

    debug_printf("starting benchmark ASYNC...\n");

    struct ump_control ctrl;

    uint32_t rep_counter = 0;
    do {
        uint64_t b_idx = 0;
        debug_printf("  > run %u of %u with %u moves...\n", rep_counter++,
        XPHI_BENCH_NUM_REPS,
                     XPHI_BENCH_NUM_RUNS);
        tsc_start = bench_tsc();

        uint32_t irun = 0;
        uint32_t n_recv = 0;
        struct bench_buf *buf;
        while (irun < XPHI_BENCH_NUM_RUNS) {
            if (in_transit < XPHI_BENCH_MSG_NUM) {
                msg = ump_chan_get_next(uc, &ctrl);
                if (!msg) {
                    continue;
                }
                buf = &bufs->buf[b_idx];
                xphi_bench_fill_buffer(buf, 1);
                XPHI_BENCH_DBG("sending message [%lu] %p\n", b_idx, msg);
                msg->data[0] = b_idx;
                msg->header.control = ctrl;
                irun++;
                in_transit++;
                b_idx = (b_idx + 1) & (bufs->num - 1);
            }

            err = ump_chan_recv(uc, &msg);
            if (err_is_ok(err)) {
                buf_idx = msg->data[0];
                XPHI_BENCH_DBG("receiving message [%"PRIu64"]\n", buf_idx);
                buf = &bufs->buf[buf_idx];
                uint32_t ret_count = 0;
                xphi_bench_read_buffer(buf, 1, &ret_count);
                in_transit--;
                n_recv++;
            }
        }

        while (n_recv < XPHI_BENCH_NUM_RUNS) {
            err = ump_chan_recv(uc, &msg);
            if (err_is_ok(err)) {
                buf_idx = msg->data[0];
                buf = &bufs->buf[buf_idx];
                uint32_t ret_count = 0;
                XPHI_BENCH_DBG("receiving message [%lu]\n", buf_idx);
                xphi_bench_read_buffer(buf, 1, &ret_count);
                in_transit--;
                n_recv++;
            }
        }

        result = bench_tsc();
        if (result - tsc_start > bench_tscoverhead()) {
            debug_printf("%lu %lu", result - tsc_start, bench_tscoverhead());
        }
        if (result < tsc_start) {
            result = (LONG_MAX - tsc_start) + result - bench_tscoverhead();
        } else {
            result = (result - tsc_start - bench_tscoverhead());
        }

        assert(in_transit == 0);
    } while (!bench_ctl_add_run(ctl, &result));

#ifdef XPHI_BENCH_CHECK_STOP
    msg = ump_chan_get_next(uc, &ctrl);
    msg->data[0] = XPHI_BENCH_STOP_FLAG;
    msg->header.control = ctrl;
#endif

    double avg_s = bench_avg(ctl->data, ctl->result_count) / tscperus;
    avg_s /= 1000000;

// bench_ctl_dump_csv(ctl, "", tscperus);
    bench_ctl_dump_analysis(ctl, 0, "ASync Throughput", tscperus);
    printf("Average seconds: %f\n", avg_s);
    printf("Average throughput: %f GByte/s\n",
           (((double) (XPHI_BENCH_NUM_RUNS * XPHI_BENCH_BUF_SIZE)) / 1024 / 1024
            / 1024)
           / (avg_s));
    printf("Average throughput (with processing): %f GByte/s\n",
           (XPHI_BENCH_NUM_RUNS * ((double) (XPHI_BENCH_NUM_RUNS
                           * XPHI_BENCH_BUF_SIZE))
            / 1024 / 1024 / 1024)
           / (avg_s));

    return SYS_ERR_OK;
}

#endif
