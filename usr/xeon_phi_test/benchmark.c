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

#include <barrelfish/barrelfish.h>
#include <barrelfish/ump_chan.h>
#include <bench/bench.h>
#include <barrelfish/sys_debug.h>

#include "benchmark.h"

extern struct bench_bufs bufs;
static struct ump_chan uc;

void xphi_bench_start_processor(void)
{
    errval_t err;

    volatile struct ump_message *msg;
    uint64_t buf_idx;
    while (1) {
        err = ump_chan_recv(&uc, &msg);
        if (err_is_ok(err)) {
            buf_idx = msg->data[0];
            XPHI_BENCH_DBG("received ump message [%016lx]\n", buf_idx);
            struct bench_buf *buf = &bufs.buf[buf_idx];
            xphi_bench_fill_buffer(buf, XPHI_BENCH_PROCESS_RUNS);
            struct ump_control ctrl;
            msg = ump_chan_get_next(&uc, &ctrl);
            msg->data[0] = buf_idx;
            msg->header.control = ctrl;
        }
    }
}

errval_t xphi_bench_start_initator_sync(void)
{
    errval_t err;

    volatile struct ump_message *msg;
    uint64_t buf_idx;

    uint32_t n_recv = 0;

    cycles_t tsc_start;
    cycles_t result;
    uint64_t tscperus;
    bench_ctl_t *ctl;

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 2, XPHI_BENCH_NUM_REPS);

    debug_printf("starting benchmark...\n");

    do {
        uint64_t b_idx = 0;

        tsc_start = rdtsc();

        struct ump_control ctrl;
        msg = ump_chan_get_next(&uc, &ctrl);
        struct bench_buf *buf = &bufs.buf[b_idx];
        xphi_bench_fill_buffer(buf, 1);

        // send initiator message
        XPHI_BENCH_DBG("sending message [%lu]\n", b_idx);
        msg->data[0] = b_idx;
        msg->header.control = ctrl;



        for (uint32_t irun = 0; irun < XPHI_BENCH_NUM_RUNS - 1; ++irun) {
            err = ump_chan_recv(&uc, &msg);
            if (err_is_ok(err)) {
                n_recv++;
                buf_idx = msg->data[0];
                XPHI_BENCH_DBG("received message [%lu]\n", buf_idx);
                assert(buf_idx == b_idx);
                b_idx = (b_idx + 1) & (bufs.num - 1);

                buf = &bufs.buf[b_idx];
                xphi_bench_fill_buffer(buf, 1);

                XPHI_BENCH_DBG("sending message [%lu]\n", b_idx);
                msg->data[0] = b_idx;
                msg->header.control = ctrl;
            }
        }

        while (n_recv < XPHI_BENCH_NUM_RUNS) {
            err = ump_chan_recv(&uc, &msg);
            if (err_is_ok(err)) {
                buf_idx = msg->data[0];
                XPHI_BENCH_DBG("received message [%lu]\n", buf_idx);
                n_recv++;
            }
        }
        result = rdtsc() - tsc_start;

    }while (!bench_ctl_add_run(ctl, &result));

    bench_ctl_dump_csv(ctl, "", tscperus);
    bench_ctl_dump_analysis(ctl, 0, "", tscperus);

    return SYS_ERR_OK;
}

errval_t xphi_bench_start_initator_async(void)
{
    volatile struct ump_message *msg;
    uint64_t buf_idx;
    uint32_t in_transit = 0;

    errval_t err;

    cycles_t tsc_start;
    cycles_t results[2];
    uint64_t tscperus;
    bench_ctl_t *ctl;

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 2, XPHI_BENCH_NUM_REPS);

    debug_printf("starting benchmark...\n");

    do {
        uint64_t b_idx = 0;
        tsc_start = rdtsc();
        struct ump_control ctrl;
        uint32_t irun = 0;
        uint32_t n_recv = 0;
        while (irun < XPHI_BENCH_NUM_RUNS) {
            if (in_transit < XPHI_BENCH_MSG_NUM) {
                msg = ump_chan_get_next(&uc, &ctrl);
                struct bench_buf *buf = &bufs.buf[b_idx];
                xphi_bench_fill_buffer(buf, 1);
                XPHI_BENCH_DBG("sending message [%lu]\n", b_idx);
                msg->data[0] = b_idx;
                msg->header.control = ctrl;
                irun++;
                in_transit++;
                b_idx = (b_idx + 1) & (bufs.num - 1);
            }

            err = ump_chan_recv(&uc, &msg);
            if (err_is_ok(err)) {
                buf_idx = msg->data[0];
                XPHI_BENCH_DBG("receiving message [%lu]\n", buf_idx);
                in_transit--;
                n_recv++;
            }
        }

        results[0] = rdtsc() - tsc_start;

        while (n_recv < XPHI_BENCH_NUM_RUNS) {
            err = ump_chan_recv(&uc, &msg);
            if (err_is_ok(err)) {
                buf_idx = msg->data[0];
                assert(buf_idx == b_idx);
                in_transit--;
                n_recv++;
            }
        }

        results[1] = rdtsc() - tsc_start;

        assert(in_transit == 0);
    } while (!bench_ctl_add_run(ctl, results));

    bench_ctl_dump_csv(ctl, "", tscperus);
    bench_ctl_dump_analysis(ctl, 0, "", tscperus);
    bench_ctl_dump_analysis(ctl, 1, "", tscperus);

    return SYS_ERR_OK;
}

