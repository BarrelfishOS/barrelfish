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
#include <xeon_phi/xeon_phi_dma_client.h>
#include "benchmark.h"

static void xphi_bench_print_settings(void)
{
    printf("Core host: %u, Core card: %u\n",
    XPHI_BENCH_CORE_HOST,
           XPHI_BENCH_CORE_CARD);
    printf("Buffer size = %lu bytes, processing runs %u\n",
    XPHI_BENCH_BUF_SIZE,
           XPHI_BENCH_PROCESS_RUNS);
    printf("Bytes per run: %lu kB\n",
           (XPHI_BENCH_NUM_RUNS * XPHI_BENCH_BUF_SIZE) / 1024);

#ifdef XPHI_BENCH_PROCESS_CARD
    printf("Processing Side:  Card\n");
#else
    printf("Processing Side:  Host\n");
#endif

#ifdef XPHI_BENCH_CHAN_CARD
#ifdef XPHI_BENCH_BUFFER_CARD
    printf("Memory Setup (Normal):     Host [  ]                      Card [ UMP | UMP | BUFFERS ] \n");
    printf("Memory Setup (Reversed):   Host [ UMP | UMP | BUFFERS ]   Card [ ] \n");
#else
    printf("Memory Setup (Normal):     Host [ BUFFERS ]               Card [ UMP | UMP ] \n");
    printf("Memory Setup (Reversed):   Host [ UMP | UMP | BUFFERS ]   Card [  ] \n");
#endif
#endif

#ifdef XPHI_BENCH_CHAN_HOST
#ifdef XPHI_BENCH_BUFFER_CARD
    printf("Memory Setup (Normal):     Host [ UMP | UMP ]             Card [ BUFFERS ] \n");
    printf("Memory Setup (Reversed):   Host [ UMP | UMP | BUFFERS ]   Card [ ] \n");
#else
    printf("Memory Setup (Normal):     Host [ BUFFERS ]   Card [ UMP | UMP ] \n");
    printf("Memory Setup (Reversed):   Host [ ]           Card [ UMP | UMP | BUFFERS ] \n");
#endif
#endif

#ifdef XPHI_BENCH_CHAN_DEFAULT
#ifdef XPHI_BENCH_BUFFER_CARD
    printf("Memory Setup (Normal):     Host [ UMP ]             Card [ UMP | BUFFERS ] \n");
    printf("Memory Setup (Reversed):   Host [ UMP | BUFFERS ]   Card [ UMP ] \n");
#else
    printf("Memory Setup (Normal):     Host [ UMP | BUFFERS ]   Card [ UMP ] \n");
    printf("Memory Setup (Reversed):   Host [ UMP ]             Card [ UMP | BUFFERS ] \n");
#endif
    printf("UMP Channel Setup (Normal):   Recv Remote, Send Local\n");
    printf("UMP Channel Setup (Reversed): Recv Local, Send Remote\n");
#endif
}

errval_t xphi_bench_memwrite(void *target)
{
    return SYS_ERR_OK;

    debug_printf("Executing local measurements\n");

    errval_t err;

    bench_init();

    cycles_t tsc_start;
    cycles_t result[4];
    uint64_t tscperus;
    bench_ctl_t *ctl;

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    debug_printf("tscperus = %lu\n", tscperus);

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 3, XPHI_BENCH_NUM_REPS);

    debug_printf("starting benchmark...\n");
    uint32_t rep_counter = 0;
    do {
        debug_printf("  > run %u of %u memwrite of %lu bytes..\n", rep_counter++,
        XPHI_BENCH_NUM_REPS,
                     XPHI_BENCH_BUF_FRAME_SIZE);

        /* using memset */
        tsc_start = rdtsc();
        memset(target, 0, XPHI_BENCH_BUF_FRAME_SIZE);
        result[0] = rdtsc() - tsc_start - bench_tscoverhead();

        /* writing in a loop*/
        volatile uint8_t *buf = target;
        tsc_start = rdtsc();
        for (uint32_t i = 0; i < XPHI_BENCH_BUF_FRAME_SIZE; ++i) {
            buf[i] = (uint8_t) 1;
        }
        result[1] = rdtsc() - tsc_start - bench_tscoverhead();

        /* reading in a while loop */
        buf = target;
        buf[XPHI_BENCH_BUF_FRAME_SIZE - 1] = 0;
        tsc_start = rdtsc();
        while (*(buf++))
            ;

        result[2] = rdtsc() - tsc_start - bench_tscoverhead();

    } while (!bench_ctl_add_run(ctl, result));

    // bench_ctl_dump_csv(ctl, "", tscperus);
    bench_ctl_dump_analysis(ctl, 0, "memset()", tscperus);
    bench_ctl_dump_analysis(ctl, 1, "forloop write", tscperus);
    bench_ctl_dump_analysis(ctl, 2, "forloop read", tscperus);
    return SYS_ERR_OK;

    return SYS_ERR_OK;
}

static volatile uint8_t dma_done;

static void dma_done_cb(xeon_phi_dma_id_t id,
                        errval_t err,
                        void *st)
{
    XPHI_BENCH_DBG("DMA request executed...\n");
    dma_done = 0x1;
}

errval_t xphi_bench_memcpy(void *dst,
                           void *src,
                           size_t size,
                           lpaddr_t pdst,
                           lpaddr_t psrc)
{

    debug_printf("Executing local measurements\n");

    errval_t err;

    bench_init();

    cycles_t tsc_start;
    cycles_t result[4];
    uint64_t tscperus;
    bench_ctl_t *ctl;

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    debug_printf("tscperus = %lu\n", tscperus);

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS, 3, XPHI_BENCH_NUM_REPS);

    debug_printf("starting benchmark...\n");
    uint32_t rep_counter = 0;
    do {
        debug_printf("  > run %u of %u memcpy of %lu bytes..\n", rep_counter++,
        XPHI_BENCH_NUM_REPS,
                     size);

        /* using memset */
        tsc_start = rdtsc();
        memcpy(dst, src, size);
        result[0] = rdtsc() - tsc_start - bench_tscoverhead();

        /* writing in a loop*/
        volatile uint64_t *bsrc = src;
        volatile uint64_t *bdst = dst;
        tsc_start = rdtsc();
        for (uint32_t i = 0; i < size / sizeof(uint64_t); ++i) {
            bdst[i] = bsrc[i];
        }
        result[1] = rdtsc() - tsc_start - bench_tscoverhead();

        struct xeon_phi_dma_info info = {
            .src = psrc,
            .dest = pdst,
            .size = size
        };

        struct xeon_phi_dma_cont cont = {
            .cb = dma_done_cb,
            .arg = NULL
        };

        /* reading in a while loop */
        dma_done = 0x0;
        tsc_start = rdtsc();
        err = xeon_phi_dma_client_start(0, &info, cont, NULL);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "could not exec the transfer");
        }
        while (!dma_done) {
            messages_wait_and_handle_next();
        }
        result[2] = rdtsc() - tsc_start - bench_tscoverhead();

    } while (!bench_ctl_add_run(ctl, result));

    // bench_ctl_dump_csv(ctl, "", tscperus);
    bench_ctl_dump_analysis(ctl, 0, "memcpy()", tscperus);
    bench_ctl_dump_analysis(ctl, 1, "forloop copy", tscperus);
    bench_ctl_dump_analysis(ctl, 2, "DMA", tscperus);
    return SYS_ERR_OK;
}

void xphi_bench_start_echo(struct bench_bufs *bufs,
                           struct ump_chan *uc)
{
    errval_t err;

    volatile struct ump_message *msg;
    volatile struct ump_message *msg_recv;

    struct ump_control ctrl;
    msg = ump_chan_get_next(uc, &ctrl);

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
        err = ump_chan_recv(uc, &msg_recv);
        if (err_is_ok(err)) {
            XPHI_BENCH_DBG("received ump message [%p]\n", msg_recv);
            msg = ump_chan_get_next(uc, &ctrl);
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

void xphi_bench_start_processor(struct bench_bufs *bufs,
                                struct ump_chan *uc)
{
    errval_t err;

    volatile struct ump_message *msg;

    uint64_t buf_idx = 0;

    struct ump_control ctrl;
    msg = ump_chan_get_next(uc, &ctrl);

    // send initiator message
    debug_printf("signal ready.\n");
    msg->data[0] = 123;
    msg->header.control = ctrl;

    debug_printf("xphi_bench_start_processor: receiving messages.\n");
#ifdef XPHI_BENCH_CHECK_STOP
    while (buf_idx != XPHI_BENCH_STOP_FLAG) {
#else
        while(true) {
#endif
        err = ump_chan_recv(uc, &msg);
        if (err_is_ok(err)) {
            buf_idx = msg->data[0];
            XPHI_BENCH_DBG("received ump message [%016lx]\n", buf_idx);
            struct bench_buf *buf = &bufs->buf[buf_idx];
            xphi_bench_fill_buffer(buf, XPHI_BENCH_PROCESS_RUNS);
            msg = ump_chan_get_next(uc, &ctrl);
            msg->data[0] = buf_idx;
            msg->header.control = ctrl;
        }
    }
    if (buf_idx == XPHI_BENCH_STOP_FLAG) {
        debug_printf("xphi_bench_start_processor: received stop flag\n");
    }
}

errval_t xphi_bench_start_initator_rtt(struct bench_bufs *bufs,
                                       struct ump_chan *uc)
{
    errval_t err;

    volatile struct ump_message *msg;

    bench_init();

    cycles_t tsc_start;
    cycles_t result;
    uint64_t tscperus;
    bench_ctl_t *ctl;

    err = sys_debug_get_tsc_per_ms(&tscperus);
    assert(err_is_ok(err));
    tscperus /= 1000;

    debug_printf("tscperus = %lu\n", tscperus);

    ctl = bench_ctl_init(BENCH_MODE_FIXEDRUNS,
                         1,
                         XPHI_BENCH_NUM_REPS * XPHI_BENCH_NUM_RUNS);

    debug_printf("waiting for ready signal\n");
    while (1) {
        err = ump_chan_recv(uc, &msg);
        if (err_is_ok(err)) {
            break;
        }
    }

    struct ump_control ctrl;

    debug_printf("starting benchmark: RTT...\n");
    uint32_t rep_counter = 0;
    do {
        if (!(rep_counter++ % XPHI_BENCH_NUM_RUNS)) {
            debug_printf("  > run %u of %u...\n",
                         rep_counter,
                         XPHI_BENCH_NUM_REPS * XPHI_BENCH_NUM_RUNS);
        }
        tsc_start = rdtsc();
        msg = ump_chan_get_next(uc, &ctrl);
        msg->header.control = ctrl;
        do {
            err = ump_chan_recv(uc, &msg);
        } while (err_is_fail(err));
        result = rdtsc();
        result = (result - tsc_start - bench_tscoverhead());
    } while (!bench_ctl_add_run(ctl, &result));

#ifdef XPHI_BENCH_CHECK_STOP
    msg = ump_chan_get_next(uc, &ctrl);
    msg->data[0] = XPHI_BENCH_STOP_FLAG;
    msg->header.control = ctrl;
#endif

    double avg_s = bench_avg(ctl->data, ctl->result_count) / tscperus;
    avg_s /= 1000000;
    xphi_bench_print_settings();
    // bench_ctl_dump_csv(ctl, "", tscperus);
    bench_ctl_dump_analysis(ctl, 0, "RTT", tscperus);

    return SYS_ERR_OK;
}

errval_t xphi_bench_start_initator_sync(struct bench_bufs *bufs,
                                        struct ump_chan *uc)
{
    errval_t err;

    volatile struct ump_message *msg;
    uint64_t buf_idx;

    bench_init();

    uint32_t n_recv = 0;

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

    struct ump_control ctrl;

    debug_printf("starting benchmark: SYNC...\n");
    uint32_t rep_counter = 0;
    do {
        uint64_t b_idx = 0;

        debug_printf("  > run %u of %u with %u moves...\n", rep_counter++,
        XPHI_BENCH_NUM_REPS,
                     XPHI_BENCH_NUM_RUNS);

        tsc_start = rdtsc();

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
                XPHI_BENCH_DBG("received message [%lu]\n", buf_idx);
                buf = &bufs->buf[buf_idx];
                uint32_t ret_count = 0;
                xphi_bench_read_buffer(buf, 1, &ret_count);
                n_recv++;
            }
        }
        result = rdtsc();
        result = result - tsc_start - bench_tscoverhead();

    } while (!bench_ctl_add_run(ctl, &result));

#ifdef XPHI_BENCH_CHECK_STOP
    msg = ump_chan_get_next(uc, &ctrl);
    msg->data[0] = XPHI_BENCH_STOP_FLAG;
    msg->header.control = ctrl;
#endif

    double avg_s = bench_avg(ctl->data, ctl->result_count) / tscperus;
    avg_s /= 1000000;
    xphi_bench_print_settings();
    // bench_ctl_dump_csv(ctl, "", tscperus);
    bench_ctl_dump_analysis(ctl, 0, "Sync Throughput", tscperus);
    printf("Average seconds: %f\n", avg_s);
    printf("Average throughput: %f GByte/s\n",
           (((double) (XPHI_BENCH_NUM_RUNS * XPHI_BENCH_BUF_SIZE)) / 1024 / 1024
            / 1024)
           / (avg_s));
    printf("Average throughput (with processing): %f GByte/s\n",
           (XPHI_BENCH_PROCESS_RUNS * ((double) (XPHI_BENCH_NUM_RUNS
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
        tsc_start = rdtsc();

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
                XPHI_BENCH_DBG("receiving message [%lu]\n", buf_idx);
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

        result = rdtsc();
        result = result - tsc_start - bench_tscoverhead();

        assert(in_transit == 0);
    } while (!bench_ctl_add_run(ctl, &result));

#ifdef XPHI_BENCH_CHECK_STOP
    msg = ump_chan_get_next(uc, &ctrl);
    msg->data[0] = XPHI_BENCH_STOP_FLAG;
    msg->header.control = ctrl;
#endif

    double avg_s = bench_avg(ctl->data, ctl->result_count) / tscperus;
    avg_s /= 1000000;
    xphi_bench_print_settings();
    // bench_ctl_dump_csv(ctl, "", tscperus);
    bench_ctl_dump_analysis(ctl, 0, "ASync Throughput", tscperus);
    printf("Average seconds: %f\n", avg_s);
    printf("Average throughput: %f GByte/s\n",
           (((double) (XPHI_BENCH_NUM_RUNS * XPHI_BENCH_BUF_SIZE)) / 1024 / 1024
            / 1024)
           / (avg_s));
    printf("Average throughput (with processing): %f GByte/s\n",
           (XPHI_BENCH_PROCESS_RUNS * ((double) (XPHI_BENCH_NUM_RUNS
                           * XPHI_BENCH_BUF_SIZE))
            / 1024 / 1024 / 1024)
           / (avg_s));

    return SYS_ERR_OK;
}

