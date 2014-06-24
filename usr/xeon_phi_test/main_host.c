/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/ump_chan.h>
#include <bench/bench.h>
#include <barrelfish/sys_debug.h>

#include <xeon_phi/xeon_phi_messaging.h>
#include <xeon_phi/xeon_phi_messaging_client.h>

#include "benchmark.h"

uint8_t connected = 0;

static void *card_buf;

static void *host_buf;

struct bench_bufs bufs;

static struct ump_chan uc;

static errval_t msg_open_cb(struct capref msgframe,
                            uint8_t chantype)
{
    errval_t err;

    struct frame_identity id;
    err = invoke_frame_identify(msgframe, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not identify the frame");
    }

    debug_printf("msg_open_cb | Frame base: %016lx, size=%lx\n",
                 id.base,
                 1UL << id.bits);

    assert((1UL << id.bits) >= XPHI_BENCH_FRAME_SIZE_CARD);

    err = vspace_map_one_frame(&card_buf, 1UL << id.bits, msgframe, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not map the frame");
    }

    debug_printf("initializing ump channel\n");

#ifdef XPHI_BENCH_CHAN_HOST
    void *inbuf = host_buf;
    void *outbuf = host_buf + XPHI_BENCH_MSG_FRAME_SIZE;
#ifdef XPHI_BENCH_BUFFER_CARD
    bufs.buf = card_buf;
#else
    bufs.buf = outbuf + XPHI_BENCH_MSG_FRAME_SIZE;
#endif
#endif

#ifdef XPHI_BENCH_CHAN_CARD
    void *inbuf = card_buf;
    void *outbuf = card_buf + XPHI_BENCH_MSG_FRAME_SIZE;
#ifdef XPHI_BENCH_BUFFER_CARD
    bufs.buf = outbuf + XPHI_BENCH_MSG_FRAME_SIZE;
#else
    bufs.buf = host_buf;
#endif
#endif

#ifdef XPHI_BENCH_CHAN_DEFAULT
    void *inbuf = host_buf;
    void *outbuf = card_buf;
#ifdef XPHI_BENCH_BUFFER_CARD
    bufs.buf = card_buf + XPHI_BENCH_MSG_FRAME_SIZE;
#else
    bufs.buf = host_buf + XPHI_BENCH_MSG_FRAME_SIZE;
#endif
#ifdef XPHI_BENCH_CHAN_REVERSED
    inbuf = card_buf;
    outbuf = host_buf;
#endif
#endif

    bufs.num = XPHI_BENCH_BUF_NUM;
    bufs.buf_size = XPHI_BENCH_BUF_SIZE;

    debug_printf("[%p, %p, %p]\n", inbuf, outbuf, bufs.buf);

    err = ump_chan_init(&uc, inbuf,
    XPHI_BENCH_MSG_FRAME_SIZE,
                        outbuf,
                        XPHI_BENCH_MSG_FRAME_SIZE);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not initialize the channel");
    }

    connected = 0x1;

    return SYS_ERR_OK;
}

static struct xeon_phi_messaging_cb callbacks = {
    .open = msg_open_cb
};

int main(int argc,
         char **argv)
{
    errval_t err;

    debug_printf("XEON PHI BENCH STARTED (HOST).\n");

    err = xeon_phi_messaging_service_init(&callbacks);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not init the service\n");
    }

    coreid_t core = XPHI_BENCH_CORE_CARD;
    char *name = "k1om/sbin/xeon_phi_test";

    err = xeon_phi_messaging_spawn(0, core, name);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not send the spawn message");
    }

    char iface[30];
    snprintf(iface, 30, "xeon_phi_test.%u", XPHI_BENCH_CORE_CARD);

    size_t frame_size = XPHI_BENCH_FRAME_SIZE_HOST;
    if (!frame_size) {
        frame_size = 4096;
    }

    debug_printf("Allocating a frame of size: 0x%lx\n", frame_size);

    struct capref frame;
    size_t alloced_size = 0;

    uint64_t minbase, maxlimit;
    ram_get_affinity(&minbase, &maxlimit);

    /* set the ram affinity to make sure we are in the correct numa node */
    ram_set_affinity(XPHI_BENCH_RAM_MINBASE, XPHI_BENCH_RAM_MAXLIMIT);
    err = frame_alloc(&frame, frame_size, &alloced_size);
    assert(err_is_ok(err));
    assert(alloced_size >= frame_size);
    ram_set_affinity(minbase, maxlimit);

    err = vspace_map_one_frame(&host_buf, frame_size, frame,
    NULL,
                               NULL);
    assert(err_is_ok(err));

    err = xeon_phi_messaging_open(0, iface, frame, XEON_PHI_CHAN_TYPE_UMP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not open channel");
    }

    err = xeon_phi_messaging_service_start(XEON_PHI_MESSAGING_NO_HANDLER);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not start the service\n");
    }

    while (!connected) {
        messages_wait_and_handle_next();
    }

#ifdef XPHI_BENCH_PROCESS_CARD
#ifndef XPHI_BENCH_THROUGHPUT
    xphi_bench_start_initator_rtt(&bufs, &uc);
#else
#ifdef XPHI_BENCH_SEND_SYNC
    xphi_bench_start_initator_sync(&bufs, &uc);
#else
    xphi_bench_start_initator_async(&bufs, &uc);
#endif
#endif
#ifdef XPHI_BENCH_BUFFER_CARD
    printf("Testing with buffer card GDDR memory\n");
#else
    printf("Testing with buffer own RAM\n");
#endif
    xphi_bench_memwrite(bufs.buf);
#else
#ifdef XPHI_BENCH_BUFFER_CARD
    printf("Testing with buffer card GDDR memory\n");
#else
    printf("Testing with buffer own RAM\n");
#endif
    xphi_bench_memwrite(bufs.buf);
#ifndef XPHI_BENCH_THROUGHPUT
    xphi_bench_start_echo(&bufs, &uc);
#else
    xphi_bench_start_processor(&bufs, &uc);
#endif
#endif

}

