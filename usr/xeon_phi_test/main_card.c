/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <barrelfish/barrelfish.h>
#include <barrelfish/ump_chan.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <xeon_phi/xeon_phi_messaging.h>
#include <xeon_phi/xeon_phi_messaging_client.h>

uint32_t send_reply = 0x0;

#include "benchmark.h"

uint8_t connected = 0;

static void *card_buf;

static void *host_buf;

struct bench_bufs bufs;

static struct ump_chan uc;

static errval_t
msg_open_cb(struct capref msgframe,
            uint8_t chantype)
{
    errval_t err;

    struct frame_identity id;
    err = invoke_frame_identify(msgframe, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not identify the frame");
    }

    debug_printf("msg_open_cb | Frame base: %016lx, size=%lx\n", id.base,
                 1UL << id.bits);

    err = vspace_map_one_frame(&host_buf, 1UL << id.bits, msgframe, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not map the frame");
    }

    debug_printf("initializing ump channel\n");

#if XPHI_BENCH_CHAN_LOCATION == XPHI_BENCH_CHAN_HOST
    void *inbuf = host_buf + XPHI_BENCH_MSG_BUF_SIZE;
    void *outbuf = host_buf;
#if XPHI_BENCH_BUF_LOCATION == XPHI_BENCH_BUF_LOC_HOST
    bufs.buf = inbuf + XPHI_BENCH_MSG_BUF_SIZE;
#else
    bufs.buf = card_buf;
#endif
#else
#if XPHI_BENCH_CHAN_LOCATION == XPHI_BENCH_CHAN_CARD
    void *inbuf = card_buf + XPHI_BENCH_MSG_BUF_SIZE;
    void *outbuf = card_buf;
#if XPHI_BENCH_BUF_LOCATION == XPHI_BENCH_BUF_LOC_HOST
    bufs.buf = host_buf;
#else
    bufs.buf = inbuf + XPHI_BENCH_MSG_BUF_SIZE;
#endif
#else
    void *inbuf = card_buf;
    void *outbuf = host_buf;
#if XPHI_BENCH_BUF_LOCATION == XPHI_BENCH_BUF_LOC_HOST
    bufs.buf = host_buf + XPHI_BENCH_MSG_BUF_SIZE;
#else
    bufs.buf = card_buf + XPHI_BENCH_MSG_BUF_SIZE;
#endif
#endif
#endif

    bufs.num = XPHI_BENCH_BUF_NUM;
    bufs.buf_size = XPHI_BENCH_BUF_SIZE;

    debug_printf("[%p, %p, %p]\n", inbuf, outbuf, bufs.buf);

    err = ump_chan_init(&uc, inbuf,
    XPHI_BENCH_MSG_BUF_SIZE,
                        outbuf,
                        XPHI_BENCH_MSG_BUF_SIZE);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not initialize the channel");
    }

    connected = 0x1;

    return SYS_ERR_OK;
}

static struct xeon_phi_messaging_cb callbacks = { .open = msg_open_cb };

int
main(int argc,
     char **argv)
{
    errval_t err;

    debug_printf("Xeon Phi Test started on the card.\n");

    debug_printf("Msg Buf Size = %lx, Buf Frame Size = %lx\n",
    XPHI_BENCH_MSG_BUF_SIZE,
                 XPHI_BENCH_BUF_FRAME_SIZE);

    err = xeon_phi_messaging_service_init(&callbacks);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not init the service\n");
    }

    size_t frame_size = XPHI_BENCH_FRAME_SIZE_CARD;
    if (!frame_size) {
        frame_size = 4096;
    }

    struct capref frame;
    size_t alloced_size = 0;
    err = frame_alloc(&frame, frame_size, &alloced_size);
    assert(err_is_ok(err));
    assert(alloced_size >= frame_size);

    err = vspace_map_one_frame(&card_buf, frame_size, frame,
    NULL,
                               NULL);
    assert(err_is_ok(err));

    err = xeon_phi_messaging_service_start(XEON_PHI_MESSAGING_NO_HANDLER);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not start the service\n");
    }

    while (!connected) {
        messages_wait_and_handle_next();
    }

    char iface[30];
    snprintf(iface, 30, "xeon_phi_test.%u", XPHI_BENCH_CORE_HOST);

    debug_printf("sending open message to %s\n", iface);
    err = xeon_phi_messaging_open(0, iface, frame, XEON_PHI_CHAN_TYPE_UMP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not open channel");
    }

    do {
        err = event_dispatch_non_block(get_default_waitset());
    } while(err_no(err) == LIB_ERR_NO_EVENT);


#if XPHI_BENCH_PROCESS == XPHI_BENCH_PROCESS_CARD
    xphi_bench_start_processor(&bufs, &uc);
#else
#if XPHI_BENCH_ASYNC
    xphi_bench_start_initator_async(&bufs, &uc);
#else
    xphi_bench_start_initator_sync(&bufs, &uc);
#endif
#endif
}

