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

    err = vspace_map_one_frame(&host_buf, 1UL << id.bits, msgframe, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not map the frame");
    }





    debug_printf("initializing ump channel\n");

    #if XPHI_BENCH_CHAN_LOCATION == XPHI_BENCH_CHAN_HOST
        void *inbuf = host_buf;
        void *outbuf = host_buf + XPHI_BENCH_MSG_BUF_SIZE;
    #if XPHI_BENCH_BUF_LOCATION == XPHI_BENCH_BUF_LOC_HOST
        bufs.buf = outbuf + XPHI_BENCH_MSG_BUF_SIZE;
    #else
        bufs.buf = card_buf;
    #endif
    #else
    #if XPHI_BENCH_CHAN_LOCATION == XPHI_BENCH_CHAN_CARD
        void *inbuf = card_buf;
        void *outbuf = card_buf + XPHI_BENCH_MSG_BUF_SIZE;
    #if XPHI_BENCH_BUF_LOCATION == XPHI_BENCH_BUF_LOC_HOST
        bufs.buf = host_buf;
    #else
        bufs.buf = outbuf + XPHI_BENCH_MSG_BUF_SIZE;
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

static struct xeon_phi_messaging_cb callbacks = {
    .open = msg_open_cb
};

int main(int argc,
         char **argv)
{
    errval_t err;

    debug_printf("Xeon Phi Test started on the card.\n");

    err = xeon_phi_messaging_service_init(&callbacks);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not init the service\n");
    }

    char iface[30];
    snprintf(iface, 30, "xeon_phi_test.%u", XPHI_BENCH_CORE_HOST);

    struct capref frame;
    size_t alloced_size = 0;
    err = frame_alloc(&frame, XPHI_BENCH_FRAME_SIZE_CARD, &alloced_size);
    assert(err_is_ok(err));
    assert(alloced_size >= XPHI_BENCH_FRAME_SIZE_CARD);

    err = vspace_map_one_frame(&card_buf,
                               XPHI_BENCH_FRAME_SIZE_CARD,
                               frame,
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

    #if XPHI_BENCH_PROCESS == XPHI_BENCH_PROCESS_CARD
        xphi_bench_start_processor();
    #else
    #if XPHI_BENCH_ASYNC
        xphi_bench_start_initator_async();
    #else
        xphi_bench_start_initator_sync();
    #endif
#endif
}

