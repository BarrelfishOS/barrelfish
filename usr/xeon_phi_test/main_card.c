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
#include <xeon_phi/xeon_phi_dma_client.h>

uint32_t send_reply = 0x0;

#include "benchmark.h"

uint8_t connected = 0;

static void *card_buf;

static struct capref card_frame;

static lpaddr_t card_base;

static size_t card_frame_sz;

static void *host_buf;

static struct capref host_frame;

static lpaddr_t host_base;

static size_t host_frame_sz;

struct bench_bufs bufs;

static struct ump_chan uc;

static void done_cb(xeon_phi_dma_id_t id,
                    errval_t err,
                    void *st)
{
    debug_printf("verifying memory range...%p\n", st);
    uint32_t *test = st;
    size_t size =
        (card_frame_sz < host_frame_sz ? card_frame_sz : host_frame_sz) >> 1;
    for (uint32_t i = 0; i < size; i += sizeof(uint32_t)) {
        if (*test != 0xCACACACA) {
            debug_printf("memory was %x, expected %x, at %lu \n", *test, 0xCACACACA, i * sizeof(uint32_t));
        }
        assert(*test == 0xCACACACA);
        test++;
    }
    debug_printf("verifying memory range: SUCCESS!\n");
}

static void do_dma_test(void)
{
    debug_printf("starting DMA test...\n");

    errval_t err;

    err = xeon_phi_dma_client_init();
    assert(err_is_ok(err));

    err = xeon_phi_dma_client_register(0, card_frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register memory");
    }

    err = xeon_phi_dma_client_register(0, host_frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register memory");
    }

    memset((void *) card_buf, 0xCA, (card_frame_sz >> 1));
    memset((void *) card_buf + (card_frame_sz >> 1), 0, (card_frame_sz >> 1));

    struct xeon_phi_dma_info info = {
        .src = card_base,
        .dest = card_base + (card_frame_sz >> 1),
        .size = (card_frame_sz >> 1)
    };

    struct xeon_phi_dma_cont cont = {
        .cb = done_cb,
        .arg = (void *) card_buf + (card_frame_sz >> 1)
    };

    debug_printf("\n\n\n++++++++++++++++++++++ Next Request: card-card\n");

    err = xeon_phi_dma_client_start(0, &info, cont, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not exec the transfer");
    }

    debug_printf("\n\n\n++++++++++++++++++++++ Next Request: card->host\n");

    cont.arg = host_buf;
    info.dest = host_base;
    info.size = (card_frame_sz < host_frame_sz ? card_frame_sz : host_frame_sz) >> 1;
    err = xeon_phi_dma_client_start(0, &info, cont, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not exec the transfer");
    }

    //  err = xeon_phi_dma_client_exec(0, &info);
    //  if (err_is_fail(err)) {
    //      USER_PANIC_ERR(err, "could not exec the transfer");
    //  }
    debug_printf("\n\n\n++++++++++++++++++++++ Next Request\n");
    info.src = card_base - 0x1000;
    err = xeon_phi_dma_client_start(0, &info, cont, NULL);
    assert(err_is_fail(err));

    debug_printf("\n\n\n++++++++++++++++++++++ Next Request\n");
    info.src = card_base;
    info.dest = card_base + card_frame_sz;
    err = xeon_phi_dma_client_start(0, &info, cont, NULL);
    assert(err_is_fail(err));

    debug_printf("\n\n\n++++++++++++++++++++++ Next Request\n");
    info.src = card_base + (card_frame_sz >> 1);
    info.dest = card_base;
    info.size += (card_frame_sz >> 1) + 0x1000;
    err = xeon_phi_dma_client_start(0, &info, cont, NULL);
    assert(err_is_fail(err));

    debug_printf("\n\n\n++++++++++++++++++++++ LOOP\n");
    while (1) {
        messages_wait_and_handle_next();
    }
}

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

    host_frame = msgframe;

    host_base = id.base;

    host_frame_sz = (1UL << id.bits);

    err = vspace_map_one_frame(&host_buf, 1UL << id.bits, msgframe, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not map the frame");
    }

#ifdef XPHI_BENCH_CHAN_HOST
    void *inbuf = host_buf + XPHI_BENCH_MSG_FRAME_SIZE;
    void *outbuf = host_buf;
#ifdef XPHI_BENCH_BUFFER_CARD
    bufs.buf = card_buf;
#else
    bufs.buf = inbuf + XPHI_BENCH_MSG_FRAME_SIZE;
#endif
#endif

#ifdef XPHI_BENCH_CHAN_CARD
    void *inbuf = card_buf + XPHI_BENCH_MSG_FRAME_SIZE;
    void *outbuf = card_buf;
#ifdef XPHI_BENCH_BUFFER_CARD
    bufs.buf = inbuf + XPHI_BENCH_MSG_FRAME_SIZE;
#else
    bufs.buf = host_buf;
#endif
#endif

#ifdef XPHI_BENCH_CHAN_DEFAULT
    void *inbuf = card_buf;
    void *outbuf = host_buf;
#ifdef XPHI_BENCH_BUFFER_CARD
    bufs.buf = card_buf + XPHI_BENCH_MSG_FRAME_SIZE;
#else
    bufs.buf = host_buf + XPHI_BENCH_MSG_FRAME_SIZE;
#endif
#ifdef XPHI_BENCH_CHAN_REVERSED
    inbuf = host_buf;
    outbuf = card_buf;
#endif
#endif

#ifdef XPHI_BENCH_BUFFER_CARD
    printf("Testing with buffer in own GDDR memory\n");
#else
    printf("Testing with buffer in host memory\n");
#endif
    xphi_bench_memwrite(bufs.buf);

    bufs.num = XPHI_BENCH_BUF_NUM;
    bufs.buf_size = XPHI_BENCH_BUF_SIZE;

    debug_printf("[%p, %p, %p]\n", inbuf, outbuf, bufs.buf);
    debug_printf("initializing ump channel\n");
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

    debug_printf("Xeon Phi Test started on the card.\n");

    debug_printf("Msg Buf Size = %lx, Buf Frame Size = %lx\n",
    XPHI_BENCH_MSG_FRAME_SIZE,
                 XPHI_BENCH_BUF_FRAME_SIZE);

    err = xeon_phi_messaging_service_init(&callbacks);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not init the service\n");
    }

    size_t frame_size = XPHI_BENCH_FRAME_SIZE_CARD;
    if (!frame_size) {
        frame_size = 4096;
    }

    debug_printf("Allocating a frame of size: %lx\n", frame_size);

    size_t alloced_size = 0;
    err = frame_alloc(&card_frame, frame_size, &alloced_size);
    assert(err_is_ok(err));
    assert(alloced_size >= frame_size);

    struct frame_identity id;
    err = invoke_frame_identify(card_frame, &id);
    assert(err_is_ok(err));
    card_base = id.base;
    card_frame_sz = alloced_size;

    err = vspace_map_one_frame(&card_buf, alloced_size, card_frame, NULL, NULL);
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

    do_dma_test();

    debug_printf("sending open message to %s\n", iface);
    err = xeon_phi_messaging_open(0, iface, card_frame, XEON_PHI_CHAN_TYPE_UMP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not open channel");
    }

    do {
        err = event_dispatch_non_block(get_default_waitset());
    } while (err_no(err) == LIB_ERR_NO_EVENT);

#ifdef XPHI_BENCH_PROCESS_CARD
    delay_ms(1000);
#endif

#ifdef XPHI_BENCH_PROCESS_CARD
#ifndef XPHI_BENCH_THROUGHPUT
    xphi_bench_start_echo(&bufs, &uc);
#else
    xphi_bench_start_processor(&bufs, &uc);
#endif
#else
#ifndef XPHI_BENCH_THROUGHPUT
    xphi_bench_start_initator_rtt(&bufs, &uc);
#else
#ifdef XPHI_BENCH_SEND_SYNC
    xphi_bench_start_initator_sync(&bufs, &uc);
#else
    xphi_bench_start_initator_async(&bufs, &uc);
#endif
#endif
#endif
}

