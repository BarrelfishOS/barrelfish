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

#include <dma/xeon_phi/xeon_phi_dma.h>
#include <dma/dma_request.h>
#include <dma/client/dma_client_device.h>
#include <dma/dma_manager_client.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>

uint32_t send_reply = 0x0;

#include "benchmark.h"

uint8_t connected = 0;

static void *local_buf;
static struct capref local_frame;
static lpaddr_t local_base;
static size_t local_frame_sz;

static void *remote_buf;
static struct capref remote_frame;
static lpaddr_t remote_base;
static size_t remote_frame_sz;

static struct ump_chan uc;
static struct ump_chan uc_rev;

static void *inbuf;
static void *outbuf;

static void *inbuf_rev;
static void *outbuf_rev;

static xphi_dom_id_t domainid;

static void init_buffer_c0(void)
{
#if XPHI_BENCH_CHAN_SEPARATED
    debug_printf("buffer configuration: in=local; out=remote\n");
    debug_printf("buffer configuration (reversed): in=remote; out=local\n");
    inbuf = local_buf;
    outbuf = remote_buf;
    inbuf_rev = remote_buf + XPHI_BENCH_MSG_FRAME_SIZE;
    outbuf_rev = local_buf + XPHI_BENCH_MSG_FRAME_SIZE;
#else
    debug_printf("buffer configuration: in=remote; out=remote\n");
    debug_printf("buffer configuration (reversed): in=local; out=local\n");
    inbuf = local_buf + XPHI_BENCH_MSG_FRAME_SIZE;
    outbuf = local_buf;
    inbuf_rev = remote_buf + XPHI_BENCH_MSG_FRAME_SIZE;
    ;
    outbuf_rev = remote_buf;
#endif
}

static errval_t alloc_local(void)
{
    errval_t err;

    size_t frame_size = 0;

    frame_size = XPHI_BENCH_FRAME_SIZE_CARD;

    if (!frame_size) {
        frame_size = 4096;
    }

    debug_printf("Allocating a frame of size: %lx\n", frame_size);

    size_t alloced_size = 0;
    err = frame_alloc(&local_frame, frame_size, &alloced_size);
    assert(err_is_ok(err));
    assert(alloced_size >= frame_size);

    struct frame_identity id;
    err = invoke_frame_identify(local_frame, &id);
    assert(err_is_ok(err));
    local_base = id.base;
    local_frame_sz = alloced_size;

    err = vspace_map_one_frame(&local_buf, alloced_size, local_frame, NULL, NULL);

    return err;
}

static errval_t msg_open_cb(xphi_dom_id_t domain,
                            uint64_t usrdata,
                            struct capref msgframe,
                            uint8_t type)
{
    errval_t err;

    struct frame_identity id;
    err = invoke_frame_identify(msgframe, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not identify the frame");
    }

    debug_printf("msg_open_cb | Frame base: %016lx, size=%lx\n", id.base,
                 1UL << id.bits);

    remote_frame = msgframe;

    remote_base = id.base;

    remote_frame_sz = (1UL << id.bits);

    err = vspace_map_one_frame(&remote_buf, 1UL << id.bits, msgframe, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not map the frame");
    }

    domainid = domain;

    init_buffer_c0();

    connected = 0x1;

    return SYS_ERR_OK;
}

static struct xeon_phi_callbacks callbacks = {
    .open = msg_open_cb
};

int main(int argc,
         char **argv)
{
    errval_t err;

    debug_printf("Xeon Phi Test started on the card.\n");

    xeon_phi_client_init(disp_xeon_phi_id());

    debug_printf("Msg Buf Size = %lx, Buf Frame Size = %lx\n",
                 XPHI_BENCH_MSG_FRAME_SIZE,
                 XPHI_BENCH_BUF_FRAME_SIZE);

    xeon_phi_client_set_callbacks(&callbacks);

    err = alloc_local();
    assert(err_is_ok(err));



    while (!connected) {
        messages_wait_and_handle_next();
    }

    char iface[30];
    snprintf(iface, 30, "xphi_ump_bench.%u", XPHI_BENCH_CORE_HOST);

    debug_printf("sending open to host domain..\n");
    err = xeon_phi_client_chan_open(disp_xeon_phi_id(), domainid, 0, local_frame, 2);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not open channel");
    }

    debug_printf("Initializing UMP channel...\n");

    err = ump_chan_init(&uc, inbuf, XPHI_BENCH_MSG_FRAME_SIZE,
                        outbuf, XPHI_BENCH_MSG_FRAME_SIZE);
    err = ump_chan_init(&uc_rev, inbuf_rev, XPHI_BENCH_MSG_FRAME_SIZE,
                        outbuf_rev, XPHI_BENCH_MSG_FRAME_SIZE);

#ifdef XPHI_BENCH_INITIATOR_HOST
    delay_ms(4000);
#endif

#if XPHI_BENCH_INITIATOR_HOST
    debug_printf("---------------- normal run -----------------\n");
    xphi_bench_start_echo(NULL, &uc);
    debug_printf("---------------- reversed run -----------------\n");
    xphi_bench_start_echo(NULL, &uc_rev);
#else
#ifndef XPHI_BENCH_THROUGHPUT
    debug_printf("---------------- normal run -----------------\n");
    xphi_bench_start_initator_rtt(NULL, &uc);
    debug_printf("---------------- reversed run -----------------\n");
    xphi_bench_start_initator_rtt(NULL, &uc_rev);
#else
#ifdef XPHI_BENCH_SEND_SYNC
    debug_printf("---------------- normal run -----------------\n");
    xphi_bench_start_initator_sync(NULL, &uc);
    debug_printf("---------------- reversed run -----------------\n");
    xphi_bench_start_initator_sync(NULL-rev, &uc_rev);
#else
    debug_printf("---------------- normal run -----------------\n");
    xphi_bench_start_initator_async(NULL, &uc);
    debug_printf("---------------- reversed run -----------------\n");
    xphi_bench_start_initator_async(NULL, &uc_rev);
#endif
#endif
#endif
}

