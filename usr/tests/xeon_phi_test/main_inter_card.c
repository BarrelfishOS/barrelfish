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
#include <barrelfish/dispatch.h>
#include <barrelfish/ump_chan.h>

#include <dma/xeon_phi/xeon_phi_dma.h>
#include <dma/dma_request.h>
#include <dma/client/dma_client_device.h>
#include <dma/dma_manager_client.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>
#include <xeon_phi/xeon_phi_domain.h>

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

static struct bench_bufs bufs;
static struct bench_bufs bufs_rev;

static xphi_dom_id_t domid;

static void init_buffer_c0(void)
{
#ifdef XPHI_BENCH_CHAN_HOST
    inbuf = local_buf + XPHI_BENCH_MSG_FRAME_SIZE;
    outbuf = local_buf;
    inbuf_rev = remote_buf + XPHI_BENCH_MSG_FRAME_SIZE;;
    outbuf_rev = remote_buf;
#endif

#ifdef XPHI_BENCH_CHAN_CARD
    inbuf = remote_buf;
    outbuf = remote_buf + XPHI_BENCH_MSG_FRAME_SIZE;
    inbuf_rev = local_buf;
    outbuf_rev = local_buf + XPHI_BENCH_MSG_FRAME_SIZE;
#endif

#ifdef XPHI_BENCH_CHAN_DEFAULT
    inbuf = remote_buf;
    outbuf = local_buf;
    inbuf_rev = outbuf + XPHI_BENCH_MSG_FRAME_SIZE;
    outbuf_rev = inbuf + XPHI_BENCH_MSG_FRAME_SIZE;
#endif

#ifdef XPHI_BENCH_BUFFER_CARD
    bufs.buf = remote_buf + 2 * XPHI_BENCH_MSG_FRAME_SIZE;
    bufs_rev.buf = local_buf + 2 * XPHI_BENCH_MSG_FRAME_SIZE;
#else
    bufs.buf = local_buf + 2 * XPHI_BENCH_MSG_FRAME_SIZE;
    bufs_rev.buf = remote_buf + 2 * XPHI_BENCH_MSG_FRAME_SIZE;
#endif
}

static errval_t alloc_local(void)
{
    errval_t err;

    size_t frame_size = 0;
    if (disp_xeon_phi_id() == 0) {
        frame_size = XPHI_BENCH_FRAME_SIZE_HOST;
    } else {
        frame_size = XPHI_BENCH_FRAME_SIZE_CARD;
    }

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

    domid = domain;

    struct frame_identity id;
    err = invoke_frame_identify(msgframe, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not identify the frame");
    }

    debug_printf("msg_open_cb | Frame base: %016lx, size=%lx, ud:%lx\n", id.base,
                 1UL << id.bits, usrdata);

    remote_frame = msgframe;

    remote_base = id.base;

    remote_frame_sz = (1UL << id.bits);

    err = vspace_map_one_frame(&remote_buf, remote_frame_sz, msgframe,
    NULL,
                               NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not map the frame");
    }

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

    debug_printf("Xeon Phi Test started on the card %u.\n", disp_xeon_phi_id());

    debug_printf("Msg Buf Size = %lx, Buf Frame Size = %lx\n",
    XPHI_BENCH_MSG_FRAME_SIZE,
                 XPHI_BENCH_BUF_FRAME_SIZE);

    xeon_phi_client_set_callbacks(&callbacks);

    err = xeon_phi_client_init(disp_xeon_phi_id());
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not init the service\n");
    }

    err = alloc_local();
    assert(err_is_ok(err));

    if (disp_xeon_phi_id() == 0) {
        char *iface = xeon_phi_domain_build_iface("xeon_phi_inter", 1, 2);
        err = xeon_phi_domain_blocking_lookup(iface, &domid);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "looking up domain id\n");
        }
        debug_printf("sending open message to %s on node 1\n", iface);
        err = xeon_phi_client_chan_open(1, domid, 0xcafebabe, local_frame, 2);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "could not open channel");
        }
    }

    while (!connected) {
        messages_wait_and_handle_next();
    }

    debug_printf("Initializing UMP channel...\n");

    if (disp_xeon_phi_id() != 0) {
        err = xeon_phi_client_chan_open(0, domid, 0xdeadbeef, local_frame, 2);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "could not open channel");
        }
    } else {
        debug_printf("Other node reply: %s\n", (char *) local_buf);
    }

    err = ump_chan_init(&uc, inbuf,
    XPHI_BENCH_MSG_FRAME_SIZE,
                        outbuf,
                        XPHI_BENCH_MSG_FRAME_SIZE);
    err = ump_chan_init(&uc_rev, inbuf_rev,
    XPHI_BENCH_MSG_FRAME_SIZE,
                        outbuf_rev,
                        XPHI_BENCH_MSG_FRAME_SIZE);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize UMP");
    }

    if (disp_xeon_phi_id() == 1) {
#ifndef XPHI_BENCH_THROUGHPUT
        debug_printf("---------------- normal run -----------------\n");
        xphi_bench_start_initator_rtt(&bufs, &uc);
        debug_printf("---------------- reversed run -----------------\n");
        xphi_bench_start_initator_rtt(&bufs_rev, &uc_rev);
#else
#ifdef XPHI_BENCH_SEND_SYNC
        debug_printf("---------------- normal run -----------------\n");
        xphi_bench_start_initator_sync(&bufs, &uc);
        debug_printf("---------------- reversed run -----------------\n");
        xphi_bench_start_initator_sync(&bufs_rev, &uc_rev);
#else
        debug_printf("---------------- normal run -----------------\n");
        xphi_bench_start_initator_async(&bufs, &uc);
        debug_printf("---------------- reversed run -----------------\n");
        xphi_bench_start_initator_async(&bufs_rev, &uc_rev);
#endif
#endif
    } else {
#ifndef XPHI_BENCH_THROUGHPUT
        debug_printf("---------------- normal run -----------------\n");
        xphi_bench_start_echo(&bufs, &uc);
        debug_printf("---------------- reversed run -----------------\n");
        xphi_bench_start_echo(&bufs_rev, &uc_rev);
#else
        debug_printf("---------------- normal run -----------------\n");
        xphi_bench_start_processor(&bufs, &uc);
        debug_printf("---------------- reversed run -----------------\n");
        xphi_bench_start_processor(&bufs_rev, &uc_rev);
#endif
    }

    err = dma_manager_wait_for_driver(DMA_DEV_TYPE_XEON_PHI, disp_xeon_phi_id());
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "waiting for drive");
    }

    struct dma_client_info info = {
        .type = DMA_CLIENT_INFO_TYPE_NAME,
        .device_type = DMA_DEV_TYPE_XEON_PHI,
        .args = {
            .name = XEON_PHI_DMA_SERVICE_NAME
        }
    };

    struct dma_client_device *xdev;
    err = dma_client_device_init(&info, &xdev);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize client device");
    }

    struct dma_device *dev = (struct dma_device *) xdev;

    err = dma_register_memory((struct dma_device *) dev, local_frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register memory");
    }

    err = dma_register_memory((struct dma_device *) dev, remote_frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register memory");
    }

    if (disp_xeon_phi_id() == 1) {
        debug_printf("+++++++ DMA / MEMCOPY Benchmark ++++++++\n");

        debug_printf("\n");
        debug_printf("========================================\n");
        debug_printf("\n");
        debug_printf("DMA-BENCH: LOCAL -> REMOTE \n");
        debug_printf("\n");
        debug_printf("========================================\n");
        debug_printf("\n");
        xphi_bench_memcpy(dev, remote_buf + 2 * XPHI_BENCH_MSG_FRAME_SIZE,
                          local_buf + 2 * XPHI_BENCH_MSG_FRAME_SIZE,
                          XPHI_BENCH_BUF_FRAME_SIZE / 2,
                          remote_base + 2 * XPHI_BENCH_MSG_FRAME_SIZE,
                          local_base + 2 * XPHI_BENCH_MSG_FRAME_SIZE);

        debug_printf("\n");
        debug_printf("========================================\n");
        debug_printf("\n");
        debug_printf("DMA-BENCH: REMOTE -> LOCAL \n");
        debug_printf("\n");
        debug_printf("========================================\n");
        debug_printf("\n");
        xphi_bench_memcpy(dev, local_buf + 2 * XPHI_BENCH_MSG_FRAME_SIZE,
                          remote_buf + 2 * XPHI_BENCH_MSG_FRAME_SIZE,
                          XPHI_BENCH_BUF_FRAME_SIZE / 2,
                          local_base + 2 * XPHI_BENCH_MSG_FRAME_SIZE,
                          remote_base + 2 * XPHI_BENCH_MSG_FRAME_SIZE);
    }

    debug_printf("benchmark done.");

    while (1) {
        messages_wait_and_handle_next();
    }
}

