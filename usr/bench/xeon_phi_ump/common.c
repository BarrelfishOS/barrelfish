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
#include <barrelfish/dispatch.h>
#include <barrelfish/ump_chan.h>
#include <bench/bench.h>
#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_client.h>

#include "benchmark.h"
#include "common.h"

static void *local_buf;
struct capref local_frame;
static lpaddr_t local_base;
static size_t local_frame_sz;

static void *remote_buf;
struct capref remote_frame;
static lpaddr_t remote_base;
static size_t remote_frame_sz;

struct ump_chan xphi_uc;
struct ump_chan xphi_uc_rev;

static void *inbuf;
static void *outbuf;

static void *inbuf_rev;
static void *outbuf_rev;

xphi_dom_id_t domainid;

volatile uint8_t connected = 0;

static void init_buffer(void)
{
#ifdef __k1om__
#if XPHI_BENCH_CHAN_SEPARATED
    if (disp_xeon_phi_id()) {
        debug_printf("buffer configuration: in=remote_buf; out=local_buf\n");
        debug_printf("buffer configuration (reversed): in=local_buf; out=remote_buf\n");
        inbuf = remote_buf;
        outbuf = local_buf;
        inbuf_rev = local_buf + XPHI_BENCH_MSG_CHAN_SIZE;
        outbuf_rev = remote_buf + XPHI_BENCH_MSG_CHAN_SIZE;
    } else {
        debug_printf("buffer configuration: in=remote_buf; out=local_buf\n");
        debug_printf("buffer configuration (reversed): in=local_buf; out=remote_buf\n");
        inbuf = remote_buf;
        outbuf = local_buf;
        inbuf_rev = local_buf + XPHI_BENCH_MSG_CHAN_SIZE;
        outbuf_rev = remote_buf + XPHI_BENCH_MSG_CHAN_SIZE;
    }
#else
    if (disp_xeon_phi_id()) {
        debug_printf("buffer configuration: in=local; out=local\n");
        debug_printf("buffer configuration (reversed): in=remote; out=remote\n");
        inbuf_rev = remote_buf;
        outbuf_rev = remote_buf + XPHI_BENCH_MSG_CHAN_SIZE;
        inbuf = local_buf;
        outbuf = local_buf + XPHI_BENCH_MSG_CHAN_SIZE;
    } else {
        debug_printf("buffer configuration: in=remote; out=remote\n");
        debug_printf("buffer configuration (reversed): in=local; out=local\n");
        inbuf = remote_buf + XPHI_BENCH_MSG_CHAN_SIZE;
        outbuf = remote_buf;
        inbuf_rev = local_buf + XPHI_BENCH_MSG_CHAN_SIZE;
        outbuf_rev = local_buf;
    }
#endif
#else
#if XPHI_BENCH_CHAN_SEPARATED
    debug_printf("buffer configuration: in=remote; out=local\n");
    debug_printf("buffer configuration (reversed): in=local; out=remote\n");
    inbuf = remote_buf;
    outbuf = local_buf;
    inbuf_rev = local_buf + XPHI_BENCH_MSG_CHAN_SIZE;
    outbuf_rev = remote_buf + XPHI_BENCH_MSG_CHAN_SIZE;
#else
    debug_printf("buffer configuration: in=remote; out=remote\n");
    debug_printf("buffer configuration (reversed): in=local; out=local\n");
    inbuf = remote_buf + XPHI_BENCH_MSG_CHAN_SIZE;
    outbuf = remote_buf;
    inbuf_rev = local_buf + XPHI_BENCH_MSG_CHAN_SIZE;
    outbuf_rev = local_buf;
#endif
#endif
}

void wait_for_connection(void)
{
    while (!connected) {
        messages_wait_and_handle_next();
    }
}

void alloc_local(void)
{
    errval_t err;

#ifndef __k1om__
    uint64_t minbase, maxlimit;
    ram_get_affinity(&minbase, &maxlimit);
    ram_set_affinity(XPHI_BENCH_RAM_MINBASE, XPHI_BENCH_RAM_MAXLIMIT);
#endif
    size_t alloced_size = 0;
    err = frame_alloc(&local_frame, XPHI_BENCH_MSG_FRAME_SIZE, &alloced_size);
    EXPECT_SUCCESS(err, "frame_alloc");

#ifndef __k1om__
    ram_set_affinity(minbase, maxlimit);
#endif

    struct frame_identity id;
    err = invoke_frame_identify(local_frame, &id);
    EXPECT_SUCCESS(err, "invoke_frame_identify");

    local_base = id.base;
    local_frame_sz = alloced_size;

    debug_printf("alloc_local | Frame base: %016lx, size=%lx\n", id.base,
                 1UL << id.bits);

    err =  vspace_map_one_frame(&local_buf, alloced_size, local_frame, NULL, NULL);
    EXPECT_SUCCESS(err, "vspace_map_one_frame");
}

static errval_t msg_open_cb(xphi_dom_id_t domain,
                     uint64_t usrdata,
                     struct capref msgframe,
                     uint8_t type)
{
    errval_t err;

    domainid = domain;

    struct frame_identity id;
    err = invoke_frame_identify(msgframe, &id);
    EXPECT_SUCCESS(err, "frame identify");

    debug_printf("msg_open_cb | Frame base: %016lx, size=%lx\n", id.base,
                 1UL << id.bits);

    assert((1UL << id.bits) >= XPHI_BENCH_MSG_FRAME_SIZE);

    err = vspace_map_one_frame(&remote_buf, XPHI_BENCH_MSG_FRAME_SIZE, msgframe,
                               NULL, NULL);
    EXPECT_SUCCESS(err, "vspace map frame");

    remote_frame = msgframe;
    remote_base = id.base;
    remote_frame_sz = (1UL << id.bits);

    init_buffer();

    connected = 0x1;

    debug_printf("Initializing UMP channel...\n");

    err = ump_chan_init(&xphi_uc, inbuf, XPHI_BENCH_MSG_CHAN_SIZE, outbuf,
                        XPHI_BENCH_MSG_CHAN_SIZE);
    EXPECT_SUCCESS(err, "initialize ump channel");

    err = ump_chan_init(&xphi_uc_rev, inbuf_rev, XPHI_BENCH_MSG_CHAN_SIZE, outbuf_rev,
                        XPHI_BENCH_MSG_CHAN_SIZE);
    EXPECT_SUCCESS(err, "initialize ump channel");

    return SYS_ERR_OK;
}

struct xeon_phi_callbacks callbacks = {
    .open = msg_open_cb
};
