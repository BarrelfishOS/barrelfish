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

static void *local_buf;

static struct capref local_frame;

static lpaddr_t local_base;

static size_t local_frame_sz;

static void *remote_buf;

static struct capref remote_frame;

static lpaddr_t remote_base;

static size_t remote_frame_sz;

struct bench_bufs bufs;

// static struct ump_chan uc;

static void done_cb(xeon_phi_dma_id_t id,
                    errval_t err,
                    void *st)
{
    debug_printf("verifying memory range...%p\n", st);
    uint32_t *test = st;
    size_t size = (local_frame_sz < remote_frame_sz ?
                    local_frame_sz : remote_frame_sz)
                  >> 1;
    for (uint32_t i = 0; i < size; i += sizeof(uint32_t)) {
        if (*test != 0xCACACACA) {
            debug_printf("memory was %x, expected %x, at %lu \n",
                         *test,
                         0xCACACACA,
                         i * sizeof(uint32_t));
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

    err = xeon_phi_dma_client_register(0, local_frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register memory");
    }

    err = xeon_phi_dma_client_register(0, remote_frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register memory");
    }

    memset((void *) local_buf, 0xCA, (local_frame_sz >> 1));
    memset((void *) local_buf + (local_frame_sz >> 1), 0, (local_frame_sz >> 1));

    struct xeon_phi_dma_info info = {
        .src = local_base,
        .dest = local_base + (local_frame_sz >> 1),
        .size = (local_frame_sz >> 1)
    };

    struct xeon_phi_dma_cont cont = {
        .cb = done_cb,
        .arg = (void *) local_buf + (local_frame_sz >> 1)
    };

    debug_printf("\n\n\n++++++++++++++++++++++ Next Request: card-card\n");

    err = xeon_phi_dma_client_start(0, &info, cont, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not exec the transfer");
    }

    debug_printf("\n\n\n++++++++++++++++++++++ Next Request: card->host\n");

    cont.arg = remote_buf;
    info.dest = remote_base;
    info.size =
                    (local_frame_sz < remote_frame_sz ?
                                    local_frame_sz : remote_frame_sz)
                    >> 1;
    err = xeon_phi_dma_client_start(0, &info, cont, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not exec the transfer");
    }

    //  err = xeon_phi_dma_client_exec(0, &info);
    //  if (err_is_fail(err)) {
    //      USER_PANIC_ERR(err, "could not exec the transfer");
    //  }
    debug_printf("\n\n\n++++++++++++++++++++++ Next Request\n");
    info.src = local_base - 0x1000;
    err = xeon_phi_dma_client_start(0, &info, cont, NULL);
    assert(err_is_fail(err));

    debug_printf("\n\n\n++++++++++++++++++++++ Next Request\n");
    info.src = local_base;
    info.dest = local_base + local_frame_sz;
    err = xeon_phi_dma_client_start(0, &info, cont, NULL);
    assert(err_is_fail(err));

    debug_printf("\n\n\n++++++++++++++++++++++ Next Request\n");
    info.src = local_base + (local_frame_sz >> 1);
    info.dest = local_base;
    info.size += (local_frame_sz >> 1) + 0x1000;
    err = xeon_phi_dma_client_start(0, &info, cont, NULL);
    assert(err_is_fail(err));

    debug_printf("\n\n\n++++++++++++++++++++++ LOOP\n");
    while (1) {
        messages_wait_and_handle_next();
    }
}

static errval_t alloc_local(void)
{
    errval_t err;

    size_t frame_size = XPHI_BENCH_FRAME_SIZE_CARD;
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

    remote_frame = msgframe;

    remote_base = id.base;

    remote_frame_sz = (1UL << id.bits);

    err = vspace_map_one_frame(&remote_buf, 1UL << id.bits, msgframe,
    NULL,
                               NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not map the frame");
    }

    debug_printf("remote buffer: %s\n", (char *) remote_buf);

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

    err = alloc_local();
    assert(err_is_ok(err));

    err = xeon_phi_messaging_service_start(XEON_PHI_MESSAGING_NO_HANDLER);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not start the service\n");
    }

    snprintf(local_buf,
             local_frame_sz,
             "Hello, this is Node %u speaking",
             disp_xeon_phi_id());

    char iface[30];
    snprintf(iface, 30, "xeon_phi_inter.%u", 2);

    if (disp_xeon_phi_id() == 0) {
        debug_printf("sending open message to %s\n", iface);
        err = xeon_phi_messaging_open(1, iface, local_frame,
        XEON_PHI_CHAN_TYPE_UMP);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "could not open channel");
        }
    }

    do {
        err = event_dispatch_non_block(get_default_waitset());
    } while (err_no(err) == LIB_ERR_NO_EVENT);

    while (!connected) {
        messages_wait_and_handle_next();
    }

    if (disp_xeon_phi_id() != 0) {
        if (disp_xeon_phi_id() == 0) {
            debug_printf("sending open message to %s\n", iface);
            err = xeon_phi_messaging_open(0, iface, local_frame,
            XEON_PHI_CHAN_TYPE_UMP);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not open channel");
            }
        }
    }

    if (0) {
        do_dma_test();
    }

}

