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

static void *out_ptr;
static size_t out_len;

static void *in_ptr;
static size_t in_len;

static struct ump_chan uc;

static uint32_t counter = 0;

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

    void *addr;
    err = vspace_map_one_frame(&addr, 1UL << id.bits, msgframe, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not map the frame");
    }

    out_ptr = addr;
    out_len = 1UL << id.bits;

    debug_printf("msg_open_cb | msg = [%s]\n", (char *) addr);

    debug_printf("initializing ump channel\n");
    err = ump_chan_init(&uc, in_ptr, in_len, out_ptr, out_len);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not initialize the channel");
    }

    struct ump_control ctrl;
    volatile struct ump_message *msg = ump_chan_get_next(&uc, &ctrl);
                        msg->data[0] = counter;
                        msg->header.control = ctrl;

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

    coreid_t core = 2;
    char *name = "k1om/sbin/xeon_phi_test";

    err = xeon_phi_messaging_spawn(0, core, name);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not send the spawn message");
    }
    char *iface = "xeon_phi_test.2";

    struct capref frame;
    err = frame_alloc(&frame, 0x2000, NULL);
    assert(err_is_ok(err));
    void *buf;
    err = vspace_map_one_frame(&buf, 0x2000, frame, NULL, NULL);
    assert(err_is_ok(err));

    in_ptr = buf;
    in_len = 0x2000;

    snprintf(buf, 0x2000, "hello world! this is host speaking");

    err = xeon_phi_messaging_open(0, iface, frame, XEON_PHI_CHAN_TYPE_UMP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not open channel");
    }

    err = xeon_phi_messaging_service_start(XEON_PHI_MESSAGING_NO_HANDLER);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not start the service\n");
    }

    while (1) {
        volatile struct ump_message *msg;
            if (out_ptr) {
            err = ump_chan_recv(&uc, &msg);
            if (err_is_ok(err)) {
                debug_printf("received ump message [%016lx]\n", msg->data[0]);
                if ((counter++) < 10) {
                    struct ump_control ctrl;
                    msg = ump_chan_get_next(&uc, &ctrl);
                    msg->data[0] = counter;
                    msg->header.control = ctrl;
                }
            }
        }
        err = event_dispatch_non_block(get_default_waitset());
        if (err_is_fail(err)) {
            if (err_no(err) == LIB_ERR_NO_EVENT) {
                    thread_yield();
                            continue;
                        }
            USER_PANIC_ERR(err,
                           "error in event_dispatch for messages_wait_and_handle_next hack");
        }
    }
}

