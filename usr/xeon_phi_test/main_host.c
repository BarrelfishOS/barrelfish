/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <barrelfish/barrelfish.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>


#include <xeon_phi/xeon_phi_messaging.h>
#include <xeon_phi/xeon_phi_messaging_client.h>

static errval_t msg_open_cb(struct capref msgframe,
                            uint8_t chantype)
{
    errval_t err;

    struct frame_identity id;
    err = invoke_frame_identify(msgframe, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not identify the frame");
    }

    debug_printf("msg_open_cb | Frame base: %016lx, size=%lx\n", id.base, 1UL << id.bits);

    void *addr;
    err = vspace_map_one_frame(&addr, 1UL << id.bits, msgframe, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not map the frame");
    }

    debug_printf("msg_open_cb | msg = [%s]\n", (char *)addr);

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

    err = xeon_phi_messaging_spawn(core, name);
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

    snprintf(buf, 0x2000, "hello world! this is host speaking");

    err = xeon_phi_messaging_open(iface, frame, XEON_PHI_CHAN_TYPE_UMP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not open channel");
    }


    err = xeon_phi_messaging_service_start(XEON_PHI_MESSAGING_START_HANDLER);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not start the service\n");
    }
}

