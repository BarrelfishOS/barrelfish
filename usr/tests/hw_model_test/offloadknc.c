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
#include <driverkit/iommu.h>

#include <if/xomp_defs.h>


#define HLINE debug_printf("========================================================\n");
#define hline debug_printf("--------------------------------------------------------\n");
#define PRINTF(x...) debug_printf("[HW Models] " x)
#define TODO(x...) debug_printf("[HW Models] TODO: " x)

#define DATA_SIZE (1UL << 30)
#define MSG_CHANNEL_SIZE (1UL << 20)
#define MSG_FRAME_SIZE (2 * MSG_CHANNEL_SIZE)

static uint8_t finished = 0;

static void do_work_rx(struct xomp_binding *_binding, uint64_t fn, uint64_t arg,
                       uint64_t tid, uint64_t flags)
{
    errval_t err;

    hline
    PRINTF("Co-processor start doing work...\n");

    for(size_t i = 0; i < arg; i += sizeof(uint64_t)) {
        uint64_t *p = (uint64_t *)(fn + i);
        *p = *p + 1;
    }

    hline
    PRINTF("Co-processor done doing work...\n");

    err = _binding->tx_vtbl.done_notify(_binding, NOP_CONT, 0, SYS_ERR_OK);
    assert(err_is_ok(err));
}

static void bind_cb(void *st, errval_t err, struct xomp_binding *_binding)
{
    hline
    PRINTF("Bind callback.\n");

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to bind to host\n");
    }

    _binding->rx_vtbl.do_work = do_work_rx;
}

static void message_passing_init(struct dmem *msgmem)
{
    errval_t err;

    hline
    PRINTF("Initializing message passing\n");


    struct xomp_frameinfo fi = {
            .sendbase = msgmem->devaddr + MSG_CHANNEL_SIZE,
            .inbuf = (void *)(msgmem->vbase),
            .inbufsize = MSG_CHANNEL_SIZE,
            .outbuf = (void *)(msgmem->vbase + MSG_CHANNEL_SIZE),
            .outbufsize = MSG_CHANNEL_SIZE,
    };


    err = xomp_connect(&fi, bind_cb, NULL, get_default_waitset(),
                       IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to connect");
    }

}

static errval_t msg_open_cb(xphi_dom_id_t domain,
                            uint64_t usrdata,
                            struct capref msgframe,
                            uint8_t type)
{
    errval_t err;

    hline
    PRINTF("Co-processor handling msg_open_cb\n");

    struct frame_identity id;
    err = frame_identify(msgframe, &id);
    if (err_is_fail(err)) {
        return err;
    }

    PRINTF("Obtained message cap: %lx..%lx\n", id.base, id.base + id.bytes - 1);


    PRINTF("Mapping at address 0x%lx\n", usrdata);

    err = vspace_map_one_frame_fixed(usrdata, id.bytes, msgframe, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "faield to map!");
        return err;
    }


    PRINTF("Setup successfull %s\n", err_getstring(err));

    return SYS_ERR_OK;
}

static struct xeon_phi_callbacks callbacks = {
    .open = msg_open_cb
};


int main(int argc, char **argv)
{
    HLINE
    PRINTF("Co-processor starts executing...\n");
    HLINE

    /* set the connection */
    xeon_phi_client_init(disp_xeon_phi_id());

    /* set the callbacks */
    xeon_phi_client_set_callbacks(&callbacks);

    /* */
    errval_t err;

    struct cnoderef argcnref;

    argcnref = build_cnoderef(cap_argcn, CNODE_TYPE_OTHER);

    struct capref msgframe = {
            .cnode = argcnref,
            .slot = 0
    };


    struct dmem dmem;
    struct frame_identity id;
    err = frame_identify(msgframe, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to invoke frame identify\n");
    }

    PRINTF("Using messaging frame 0x%lx..0x%lx\n", id.base, id.base + id.bytes - 1);



    dmem.devaddr = id.base;
    dmem.mem = msgframe;
    dmem.size = id.bytes;

    err = vspace_map_one_frame((void **)&dmem.vbase, dmem.size, dmem.mem, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    message_passing_init(&dmem);

    PRINTF("Waiting for the commands...\n");

    while(!finished) {
        messages_wait_and_handle_next();
    }


    PRINTF("Co-processor terminated...\n");
}
