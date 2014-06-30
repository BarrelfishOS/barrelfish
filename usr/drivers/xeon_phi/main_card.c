/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

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
#include <barrelfish/barrelfish.h>

#include <if/xeon_phi_messaging_defs.h>

#include <xeon_phi/xeon_phi.h>

#include "xeon_phi_internal.h"
#include "messaging.h"
#include "service.h"
#include "sysmem_caps.h"
#include "dma/dma.h"
#include "smpt.h"

static struct xeon_phi xphi;

struct xeon_phi_messaging_cb msg_cb = {
    .open_card = messaging_send_open_to_xphi,
    .open_iface = messaging_send_open,
    .spawn_card = messaging_send_spawn_to_xphi,
    .spawn = messaging_send_spawn
};

static struct capref mmio_cap = {
    .slot = TASKCN_SLOT_IO
};

static struct capref sysmem_cap = {
    .slot = TASKCN_SLOT_SYSMEM
};

static struct capref host_cap;

static errval_t map_mmio_space(struct xeon_phi *phi)
{
    errval_t err;
    void *mmio;

    struct frame_identity id;
    err = invoke_frame_identify(mmio_cap, &id);
    if (err_is_fail(err)) {
        return err;
    }

    err = vspace_map_one_frame(&mmio, (1UL << id.bits), mmio_cap, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    XDEBUG("mapped mmio register space @ [%p]\n", mmio);

    phi->mmio.bits = id.bits;
    phi->mmio.vbase = (lvaddr_t) mmio;
    phi->mmio.cap = mmio_cap;
    phi->mmio.pbase = id.base;
    phi->mmio.length = (1UL << id.bits);

    return SYS_ERR_OK;
}

int main(int argc,
         char *argv[])
{
    debug_printf("Xeon Phi module started on node [%u].\n", disp_xeon_phi_id());

    errval_t err;
    lpaddr_t host_base;
    genvaddr_t offset;

    mmio_cap.cnode = cnode_task;
    sysmem_cap.cnode = cnode_task;

    assert(!capref_is_null(mmio_cap));
    assert(!capref_is_null(sysmem_cap));

    xphi.is_client = 0x1;
    xphi.id = disp_xeon_phi_id();

    for (uint32_t i = 0; i < XEON_PHI_NUM_MAX; ++i) {
        xphi.topology[i].id = i;
        xphi.topology[i].local = &xphi;
    }

    XDEBUG("Initializing system memory cap manager...\n");
    err = sysmem_cap_manager_init(sysmem_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize the cap manager.\n");
    }

    err = map_mmio_space(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not map the mmio space");
    }

    err = dma_init(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize the dma engine.\n");
    }

    err = smpt_init(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize the SMTP.\n");
    }


    //dma_impl_test(&xphi);

    host_base = strtol(argv[0], NULL, 16);
    offset = host_base;

    XMESSAGING_DEBUG("Getting the host messaging cap...\n");
    err = sysmem_cap_request(host_base, XEON_PHI_MSG_INIT_BITS, &host_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not obtain the system messsaging cap\n");
    }

    messaging_init(&xphi, host_cap);
    err = xeon_phi_messaging_service_init(&msg_cb);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the messaging service");
    }

    err = xeon_phi_messaging_service_start_phi(0);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not export the service");
    }

    XMESSAGING_DEBUG("Start polling for messages...\n");
    while (1) {
        uint8_t idle = 0x1;
        err = messaging_poll(&xphi);
        idle = idle && (err_no(err) == LIB_ERR_NO_EVENT);
        err = dma_poll_channels(&xphi);
        idle = idle && (err_no(err) == XEON_PHI_ERR_DMA_IDLE);
        err = event_dispatch_non_block(get_default_waitset());
        if (err_is_fail(err)) {
            if (err_no(err) == LIB_ERR_NO_EVENT && idle) {
                thread_yield();
                continue;
            }
            if (err_no(err) != LIB_ERR_NO_EVENT) {
                USER_PANIC_ERR(err, "msg loop");
            }
        }
    }

    XDEBUG("Messaging loop terminated...\n");
    return 0;
}
