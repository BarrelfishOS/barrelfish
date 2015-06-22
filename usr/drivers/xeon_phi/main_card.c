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
#include <barrelfish/dispatch.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <octopus/octopus.h>

#include <xeon_phi/xeon_phi.h>

#include "xeon_phi_internal.h"
#include "interphi.h"
#include "dma_service.h"
#include "sysmem_caps.h"
#include "smpt.h"
#include "xphi_service.h"

static struct xeon_phi xphi;

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

/*
 * -------------------------------------------------------------------------------
 * event loop
 * -------------------------------------------------------------------------------
 */

/**
 * \brief handles events on the waitset and polls for completed DMA transfers
 *        and new data on the serial line (host only)
 *
 * \param do_yield if set, yield thread if no event was discovered
 *
 * \return SYS_ERR_OK if an event was handled
 *         LIB_ERR_NO_EVENT if there was no evetn
 */
errval_t xeon_phi_event_poll(uint8_t do_yield)
{
    errval_t err;

    uint8_t idle = 0x1;
    err = xdma_service_poll(&xphi);
    switch(err_no(err)) {
        case SYS_ERR_OK:
            idle = 0;
            break;
        case DMA_ERR_DEVICE_IDLE:
            /* no op */
            break;
        default:
            return err;
            break;
    }
    err = event_dispatch_non_block(get_default_waitset());
    switch(err_no(err)) {
        case LIB_ERR_NO_EVENT :
            if (idle) {
                if (do_yield) {
                    thread_yield();
                    return SYS_ERR_OK;
                } else {
                    return LIB_ERR_NO_EVENT;
                }
            } else {
                return SYS_ERR_OK;
            }
        default:
            return err;
            break;
    }
    return SYS_ERR_OK;

}

/*
 * -------------------------------------------------------------------------------
 * booting cores
 * -------------------------------------------------------------------------------
 */

static char *cores_arg = NULL;
coreid_t cores_count = 0;
coreid_t cores_seen = 0;

#define USE_OCTOPUS_EVENTS 0
#if USE_OCTOPUS_EVENTS




octopus_trigger_id_t cores_tid;

static void spawnd_change_event(octopus_mode_t mode, char* record, void* state)
{
    debug_printf("spawnd_change_event...\n");
    if (mode & OCT_ON_SET) {
        debug_printf("spawnd found: %s\n", record);
        cores_seen++;

        if (cores_seen == cores_count) {
            errval_t err = oct_set("all_spawnds_up { iref: 0 }");
            assert(err_is_ok(err));
            err = oct_remove_trigger(cores_tid);
            assert(err_is_ok(err));
        }
    }
}

#endif

static errval_t boot_cores(void)
{
    errval_t err;

    debug_printf("booting cores...\n");

    char *arg[4];
    arg[0] = "corectrl";
    arg[1] = "boot";

    /* spawn core boot */
    if (cores_arg != NULL) {
        arg[2] = cores_arg;
    } else {
        arg[2] = "1:9";
        cores_count = 10;

    }
    arg[3] = NULL;

#if USE_OCTOPUS_EVENTS
    debug_printf("setting trigger...\n");

    /* set trigger for booting cors */
    err = oct_trigger_existing_and_watch("r'spawn.[0-9]+' { iref: _ }",
                                          spawnd_change_event, &cores_count,
                                          &cores_tid);
    if (err_is_fail(err)) {
        /* TODO: ERROR HANDLING */
    }
#endif
    debug_printf("spawning corectrl...\n");

    domainid_t new_domain;
    struct capref coreboot_cap = {cnode_task, TASKCN_SLOT_COREBOOT};

    err = spawn_program_with_caps(0, "k1om/sbin/corectrl", arg, NULL, NULL_CAP,
                                  coreboot_cap, 0, &new_domain);
    assert(err_is_ok(err));

#if !USE_OCTOPUS_EVENTS
    debug_printf("waiting for spawn...\n");
    for (cores_seen = 0; cores_seen < cores_count; ++cores_seen) {
        char buf[10];
        debug_printf("XX waiting for spawn.%u\n", cores_seen);
        snprintf(buf, 10, "spawn.%u", cores_seen);
        err = nameservice_blocking_lookup(buf, NULL);
        assert(err_is_ok(err));
    }

    err = oct_set("all_spawnds_up { iref: 0 }");
    assert(err_is_ok(err));
#endif

    return SYS_ERR_OK;
}

static void parse_arguments(int argc, char** argv)
{
    for (int i = 1; i < argc; i++) {
        if (strncmp(argv[i], "cpus=", 5) == 0) {
            cores_arg = argv[i] + 5;
        }
    }
}

/*
 * -------------------------------------------------------------------------------
 * Main
 * -------------------------------------------------------------------------------
 */

int main(int argc,
         char *argv[])
{
    debug_printf("Xeon Phi module started on node [%u].\n", disp_xeon_phi_id());

    errval_t err;

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

    parse_arguments(argc, argv);

    XDEBUG("Initializing system memory cap manager...\n");
    err = sysmem_cap_manager_init(sysmem_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize the cap manager.\n");
    }

    err = map_mmio_space(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not map the mmio space");
    }

    err = oct_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Octopus initialization failed.");
    }

    /* boot cores */
    err = boot_cores();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawning coreboot\n");
    }

    /* wait until the kernels are booted and spawnds are ready */
    err = nameservice_blocking_lookup("all_spawnds_up", NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "all_spawnds_up.\n");
    }

    err = xdma_service_init(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize the dma engine.\n");
    }

    err = smpt_init(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize the SMTP.\n");
    }

    lpaddr_t host_msg_base = strtol(argv[0], NULL, 16);
    uint8_t host_msg_size = strtol(argv[1], NULL, 16);

    XMESSAGING_DEBUG("Getting the host messaging cap...[%016lx, %02x]\n",
                     host_msg_base, host_msg_size);

    err = sysmem_cap_request(host_msg_base, host_msg_size, &host_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not obtain the system messsaging cap\n");
    }

    err = interphi_init(&xphi, host_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize the interphi communication\n");
    }

    err = xeon_phi_service_init(&xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the messaging service");
    }

    XDEBUG("Start polling for messages...\n");

    while (1) {
        xeon_phi_event_poll(1);
    }

    XDEBUG("Messaging loop terminated...\n");
    return 0;
}
