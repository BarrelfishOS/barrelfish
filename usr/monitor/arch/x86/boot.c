/**
 * \file
 * \brief Code for handling booting additional cores
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <inttypes.h>
#include <elf/elf.h>
#include <target/x86/barrelfish_kpi/coredata_target.h>
#include <target/x86_32/barrelfish_kpi/paging_target.h>
#include <target/x86_64/barrelfish_kpi/paging_target.h>
#include <notify_ipi.h>

/**
 * \brief Initialize monitor running on app cores
 */
errval_t boot_arch_app_core(int argc, char *argv[],
                            coreid_t *ret_parent_coreid,
                            struct intermon_binding **ret_binding)

{
    errval_t err;

    assert(argc == 4);

    // core_id of the core that booted this core
    coreid_t core_id = strtol(argv[1], NULL, 10);
    *ret_parent_coreid = core_id;

#ifdef CONFIG_FLOUNDER_BACKEND_UMP_IPI
    // other monitor's channel id
    assert(strncmp("chanid", argv[2], strlen("chanid")) == 0);
    int chan_id = strtol(strchr(argv[2], '=') + 1, NULL, 10);

    // arch id of the core that booted us
    assert(strncmp("archid", argv[3], strlen("archid")) == 0);
    int arch_id = strtol(strchr(argv[3], '=') + 1, NULL, 10);
#endif

    // check that the frame is big enough
    struct capref frame = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_MON_URPC,
    };
    struct frame_identity frameid;
    err = invoke_frame_identify(frame, &frameid);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_FRAME_IDENTIFY);
        return err;
    }

    size_t framesize = ((uintptr_t)1) << frameid.bits;
    if (framesize < 2 * MON_URPC_CHANNEL_LEN) {
        return LIB_ERR_UMP_FRAME_OVERFLOW;
    }

    // map it in
    void *buf;
    err = vspace_map_one_frame(&buf, framesize, frame, NULL, NULL);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_VSPACE_MAP);
        return err;
    }

#ifdef CONFIG_FLOUNDER_BACKEND_UMP_IPI
    // Create notify cap to other monitor
    struct capref notify_cap;
    err = notification_create_cap(chan_id, arch_id, &notify_cap);
    assert(err == SYS_ERR_OK);

    // Allocate my own notification caps
    struct capref ep, my_notify_cap;
    struct lmp_endpoint *iep;
    int chanid;
    err = endpoint_create(LMP_RECV_LENGTH, &ep, &iep);
    assert(err_is_ok(err));
    err = notification_allocate(ep, &chanid);
    assert(err == SYS_ERR_OK);
    assert(chanid == 1);        // Make sure it's channel 1
    uintptr_t my_arch_id;
    err = invoke_monitor_get_arch_id(&my_arch_id);
    assert(err == SYS_ERR_OK);
    err = notification_create_cap(chanid, my_arch_id, &my_notify_cap);
    assert(err == SYS_ERR_OK);

    // setup our side of the binding
    struct intermon_ump_ipi_binding *umpb =
        malloc(sizeof(struct intermon_ump_ipi_binding));
    assert(umpb != NULL);

    err = intermon_ump_ipi_init(umpb, get_default_waitset(),
                                buf + MON_URPC_CHANNEL_LEN,
                                MON_URPC_CHANNEL_LEN,
                                buf, MON_URPC_CHANNEL_LEN, notify_cap,
                                my_notify_cap, ep, iep);
#else
    struct intermon_ump_binding *umpb;
    umpb = malloc(sizeof(struct intermon_ump_binding));
    assert(umpb != NULL);

    err = intermon_ump_init(umpb, get_default_waitset(),
                            (char *)buf + MON_URPC_CHANNEL_LEN,
                            MON_URPC_CHANNEL_LEN,
                            buf, MON_URPC_CHANNEL_LEN);
#endif
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_UMP_CHAN_BIND);
        return err;
    }

    // Identify UMP frame for tracing
    umpb->ump_state.chan.sendid = (uintptr_t)frameid.base;
    umpb->ump_state.chan.recvid =
        (uintptr_t)(frameid.base + MON_URPC_CHANNEL_LEN);

    *ret_binding = &umpb->b;

    return SYS_ERR_OK;
}
