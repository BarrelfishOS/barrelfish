/**
 * \file
 * \brief Code for handling booting additional cores
 */

/*
 * Copyright (c) 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <inttypes.h>
#include <barrelfish_kpi/cpu.h> // for cpu_type_to_archstr()

#if defined(CONFIG_FLOUNDER_BACKEND_UMP)
/* Use to figure out when all monitors initialized. */
int seen_connections = 0;

static errval_t trace_ump_frame_identify(struct capref frame,
                                         struct intermon_ump_binding* b,
                                         size_t channel_length)
{
    // Identify UMP frame for tracing
    struct frame_identity umpid;
    errval_t err = invoke_frame_identify(frame, &umpid);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "invoke frame identity failed");
        return err;
    }

    b->ump_state.chan.recvid = (uintptr_t)umpid.base;
    b->ump_state.chan.sendid = (uintptr_t)
        (umpid.base + channel_length);

    return SYS_ERR_OK;
}


/**
 * \brief Msg handler for booting a given core
 *
 * \param id     id of the core to boot
 * \param hwid   hardware specific id of the core to boot
 * \param cpu_type  Type of cpu to boot
 * \param cmdline command-line args for kernel
 *
 * \bug Verify that cpu_type matches the elf image
 */
void boot_core_request(struct monitor_binding *b, coreid_t id,
                       struct capref frame)
{
    errval_t err;
    struct intermon_state *imon_st = NULL;

    struct intermon_binding *ibind;
    err = intermon_binding_get(id, &ibind);
    if (err_is_ok(err)) {
        imon_st = ibind->st;
        imon_st->originating_client = b;
        imon_st->capops_ready = false;
        return;
    }


    // Setup new inter-monitor connection to ourselves
#ifdef CONFIG_FLOUNDER_BACKEND_UMP_IPI
    struct intermon_ump_ipi_binding *ump_binding = malloc(sizeof(
                struct intermon_ump_ipi_binding));
#else
    struct intermon_ump_binding *ump_binding = malloc(sizeof(
                struct intermon_ump_binding));
#endif
    assert(ump_binding != NULL);

    // map it in
    void *buf;
    err = vspace_map_one_frame(&buf, MON_URPC_SIZE, frame, NULL, NULL);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_VSPACE_MAP);
        goto cleanup;
    }

#ifdef CONFIG_FLOUNDER_BACKEND_UMP_IPI
    // Get my arch ID
    uintptr_t my_arch_id = 0;
    err = invoke_monitor_get_arch_id(&my_arch_id);
    assert(err == SYS_ERR_OK);

    // Bootee's notify channel ID is always 1
    struct capref notify_cap;
    err = notification_create_cap(1, hwid, &notify_cap);
    assert(err == SYS_ERR_OK);

    // Allocate my own notification caps
    struct capref ep, my_notify_cap;
    struct lmp_endpoint *iep;
    int chanid;
    err = endpoint_create(LMP_RECV_LENGTH, &ep, &iep);
    assert(err_is_ok(err));
    err = notification_allocate(ep, &chanid);
    assert(err == SYS_ERR_OK);
    err = notification_create_cap(chanid, my_arch_id, &my_notify_cap);
    assert(err == SYS_ERR_OK);

    // init our end of the binding and channel
    err = intermon_ump_ipi_init(ump_binding, get_default_waitset(),
                                buf, MON_URPC_CHANNEL_LEN,
                                buf + MON_URPC_CHANNEL_LEN,
                                MON_URPC_CHANNEL_LEN, notify_cap,
                                my_notify_cap, ep, iep);
#else
    err = intermon_ump_init(ump_binding, get_default_waitset(),
                            buf, MON_URPC_CHANNEL_LEN,
                            (char *)buf + MON_URPC_CHANNEL_LEN,
                            MON_URPC_CHANNEL_LEN);
#endif
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_UMP_CHAN_BIND);
        goto cleanup;
    }

    err = trace_ump_frame_identify(frame, ump_binding,
                                   MON_URPC_CHANNEL_LEN);
    if (err_is_fail(err)) {
        goto cleanup;
    }

    struct intermon_binding* ib = (struct intermon_binding*)ump_binding;
    err = intermon_init(ib, id);
    imon_st = ib->st;
    imon_st->originating_client = b;
    imon_st->capops_ready = false;

    return;

cleanup:
    if (err_is_fail(err)) {
        // Cleanup
        DEBUG_ERR(err, "Failed to register intermon binding.");
        cap_destroy(frame);
        free(ump_binding);
    }

    errval_t err2 = b->tx_vtbl.boot_core_reply(b, NOP_CONT, err);
    if (err_is_fail(err2)) {
        USER_PANIC_ERR(err2, "sending boot_core_reply failed");
    }
}
#else
void boot_core_request(struct monitor_binding *b, coreid_t id,
                       struct capref frame) {
    printf("%s:%s:%d: unable to handle: boot_core_request\n", 
           __FILE__, __FUNCTION__, __LINE__);
}
#endif
