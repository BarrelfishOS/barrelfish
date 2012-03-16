/**
 * \file
 * \brief Code responsible for booting application cores
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/cpu_arch.h>

#include <if/monitor_defs.h>

#include <octopus/octopus.h>
#include <skb/skb.h>

#include "kaluga.h"

// TODO: Needed because of we have to send
// boot_initialize_request after all cores have booted
// It's a hack (see monitors boot.c)
static coreid_t cores_on_boot = 0;
static coreid_t core_counter = 1; // BSP already up
static bool cores_booted = false;

static errval_t new_mon_msg(struct mon_msg_state** mms, send_handler_fn s)
{
    *mms = malloc(sizeof(struct mon_msg_state));
    if (*mms == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    (*mms)->send = s;
    (*mms)->next = NULL;

    return SYS_ERR_OK;
}

static void send_boot_initialize_request(struct monitor_binding* b,
        struct mon_msg_state* mm)
{
    errval_t err;

    err = b->tx_vtbl.boot_initialize_request(b,
            MKCONT(free, mm));

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, mm);
            return;
        }
        USER_PANIC_ERR(err, "kaluga: sending %s failed!", __FUNCTION__);
    }
}

static void boot_core_reply(struct monitor_binding *st, errval_t msgerr)
{
    static coreid_t core_boot_replies = 1; // BSP Monitor already running
    if (err_is_fail(msgerr)) {
        USER_PANIC_ERR(msgerr, "msgerr in boot_core_reply, exiting\n");
    }

    KALUGA_DEBUG("boot_core_reply: core_boot_replies=%d, cores_on_boot=%d\n",
            core_boot_replies+1, cores_on_boot);

    if (++core_boot_replies == cores_on_boot) {
        struct monitor_binding *mb = get_monitor_binding();
        struct mon_msg_state *mms = NULL;
        errval_t err = new_mon_msg(&mms, send_boot_initialize_request);
        assert(err_is_ok(err));

        KALUGA_DEBUG("before boot send...\n");
        mms->send(mb, mms);
    }
}

static void boot_initialize_reply(struct monitor_binding *st)
{
    KALUGA_DEBUG("boot_initialize_reply\n");
    cores_booted = true;
    errval_t err = dist_set("all_spawnds_up { iref: 0 }");
    assert(err_is_ok(err));
}


static void send_boot_core_request(struct monitor_binding* b,
        struct mon_msg_state* mm)
{
    errval_t err;

    struct module_info* mi = find_module("cpu");
    err = b->tx_vtbl.boot_core_request(b, MKCONT(free, mm), mm->core_id,
            mm->arch_id, CURRENT_CPU_TYPE, mi->complete_line);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            enqueue_msg_state(b, mm);
            return;
        }
        USER_PANIC_ERR(err, "device_manager: sending %s failed!", __FUNCTION__);
    }
}

static inline void configure_monitor_binding(void)
{
    struct monitor_binding* mb = get_monitor_binding();
    mb->rx_vtbl.boot_core_reply = boot_core_reply;
    mb->rx_vtbl.boot_initialize_reply = boot_initialize_reply;
    mb->st = NULL;
}

static void cpu_change_event(dist2_mode_t mode, char* record, void* state)
{
    if (mode & DIST_ON_SET) {
        KALUGA_DEBUG("CPU found: %s\n", record);

        uint64_t cpu_id, arch_id, enabled = 0;
        errval_t err = dist_read(record, "_ { cpu_id: %d, id: %d, enabled: %d }",
                &cpu_id, &arch_id, &enabled);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Cannot read record.");
            assert(!"Illformed core record received");
            goto out;
        }

        // cpu_id may vary so we can't really use this
        // to enumerate cores...
        assert(my_core_id == 0);
        static coreid_t core_id = 1;

        if (arch_id != my_arch_id && enabled) {
            struct monitor_binding* mb = get_monitor_binding();

            struct mon_msg_state* mms = NULL;
            err = new_mon_msg(&mms, send_boot_core_request);
            assert(err_is_ok(err));
           
            mms->core_id = core_id++;
            mms->arch_id = arch_id;
            mms->send(mb, mms);
            (*(coreid_t*) state) += 1;

            // XXX: copied this line from spawnd bsp_bootup,
            // not sure why x86_64 is hardcoded here but it
            // seems broken...
            skb_add_fact("corename(%d, x86_64, apic(%lu)).",
                    mms->core_id, mms->arch_id);
        }

    }
    if (mode & DIST_ON_DEL) {
        KALUGA_DEBUG("CPU removed: %s\n", record);
        assert(!"NYI");
    }

out:
    assert(!(mode & DIST_REMOVED));
    free(record);
}

errval_t watch_for_cores(void)
{
    configure_monitor_binding();

    static char* local_apics = "r'hw\\.apic\\.[0-9]+' { cpu_id: _, "
                               "                        enabled: 1, "
                               "                        id: _ }";
    dist2_trigger_id_t tid;
    errval_t err = trigger_existing_and_watch(local_apics, cpu_change_event,
            &core_counter, &tid);
    cores_on_boot = core_counter;

    if (err_is_ok(err)) {
        if (cores_on_boot == 1) {
            // No additional cores found
            // XXX: We simulate a boot initialize reply to set the
            // all_spawnds_up record. The whole app monitor boot protocol will
            // most likely change in the future and render this unnecessary.
            boot_initialize_reply(NULL);
        }
    }

    // Wait until all cores found are booted, this ensures
    // we won't deadlock in case we start a driver on
    // a core that is not ready
    /*
    while (!cores_booted) {
        messages_wait_and_handle_next();
    }*/

    return err;
}

static void ioapic_change_event(dist2_mode_t mode, char* record, void* state)
{
    if (mode & DIST_ON_SET) {
        struct module_info* mi = find_module("ioapic");
        if (mi != NULL) {
            // Pass apic id as 1st argument
            static const char* fmt = "apicid=%u";
            int len = snprintf(NULL, 0, fmt, my_arch_id);
            char* apic_arg = malloc(len+1);
            snprintf(apic_arg, len+1, fmt, my_arch_id);

            mi->argv[2] = apic_arg;
            errval_t err = mi->start_function(my_core_id, mi, record);
            free(apic_arg);

            switch (err_no(err)) {
            case SYS_ERR_OK:
                KALUGA_DEBUG("Spawned I/O APIC driver: %s\n", mi->binary);
                break;

            case KALUGA_ERR_DRIVER_ALREADY_STARTED:
                KALUGA_DEBUG("%s already running.\n", mi->binary);
                break;

            case KALUGA_ERR_DRIVER_NOT_AUTO:
                KALUGA_DEBUG("%s not declared as auto, ignore.\n", mi->binary);
                break;

            default:
                DEBUG_ERR(err, "Unhandled error while starting %s\n", mi->binary);
                break;
            }
        }
    }
    else if (mode & DIST_ON_DEL) {
        KALUGA_DEBUG("Removed I/O APIC?");
        assert(!"NYI");
    }

    free(record);
}


errval_t watch_for_ioapic(void)
{
    KALUGA_DEBUG("watch_for_ioapic\n");
    static char* io_apics = "r'hw.ioapic.[0-9]+' { id: _, address: _, "
                            "irqbase: _ }";

    dist2_trigger_id_t tid;
    return trigger_existing_and_watch(io_apics, ioapic_change_event,
            &core_counter, &tid);
}
