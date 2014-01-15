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
#include <trace/trace.h>
#include <barrelfish/spawn_client.h>

#include "kaluga.h"

static coreid_t core_counter = 1;

static void cpu_change_event(octopus_mode_t mode, char* record, void* state)
{
    if (mode & OCT_ON_SET) {
        KALUGA_DEBUG("CPU found: %s\n", record);
        assert(my_core_id == 0); // TODO(gz): why?

        uint64_t cpu_id, arch_id, enabled = 0;
        errval_t err = oct_read(record, "_ { processor_id: %d, apic_id: %d, enabled: %d }",
                &cpu_id, &arch_id, &enabled);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Cannot read record.");
            assert(!"Illformed core record received");
            goto out;
        }
        // XXX: copied this line from spawnd bsp_bootup,
        // not sure why x86_64 is hardcoded here but it
        // seems broken...
        skb_add_fact("corename(%"PRIuCOREID", x86_64, apic(%"PRIu64")).",
                core_counter, arch_id);

        struct module_info* mi = find_module("x86boot");
        if (mi != NULL) {
            err = mi->start_function(0, mi, record);
            assert(err_is_ok(err));
        }

    }
    if (mode & OCT_ON_DEL) {
        KALUGA_DEBUG("CPU removed: %s\n", record);
        assert(!"NYI");
    }

out:
    assert(!(mode & OCT_REMOVED));
    free(record);
}


errval_t watch_for_cores(void)
{
    static char* local_apics = "r'hw\\.apic\\.[0-9]+' { cpu_id: _, "
                               "                        enabled: 1, "
                               "                        id: _ }";
   octopus_trigger_id_t tid;
   return oct_trigger_existing_and_watch(local_apics, cpu_change_event, NULL, &tid);
}

errval_t start_boot_driver(coreid_t where, struct module_info* mi,
        char* record)
{
    assert(mi != NULL);
    errval_t err = SYS_ERR_OK;

    if (!is_auto_driver(mi)) {
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    // Construct additional command line arguments containing pci-id.
    // We need one extra entry for the new argument.
    uint64_t cpu_id, apic_id;
    char **argv = mi->argv;
    bool cleanup = false;

    KALUGA_DEBUG("Starting x86boot for %s", record);
    err = oct_read(record, "_ { processor_id: %d, apic_id: %d }",
            &cpu_id, &apic_id);
    skb_add_fact("corename(%"PRIuCOREID", x86_64, apic(%"PRIu64")).",
            core_counter++, apic_id);

    if (err_is_ok(err)) {
        argv = malloc((mi->argc+1) * sizeof(char *));
        memcpy(argv, mi->argv, mi->argc * sizeof(char *));
        char *apic_id_s  = malloc(10);
        snprintf(apic_id_s, 10, "%"PRIx64"", apic_id);

        argv[mi->argc] = "up";
        mi->argc += 1;
        argv[mi->argc] = apic_id_s;
        mi->argc += 1;
        argv[mi->argc] = NULL;

        cleanup = true;
    }
    else {
        DEBUG_ERR(err, "Malformed CPU record?");
        return err;
    }

    struct capref task_cap_kernel;
    task_cap_kernel.cnode = cnode_task;
    task_cap_kernel.slot = TASKCN_SLOT_KERNELCAP;

#ifdef KALUGA_SERVICE_DEBUG
    struct capability info;
    err = debug_cap_identify(task_cap_kernel, &info);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not identify the capability.");
    }
    char buffer[1024];
    debug_print_cap(buffer, 1024, &info);
    KALUGA_DEBUG("%s:%d: capability=%s\n", __FILE__, __LINE__, buffer);
#endif

    struct capref inheritcn_cap;
    err = alloc_inheritcn_with_caps(&inheritcn_cap,
                                    NULL_CAP, NULL_CAP, task_cap_kernel);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "alloc_inheritcn_with_caps failed.");
    }

    err = spawn_program_with_caps(where, mi->path, argv,
                                  environ, inheritcn_cap,
                                  NULL_CAP, 0, &mi->did);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", mi->path);
    }

    if (cleanup) {
        free(argv[mi->argc-1]);
        free(argv);
    }

    return err;
}
