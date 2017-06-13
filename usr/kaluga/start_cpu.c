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
#include <if/octopus_defs.h>

#include <octopus/octopus.h>
#include <octopus/trigger.h>

#include <skb/skb.h>
#include <trace/trace.h>
#include <barrelfish/spawn_client.h>

#include <hw_records.h>

#include "kaluga.h"

static const char *processor_regex = HW_PROCESSOR_GENERIC_REGEX;

static void cpu_change_event(octopus_mode_t mode, const char* record, void* state)
{
    if (mode & OCT_ON_SET) {
        KALUGA_DEBUG("CPU found: %s\n", record);

        /* try to extract basic information from the record */
        uint64_t barrelfish_id, type, hw_id, enabled = 0;
        errval_t err = oct_read(record, "_ { " HW_PROCESSOR_GENERIC_FIELDS " }",
                                &enabled, &barrelfish_id, &hw_id, &type);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Cannot read record.");
            printf("Malformed CPU record. Do not boot discovered CPU %"PRIu64".\n",
                    barrelfish_id);
            goto out;
        }

        /* find the corectrl module for the given cpu type */
        struct module_info* mi = find_corectrl_for_cpu_type((enum cpu_type)type);
        if (mi != NULL) {
            err = mi->start_function(0, mi, (CONST_CAST)record, NULL);
            if (err_is_fail(err)) {
                printf("Boot driver not found. Do not boot discovered CPU %"PRIu64".\n",
                       barrelfish_id);
                goto out;
            }
        }
    }
    if (mode & OCT_ON_DEL) {
        KALUGA_DEBUG("CPU removed: %s\n", record);
        assert(!"NYI");
    }

out:
    assert(!(mode & OCT_REMOVED));
}

errval_t watch_for_cores(void)
{
    octopus_trigger_id_t tid;
    return oct_trigger_existing_and_watch(processor_regex, cpu_change_event,
                                          NULL, &tid);
}

// State for delayed cleanup of inherit cnode
struct inheritcn_del_st {
    struct capref        capref;
    octopus_trigger_id_t tid;
    coreid_t             coreid;
};

// Trigger function: gets called when spanwd on core n is up to delete
// associated inherit cnode that was given to `corectrl boot n`.
static void delete_inheritcn(octopus_mode_t mode, const char *record, void *state)
{
    errval_t err;
    struct inheritcn_del_st *st = state;
    if (mode & OCT_ON_SET) {
        KALUGA_DEBUG("spawnd up for %d: deleting inheritcn\n", st->coreid);
        err = cap_delete(st->capref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "deleting inheritcn for %d\n", st->coreid);
        }
        assert(err_is_ok(err));
        err = slot_free(st->capref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "freeing slot for %d\n", st->coreid);
        }
        assert(err_is_ok(err));
        err = oct_remove_trigger(st->tid);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "removing trigger");
        }
        assert(err_is_ok(err));
        free(state);
    }
}

errval_t start_boot_driver(coreid_t where, struct module_info* mi,
        char* record, struct driver_argument * int_arg)
{
    assert(mi != NULL);
    errval_t err;

    if (!is_auto_driver(mi)) {
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    // Construct additional command line arguments containing pci-id.
    // We need one extra entry for the new argument.
    char **argv = mi->argv;
    bool cleanup = false;
    char barrelfish_id_s[10];
    size_t argc = mi->argc;

    KALUGA_DEBUG("Starting corectrl for %s\n", record);
    uint64_t barrelfish_id, cpu_type, hw_id, enabled = 0;
    err = oct_read(record, "_ { " HW_PROCESSOR_GENERIC_FIELDS " }",
                            &enabled, &barrelfish_id, &hw_id, &cpu_type);
    if (err_is_ok(err)) {
        /*
         * XXX: change this to a generic cpuhwid instead of apic!
         */
        skb_add_fact("corename(%"PRIu64", %s, apic(%"PRIu64")).",
                     barrelfish_id, cpu_type_to_archstr(cpu_type), hw_id);

        /* we are already running */
        if (barrelfish_id == my_core_id) {
            return SYS_ERR_OK;
        }

        if (!enabled) {
            printf("CPU %" PRIu64 " is not enabled. Skipping driver initialization\n",
                    barrelfish_id);
            return SYS_ERR_OK;
        }

        argv = malloc((argc+5) * sizeof(char *));
        memcpy(argv, mi->argv, argc * sizeof(char *));
        snprintf(barrelfish_id_s, 10, "%"PRIu64"", barrelfish_id);

        argv[argc] = "boot";
        argc += 1;
        argv[argc] = barrelfish_id_s;
        argc += 1;
        // Copy kernel args over to new core
        struct module_info* cpu_module = find_module("cpu");
        if (cpu_module != NULL && strlen(cpu_module->args) > 1) {
            KALUGA_DEBUG("%s:%s:%d: Boot with cpu arg %s and barrelfish_id_s=%s\n",
                         __FILE__, __FUNCTION__, __LINE__, cpu_module->args, barrelfish_id_s);
            argv[argc] = "-a";
            argc += 1;
            argv[argc] = cpu_module->args;
            argc += 1;
        }
        argv[argc] = NULL;

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
                                  NULL_CAP, SPAWN_FLAGS_NEW_DOMAIN,
                                  &mi->did[0]);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Spawning %s failed.", mi->path);
    }

    if (cleanup) {
        free(argv);
    }

    // Cannot just delete inherit cnode capability here, as deleting any CNode
    // copy will always delete all copies of the CNode. Store inherit cnode
    // cap in octopus, and delete it when the matching spawnd is up
    char spawnd[32];
    snprintf(spawnd, 32, "spawn.%s", barrelfish_id_s);
    struct inheritcn_del_st *tstate = calloc(1, sizeof(*tstate));
    tstate->capref = inheritcn_cap;
    tstate->coreid = barrelfish_id;
    errval_t err2 = oct_trigger_existing_and_watch(spawnd, delete_inheritcn,
                                                   tstate, &tstate->tid);
    if (err_is_fail(err2)) {
        free(tstate);
        return err2;
    }

    return err;
}


static void spawnd_change_event(octopus_mode_t mode, const char* record,
                                void* state)
{
    size_t count = (size_t) state;
    static coreid_t spawnd_counter = 0;

    if (mode & OCT_ON_SET) {
        KALUGA_DEBUG("spawnd found: %s\n", record);
        spawnd_counter++;

        if (spawnd_counter == count) {
            KALUGA_DEBUG("Found enough spawnds, setting all_spawnds_up\n");
            errval_t err = oct_set("all_spawnds_up { iref: 0 }");
            assert(err_is_ok(err));
        }
    }
}

extern size_t cpu_count;

errval_t wait_for_all_spawnds(void)
{
    // Note: The whole wait for all_spawnds_up thing is a hack.
    // Our overall design goal is a system where cores
    // come and go dynamically and we do not want / need
    // to wait for a stable state.
    // However, some of our code (for example domain spanning)
    // still assumes a fixed set of cores and will deadlock
    // otherwise. Therefore we need to fix those parts first.
    errval_t err;
#if !defined(__ARM_ARCH_7A__)
    KALUGA_DEBUG("Waiting for acpi");
    char* record = NULL;
    err = oct_wait_for(&record, "acpi { iref: _ }");
    if (err_is_fail(err)) {
        return err_push(err, KALUGA_ERR_WAITING_FOR_ACPI);
    }
#endif

    // No we should be able to get core count
    // of all cores to estimate the amount of
    // spawnd's we have to expect (one per core)
    char** names;
    size_t count;
    err = oct_get_names(&names, &count, processor_regex);
    if (err_is_fail(err)) {
        return err_push(err, KALUGA_ERR_QUERY_LOCAL_APIC);
    }
    oct_free_names(names, count);

    if (cpu_count) {
        count = cpu_count;
    }

    static char* spawnds = "r'spawn.[0-9]+' { iref: _ }";
    octopus_trigger_id_t tid;
    return oct_trigger_existing_and_watch(spawnds, spawnd_change_event, (void*)count, &tid);
}
