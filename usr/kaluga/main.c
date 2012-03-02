/**
 * \file
 * \brief Device manager for Barrelfish.
 *
 * Interacts with the SKB / PCI to start cores, drivers etc.
 *
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
#include <string.h>
#include <errors/errno.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/cpu_arch.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>

#include <if/monitor_defs.h>

#include <dist2/dist2.h>
#include <skb/skb.h>

#include "kaluga.h"

coreid_t my_core_id = 0; // Core ID
uint32_t my_arch_id = 0; // APIC ID

extern char **environ;

static errval_t start_networking(coreid_t core, struct module_info* driver,
        char* record)
{
    errval_t err = SYS_ERR_OK;

    if (!is_started(driver)) {
        err = spawn_program(core, driver->path, driver->argv+1,
                environ, 0, &driver->did);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Spawning %s failed.", driver->path);
            return err;
        }

        struct module_info* netd = find_module("netd");
        if (netd == NULL || !is_auto_driver(netd)) {
            KALUGA_DEBUG("netd not found. Driver will probably not work correctly.");
            return err;
        }

        // XXX: Manually add cardname (overwrite first (auto) argument)
        size_t name_len = strlen("cardname=")+strlen(driver->binary)+1;
        char* cardname = malloc(name_len);
        sprintf(cardname, "cardname=%s", driver->binary);
        netd->argv[0] = cardname;
        err = spawn_program(core, netd->path, netd->argv,
                environ, 0, &netd->did);
        free(cardname);
    }

    return err;

}

static void add_start_function_overrides(void)
{
    set_start_function("e1000", start_networking);
    set_start_function("rtl8029", start_networking);
}

static void parse_arguments(int argc, char** argv)
{
    for (int i = 1; i < argc; i++) {
        if (strncmp(argv[i], "apicid=", sizeof("apicid")) == 0) {
            my_arch_id = strtol(argv[i] + sizeof("apicid"), NULL, 10);
        }
        else if(strcmp(argv[i], "boot") == 0) {
            // ignored
        }
    }
}

static inline errval_t wait_for_pci(void)
{
    iref_t iref;
    return nameservice_blocking_lookup("pci_discovery_done", &iref);
}

int main(int argc, char** argv)
{
    init_environ();

    errval_t err;
    char* record = NULL;

    coreid_t my_core_id = disp_get_core_id();
    parse_arguments(argc, argv);

    // We need to run on core 0
    // (we are responsible for booting all the other cores)
    assert(my_core_id == BSP_CORE_ID);
    printf("Kaluga running.\n");

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Connect to SKB.");
    }
    // Make sure the driver db is loaded
    err = skb_execute("[device_db].");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Device DB not loaded.");
    }

    err = dist_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Initialize dist library.");
    }

    err = init_boot_modules();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Parse boot modules.");
    }
    add_start_function_overrides();

    // The current boot protocol needs us to have
    // knowledge about how many CPUs are available at boot
    // time in order to start-up properly.
    err = dist_barrier_enter("barrier.acpi", &record, 2);

    err = watch_for_cores();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching for cores.");
    }

    err = watch_for_pci_root_bridge();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching for PCI root bridges.");
    }

    /*
    err = watch_for_ioapic();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching I/O APICs.");
    }*/

    // 3. Watch for PCI
    err = wait_for_pci();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "PCI Initialized.");
        return EXIT_FAILURE;
    }

    err = watch_for_pci_devices();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching PCI devices.");
    }

    messages_handler_loop();
    return EXIT_SUCCESS;
}
