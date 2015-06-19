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

#include <if/monitor_defs.h>

#include <vfs/vfs.h>
#include <pci/pci.h> // for pci_addr
#include <octopus/octopus.h>
#include <skb/skb.h>
#include <thc/thc.h>

#include <trace/trace.h>

#include "kaluga.h"


coreid_t my_core_id = 0;  // Core ID
uint32_t my_arch_id = 0;  // APIC ID
struct pci_address eth0 = {0xff, 0xff, 0xff};

static void add_start_function_overrides(void)
{
    set_start_function("e1000n", start_networking);
    set_start_function("rtl8029", start_networking);
    set_start_function("corectrl", start_boot_driver);
}

static void parse_arguments(int argc, char** argv)
{
    for (int i = 1; i < argc; i++) {
        if (strncmp(argv[i], "apicid=", 7) == 0) {
            my_arch_id = strtol(argv[i] + 7, NULL, 10);
        } else if (strncmp(argv[i], "eth0=", 5) == 0) {
            int parsed = sscanf(argv[i], "eth0=%" SCNu8 ":%" SCNu8 ":%" SCNu8,
                                &eth0.bus, &eth0.device, &eth0.function);
            printf("Kaluga using eth0=%u:%u:%u as network device\n", eth0.bus,
                         eth0.device, eth0.function);
            if (parsed != 3) {
                eth0.bus = 0xff;
                eth0.device = 0xff;
                eth0.function = 0xff;
            }
        } else if (strcmp(argv[i], "boot") == 0) {
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
    vfs_init();
    init_environ();

    errval_t err;

    my_core_id = disp_get_core_id();
    parse_arguments(argc, argv);

    err = oct_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Initialize octopus service.");
    }

    KALUGA_DEBUG("Kaluga: parse boot modules...\n");

    err = init_boot_modules();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Parse boot modules.");
    }
    add_start_function_overrides();
#ifdef __x86__
    // We need to run on core 0
    // (we are responsible for booting all the other cores)
    assert(my_core_id == BSP_CORE_ID);
    KALUGA_DEBUG("Kaluga running on x86.\n");

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Connect to SKB.");
    }

    // Make sure the driver db is loaded
    err = skb_execute("[device_db].");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Device DB not loaded.");
    }

    // The current boot protocol needs us to have
    // knowledge about how many CPUs are available at boot
    // time in order to start-up properly.
    char* record = NULL;
    err = oct_barrier_enter("barrier.acpi", &record, 2);

    KALUGA_DEBUG("Kaluga: watch_for_cores\n");

    err = watch_for_cores();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching cores.");
    }

    KALUGA_DEBUG("Kaluga: pci_root_bridge\n");

    err = watch_for_pci_root_bridge();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching PCI root bridges.");
    }

    KALUGA_DEBUG("Kaluga: pci_devices\n");

    err = watch_for_pci_devices();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching PCI devices.");
    }

    KALUGA_DEBUG("Kaluga: wait_for_all_spawnds\n");

    err = wait_for_all_spawnds();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Unable to wait for spawnds failed.");
    }

#elif __pandaboard__
    debug_printf("Kaluga running on Pandaboard.\n");

    err = init_cap_manager();
    assert(err_is_ok(err));

    err = oct_set("all_spawnds_up { iref: 0 }");
    assert(err_is_ok(err));

    struct module_info* mi = find_module("fdif");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.fdif {}");
        assert(err_is_ok(err));
    }
    mi = find_module("mmchs");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.mmchs {}");
        assert(err_is_ok(err));
    }
    mi = find_module("mmchs2");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.mmchs {}");
        assert(err_is_ok(err));
    }
    mi = find_module("prcm");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.prcm {}");
        assert(err_is_ok(err));
    }
    mi = find_module("serial");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.uart {}");
        assert(err_is_ok(err));
    }
    mi = find_module("sdma");
    if (mi != NULL) {
        err = mi->start_function(0, mi, "hw.arm.omap44xx.sdma {}");
        assert(err_is_ok(err));
    }

    mi = find_module("usb_manager");
    if (mi != NULL) {
#define USB_ARM_EHCI_IRQ 109
        char *buf = malloc(255);
        uint8_t offset = 0;
        mi->cmdargs = buf;
        mi->argc = 3;
        mi->argv[0] = mi->cmdargs + 0;

        snprintf(buf + offset, 255 - offset, "ehci\0");
        offset += strlen(mi->argv[0]) + 1;
        mi->argv[1] = mi->cmdargs + offset;
        snprintf(buf + offset, 255 - offset, "%u\0", 0xC00);
        offset += strlen(mi->argv[1]) + 1;
        mi->argv[2] = mi->cmdargs + offset;
        snprintf(buf+offset, 255-offset, "%u\0", USB_ARM_EHCI_IRQ);

        // XXX Use customized start function or add to module info
        err = mi->start_function(0, mi, "hw.arm.omap44xx.usb {}");
        assert(err_is_ok(err));
    }
#endif

    THCFinish();
    return EXIT_SUCCESS;
}

