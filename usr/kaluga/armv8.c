/**
 * \file
 * \brief ARMv8 arch specfic code
 */

/*
 * Copyright (c) 2013, 2016 ETH Zurich.
 * Copyright (c) 2015-2016, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <skb/skb.h>
#include <barrelfish_kpi/platform.h>
#include <if/monitor_blocking_rpcclient_defs.h>
#include "kaluga.h"


static errval_t tmas_startup(void)
{
    errval_t err = SYS_ERR_OK;
    // We need to run on core 0
    // (we are responsible for booting all the other cores)
    assert(my_core_id == BSP_CORE_ID);

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

    return SYS_ERR_OK;
}

errval_t arch_startup(void)
{
    errval_t err = SYS_ERR_OK;

    struct monitor_blocking_rpc_client *m = get_monitor_blocking_rpc_client();
    assert(m != NULL);

    uint32_t arch, platform;
    err = m->vtbl.get_platform(m, &arch, &platform);
    assert(err_is_ok(err));
    assert(arch == PI_ARCH_ARMV8A);

    switch(platform) {
        case PI_PLATFORM_TMAS:
            debug_printf("Kaluga running on TMAS\n");
            return tmas_startup();
    }

    return KALUGA_ERR_UNKNOWN_PLATFORM;
}
