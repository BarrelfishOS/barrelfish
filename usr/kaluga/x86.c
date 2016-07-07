/**
 * \file
 * \brief Device manager for Barrelfish.
 *
 * Interacts with the SKB / PCI to start cores, drivers etc.
 *
 * x86 specific startup code
 *
 */

/*
 * Copyright (c) 2007-2010, 2016 ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <skb/skb.h>
#include "kaluga.h"

errval_t arch_startup(void)
{
    errval_t err = SYS_ERR_OK;
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

    KALUGA_DEBUG("Kaluga: int_controller_devices\n");

    err = watch_for_int_controller();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching interrupt controllers.");
    }

    KALUGA_DEBUG("Kaluga: wait_for_all_spawnds\n");

    err = wait_for_all_spawnds();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Unable to wait for spawnds failed.");
    }

    return err;
}
