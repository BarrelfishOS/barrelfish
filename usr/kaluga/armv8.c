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
#include <if/monitor_blocking_defs.h>
#include "kaluga.h"


static errval_t armv8_startup_common(void)
{
    errval_t err = SYS_ERR_OK;

    // Since we don't seem to be able to boot cores on the ARMv8 platforms yet,
    // we just set all_spawnds_up here. -RA,2017-02-24.
    err = oct_set("all_spawnds_up { iref: 0 }");
    assert(err_is_ok(err));

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

static errval_t fvp_startup(void)
{
    return armv8_startup_common();
}

static errval_t apm88xxxx_startup(void)
{
    errval_t err;

    err = skb_execute_query("[plat_apm88xxxx].");
    if(err_is_fail(err)){
        USER_PANIC_SKB_ERR(err, "Additional device db file 'plat_apm88xxxx' not loaded.");
    }

    return armv8_startup_common();
}

static errval_t cn88xx_startup(void)
{
    errval_t err;

    err = skb_execute_query("[plat_cn88xx].");
    if(err_is_fail(err)){
        USER_PANIC_SKB_ERR(err, "Additional device db file 'plat_cn88xx' not loaded.");
    }

    return armv8_startup_common();
}

errval_t arch_startup(char * add_device_db_file)
{
    errval_t err;

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Connect to SKB.");
    }

    // Make sure the driver db is loaded
    err = skb_execute("[device_db].");
    if (err_is_fail(err)) {
        USER_PANIC_SKB_ERR(err, "Device DB not loaded.");
    }

    if (add_device_db_file) {
        err = skb_execute_query("[%s].", add_device_db_file);
        if(err_is_fail(err)){
            USER_PANIC_SKB_ERR(err, "Additional device db file '%s' not loaded.",
                               add_device_db_file);
        }
    }

    struct monitor_blocking_binding *m = get_monitor_blocking_binding();
    assert(m != NULL);

    uint32_t arch, platform;
    err = m->rpc_tx_vtbl.get_platform(m, &arch, &platform);
    assert(err_is_ok(err));
    assert(arch == PI_ARCH_ARMV8A);

    switch(platform) {
    case PI_PLATFORM_FVP:
        debug_printf("Kaluga running on FVP\n");
        return fvp_startup();
    case PI_PLATFORM_APM88XXXX:
        debug_printf("Kaluga running on APM88xxxx\n");
        return apm88xxxx_startup();
    case PI_PLATFORM_CN88XX:
        debug_printf("Kaluga running on CN88xx\n");
        return cn88xx_startup();
    }

    return KALUGA_ERR_UNKNOWN_PLATFORM;
}
