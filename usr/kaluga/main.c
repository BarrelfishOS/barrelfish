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
#include <errors/errno.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/cpu_arch.h>
#include <barrelfish/nameservice_client.h>

#include <if/monitor_defs.h>

#include <dist2/dist2.h>
#include <skb/skb.h>

#include "kaluga.h"



errval_t watch_for_pci_devices(void);
errval_t watch_for_pci_devices(void)
{
    char** names = NULL;
    size_t len = 0;
    errval_t err = dist_get_names(&names, &len, "r'device.*' { class: _ }");

    for (size_t i=0; i<len; i++) {
        //debug_printf("found device: %s\n", names[i]);
        char* device_record = NULL;
        err = dist_get(&device_record, names[i]);
        assert(err_is_ok(err));

        uint64_t device_id, vendor_id;
        //debug_printf("device record is: %s\n", device_record);
        err = dist_read(device_record, "_ { device_id: %d, vendor: %d }", &device_id, &vendor_id);
        assert(err_is_ok(err));
        debug_printf("device_record: %s\n", device_record);
        char* db_record = NULL;
        err = dist_get(&db_record, "pci.db.devices.%lu.%lu", vendor_id, device_id);
        if (err_is_ok(err)) {
            char* vendor_name = NULL;
            char* device_name = NULL;
            //debug_printf("found db_record: %s\n", db_record);

            err = dist_read(db_record, "_ { device_name: %s, vendor_name: %s }", &device_name, &vendor_name);
            assert(err_is_ok(err));

            KALUGA_DEBUG("** New device found: %s, %s\n", vendor_name, device_name);
            free(vendor_name);
            free(device_name);
        }
        else {
            KALUGA_DEBUG("** New device not recognized: vendor_id:%lu, device_id:%lu\n", vendor_id, device_id);
        }

        free(device_record);
    }

    dist_free_names(names, len);

    return err;
}


static inline errval_t wait_for_pci(void)
{
    iref_t iref;
    return nameservice_blocking_lookup("pci_discovery_done", &iref);
}

int main(int argc, char** argv)
{
    errval_t err;
    char* record = NULL;

    // We need to run on core 0
    // (we are responsible for booting all the other cores)
    assert(disp_get_core_id() == BSP_CORE_ID);
    printf("Kaluga running.\n");

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Connect to SKB.");
    }

    err = dist_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Initialize dist library.");
    }

    // The current boot protocol needs us to have
    // knowledge about how many CPUs are available at boot
    // time in order to start-up properly.
    err = dist_barrier_enter("barrier.acpi", &record, 2);

    err = watch_for_cores();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching for cores.");
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

    /*
    err = watch_for_pci_rootcomplex();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching PCI root complexes.");
    }*/

    /*
    err = watch_for_pci_devices();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Watching PCI devices.");
    }*/


    messages_handler_loop();
    return EXIT_SUCCESS;
}
