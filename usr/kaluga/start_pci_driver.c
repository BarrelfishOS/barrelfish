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
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include <dist2/dist2.h>
#include <skb/skb.h>

#include "kaluga.h"

extern char **environ;

static void pci_change_event(dist2_mode_t mode, char* device_record, void* st)
{
    KALUGA_DEBUG("pci_change_event: %s\n", device_record);
    errval_t err;
    if (mode & DIST_ON_SET) {
        uint64_t vendor_id, device_id;
        err = dist_read(device_record, "_ { vendor: %d, device_id: %d }",
                &vendor_id, &device_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Got malformed device record?");
        }

        // Ask the SKB which binary and where to start it...
        static char* query = "find_pci_driver(pci_card(%lu, %lu, _, _, _), Driver),"
                             "writeln(Driver).";
        err = skb_execute_query(query, vendor_id, device_id);
        if (err_no(err) == SKB_ERR_EXECUTION) {
            KALUGA_DEBUG("No PCI driver found for: VendorId=0x%lx, "
                         "DeviceId=0x%lx\n",
                    vendor_id, device_id);
            goto out;
        }
        else if (err_is_fail(err)) {
            DEBUG_ERR(err, "Failed to query SKB.\n");
            goto out;
        }

        // XXX: Find better way to parse binary
        // name from SKB
        char* binary_name = malloc(strlen(skb_get_output()));
        coreid_t core;
        skb_read_output("driver(%s, %c)", binary_name, &core);
        *strrchr(binary_name, ',') = '\0';

        // No need to ask the SKB as we always start pci for
        // in case we find a root bridge
        struct module_info* mi = find_module(binary_name);
        free(binary_name);
        if (mi == NULL || !is_auto_driver(mi)) {
            KALUGA_DEBUG("PCI driver not found or not declared as auto.\n");
            goto out;
        }

        if (mi->did == 0) {
            KALUGA_DEBUG("Spawn PCI driver: %s\n", mi->binary);

            err = spawn_program(core, mi->path, mi->argv+1,
                    environ, 0, &mi->did);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "Spawning %s failed.", mi->path);
            }
        }
        else {
            // Driver already running.
        }
    }

out:
    free(device_record);
}

errval_t watch_for_pci_devices(void)
{
    static char* pci_device  = "r'hw\\.pci\\.device\\.[0-9]+' { "
                               " bus: _, device: _, function: _, vendor: _,"
                               " device_id: _, class: _, subclass: _, "
                               " prog_if: _ }";
    dist2_trigger_id_t tid;
    return trigger_existing_and_watch(pci_device, pci_change_event, &tid);
}
