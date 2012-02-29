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
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include <dist2/dist2.h>
#include <skb/skb.h>

#include "kaluga.h"

static domainid_t pci_driver = 0;
extern char **environ;

static void bridge_change_event(dist2_mode_t mode, char* bridge_record, void* st)
{
    if (mode & DIST_ON_SET) {
        if (pci_driver != 0) {
            KALUGA_DEBUG("Got new root bridge, PCI driver is already running.");
            goto out;
        }

        // No need to ask the SKB as we always start pci for
        // in case we find a root bridge
        struct module_info* mi = find_module("pci");
        if (mi == NULL || !is_auto_driver(mi)) {
            KALUGA_DEBUG("PCI driver not found or not declared as auto.");
            goto out;
        }

        // XXX: always spawn on my_core_id
        KALUGA_DEBUG("bridge_change_event: spawn mi->path: %s\n", mi->path);
        errval_t err = spawn_program(my_core_id, mi->path, mi->argv+1,
                environ, 0, &pci_driver);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Spawning PCI driver failed.");
        }
    }

out:
    free(bridge_record);
}

errval_t watch_for_pci_root_bridge(void)
{
    static char* root_bridge = "r'hw\\.pci\\.rootbridge\\.[0-9]+' { "
                               " bus: _, device: _, function: _, maxbus: _,"
                               " acpi_node: _ }";
    dist2_trigger_id_t tid;
    return trigger_existing_and_watch(root_bridge, bridge_change_event, &tid);
}
