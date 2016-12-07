/**
 * \file
 * \brief Device manager for Barrelfish.
 *
 * Interacts with the SKB / PCI to start cores, drivers etc.
 *
 */

/*
 * Copyright (c) 2007-2010, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
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
#include <thc/thc.h>

#include <trace/trace.h>

#include "kaluga.h"


coreid_t my_core_id = 0;  // Core ID
uint32_t my_arch_id = 0;  // APIC ID
struct pci_address eth0 = {0xff, 0xff, 0xff};
size_t cpu_count = 0;

static void add_start_function_overrides(void)
{
    set_start_function("e1000n", start_networking);
    set_start_function("rtl8029", start_networking);
    set_start_function("corectrl", start_boot_driver);
}

static void parse_arguments(int argc, char** argv, char ** add_device_db_file, size_t *cpu_count)
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
        } else if (strncmp(argv[i],"add_device_db=", strlen("add_device_db=")) == 0){
           *add_device_db_file = argv[i] + strlen("add_device_db=");
           printf("Kaluga using additional device_db file: %s.\n", *add_device_db_file);
        } else if (strncmp(argv[i], "cpu_count=", strlen("cpu_count=")) == 0) {
            sscanf(argv[i], "cpu_count=%zu", cpu_count);
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
    char * add_device_db_file = NULL;
    parse_arguments(argc, argv, &add_device_db_file, &cpu_count);

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

    err = arch_startup(add_device_db_file);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "arch startup");
    }

    THCFinish();
    return EXIT_SUCCESS;
}

