/**
 * \file
 * \brief PCI
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>
#include <barrelfish/nameservice_client.h>
#include <stdio.h>
#include <stdlib.h>
#include <mm/mm.h>
#include <if/monitor_blocking_defs.h>

#include <octopus/octopus.h>
#include <skb/skb.h>

#include "acpi_debug.h"
#include "acpi_shared.h"
#include "acpi_allocators.h"




uintptr_t my_hw_id;
bool vtd_force_off;




static errval_t setup_skb_info(void)
{
    skb_execute("[pci_queries].");
    errval_t err = skb_read_error_code();
    if (err_is_fail(err)) {
        ACPI_DEBUG("\npcimain.c: Could not load pci_queries.pl.\n"
               "SKB returned: %s\nSKB error: %s\n",
                skb_get_output(), skb_get_error_output());
        return err;
    }

    skb_add_fact("mem_region_type(%d,ram).", RegionType_Empty);
    skb_add_fact("mem_region_type(%d,roottask).", RegionType_RootTask);
    skb_add_fact("mem_region_type(%d,phyaddr).", RegionType_PhyAddr);
    skb_add_fact("mem_region_type(%d,multiboot_module).", RegionType_Module);
    skb_add_fact("mem_region_type(%d,platform_data).", RegionType_PlatformData);


    return acpi_arch_skb_set_info();
}

int main(int argc, char *argv[])
{
    errval_t err;

    // Parse CMD Arguments
    bool got_apic_id = false;
    bool do_video_init = false;
    vtd_force_off = true;

    for (int i = 1; i < argc; i++) {
        if(sscanf(argv[i], "apicid=%" PRIuPTR, &my_hw_id) == 1) {
            got_apic_id = true;
        } else if (strcmp(argv[i], "video_init") == 0) {
            do_video_init = true;
        } else if (strncmp(argv[i], "vtd_force_off", strlen("vtd_force_off")) == 0) {
            vtd_force_off = true;
        }
    }

    if(got_apic_id == false) {
        fprintf(stderr, "Usage: %s APIC_ID\n", argv[0]);
        fprintf(stderr, "Wrong monitor version?\n");
        return EXIT_FAILURE;
    }

    err = oct_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Initialize dist");
    }

    //connect to the SKB
    ACPI_DEBUG("acpi: connecting to the SKB...\n");
    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Connecting to SKB failed.");
    }

    ACPI_DEBUG("acpi: connecting to the SKB...\n");
    err = setup_skb_info();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Populating SKB failed.");
    }

    err = acpi_allocators_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Init memory allocator");
    }

    err = acpi_arch_copy_bios_mem();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Copy BIOS Memory");
    }

    err = acpi_arch_load_irq_routing_new();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "load irq routing new.");
    }

    int r = init_acpi();
    assert(r == 0);

    buttons_init();

    if (do_video_init) {
        acpi_arch_video_init();
    }

    err = acpi_interrupts_arch_setup();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "setup skb irq controllers");
    }


    start_service();


    messages_handler_loop();
}
