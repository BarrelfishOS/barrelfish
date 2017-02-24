/**
 * \file
 * \brief ACPI management
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish_kpi/types.h>
#include <acpi.h>
#include <mm/mm.h>
#include <octopus/getset.h>
#include <octopus/barrier.h>
#include <skb/skb.h>
#include <pci/confspace/pci_confspace.h>
#include "acpi_shared.h"
#include "acpi_debug.h"
#include "ioapic.h"
#include "intel_vtd.h"
#include <trace/trace.h>

#include "pcilnk_controller_client.h"
#include "ioapic_controller_client.h"

// BIOS Copy
struct capref biosmem;


int acpi_arch_init(void)
{
    ACPI_STATUS as;

    // find and init any embedded controller drivers
    // we do this early, because control methods may need to access the EC space
    ec_probe_ecdt();

    as = AcpiInitializeObjects(ACPI_FULL_INITIALIZATION);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("AcpiInitializeObjects failed\n");
        return -1;
    }

    if (!vtd_force_off) {
        vtd_init();
    }

    return 0;
}

errval_t acpi_arch_load_irq_routing_new(void){
    // Load irq file
    errval_t err;
    err = skb_execute("[irq_routing_new].");
    if (err_is_fail(err)) {
        ACPI_DEBUG("Could not load irq_routing_new.pl.\n"
               "SKB returned: %s\nSKB error: %s\n",
                skb_get_output(), skb_get_error_output());
        return err;
    } else if(strstr(skb_get_error_output(), "library not found") != NULL) {
        debug_printf("Error processing irq_routing_new.pl.\n"
               "SKB stdout: %s\nSKB stderr: %s\n",
                skb_get_output(), skb_get_error_output());
        return SKB_ERR_EXECUTION;
    } else {
        ACPI_DEBUG("Successfully loaded irq_routing_new.pl.\n"
               "SKB returned: %s\nSKB error: %s\n",
                skb_get_output(), skb_get_error_output());
        return SYS_ERR_OK;
    }
}

errval_t acpi_arch_copy_bios_mem(void)
{
    errval_t err = SYS_ERR_OK;

    // Get a copy of the VBE BIOS before ACPI touches it
    struct capref bioscap;

    err = mm_alloc_range(&pci_mm_physaddr, BIOS_BITS, 0,
                       1UL << BIOS_BITS, &bioscap, NULL);
    assert(err_is_ok(err));

    void *origbios;
    struct vregion *origbios_vregion;
    err = vspace_map_one_frame(&origbios, 1 << BIOS_BITS, bioscap,
                               NULL, &origbios_vregion);
    assert(err_is_ok(err));

    err = frame_alloc(&biosmem, 1 << BIOS_BITS, NULL);
    assert(err_is_ok(err));

    void *newbios;
    struct vregion *newbios_vregion;
    err = vspace_map_one_frame(&newbios, 1 << BIOS_BITS, biosmem,
                               NULL, &newbios_vregion);
    assert(err_is_ok(err));

    memcpy(newbios, origbios, 1 << BIOS_BITS);

    // Unmap both vspace regions again
    vregion_destroy(origbios_vregion);
    vregion_destroy(newbios_vregion);

    err = mm_free(&pci_mm_physaddr, bioscap, 0, BIOS_BITS);
    assert(err_is_ok(err));

    return err;
}


errval_t acpi_arch_skb_set_info(void)
{
    errval_t err;
    err = skb_add_fact("mem_region_type(%d,apic).", RegionType_LocalAPIC);
    if (err_is_fail(err)) {
        return err;
    }

    return skb_add_fact("mem_region_type(%d,ioapic).", RegionType_IOAPIC);
}
