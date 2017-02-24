/**
 * \file acpi_allocators_arch.c
 * \brief 
 */


/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <mm/mm.h>

#include "acpi_debug.h"
#include "acpi_shared.h"
#include "acpi_allocators.h"

errval_t acpi_allocators_init_arch(struct bootinfo *bootinfo)
{
    for (int i = 0; i < bootinfo->regions_length; i++) {
        struct mem_region *mrp = &bootinfo->regions[i];
        if (mrp->mr_type == RegionType_ACPI_TABLE) {
            debug_printf("FOUND ACPI TABLE: %" PRIxGENPADDR "\n", mrp->mr_base);

            AcpiOsSetRootPointer(mrp->mr_base);
        }
    }
    return SYS_ERR_OK;
}
