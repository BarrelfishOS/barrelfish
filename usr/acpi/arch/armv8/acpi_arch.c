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
#include <trace/trace.h>


int acpi_arch_init(void)
{
    ACPI_STATUS as;

    as = AcpiInitializeObjects(ACPI_FULL_INITIALIZATION);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("AcpiInitializeObjects failed\n");
        return -1;
    }

    return 0;
}


errval_t acpi_arch_copy_bios_mem(void)
{
    return SYS_ERR_OK;
}

errval_t acpi_arch_load_irq_routing_new(void)
{
    return SYS_ERR_OK;
}

void acpi_arch_video_init(void)
{
    /* no op */
}

errval_t acpi_arch_skb_set_info(void)
{
    errval_t err;
    err = skb_add_fact("mem_region_type(%d,gic).", RegionType_GIC);
    if (err_is_fail(err)) {
        return err;
    }

    return skb_add_fact("mem_region_type(%d,gicd).", RegionType_GIC_DIST);
}
