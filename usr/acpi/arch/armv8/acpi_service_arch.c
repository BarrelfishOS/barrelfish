/**
 * \file
 * \brief ACPI daemon Flounder handler functions
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
#include <if/acpi_defs.h>
#include <acpi.h>
#include "acpi_shared.h"
#include "acpi_debug.h"


void acpi_service_arch_init(struct acpi_rx_vtbl *acpi_rx_vtbl)
{
    /* no setup for VTD etc */
    return;
}
