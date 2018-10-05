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
#include <barrelfish/nameservice_client.h>
#include <if/acpi_defs.h>
#include <acpi.h>
#include <mm/mm.h>
#include "acpi_shared.h"
#include "acpi_debug.h"
#include "ioapic.h"

extern struct capref biosmem;
static void get_vbe_bios_cap(struct acpi_binding *b)
{
    errval_t err;
    err = b->tx_vtbl.get_vbe_bios_cap_response(b, NOP_CONT, SYS_ERR_OK, biosmem,
                                               1UL << BIOS_BITS);
    assert(err_is_ok(err));
}

void acpi_service_arch_init(struct acpi_rx_vtbl *acpi_rx_vtbl)
{
    acpi_rx_vtbl->get_vbe_bios_cap_call = get_vbe_bios_cap;
}
