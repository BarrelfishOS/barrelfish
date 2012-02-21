/**
 * \file
 * \brief ACPI daemon Flounder handler functions
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/ioapic_defs.h>

#include "pci.h"
#include "pci_acpi.h"
#include "acpi_shared.h"

#include "ioapic_debug.h"


static void enable_interrupt_handler(struct ioapic_binding* b, uint32_t gsi,
        coreid_t dest, uint32_t vector)
{
    errval_t err = SYS_ERR_OK;
    err = enable_and_route_interrupt(gsi, dest, vector);

    err = b->tx_vtbl.enable_and_route_interrupt_response(b, NOP_CONT, err);
    assert(err_is_ok(err));

}

struct ioapic_rx_vtbl acpi_rx_vtbl = {
    .enable_and_route_interrupt_call = enable_interrupt_handler,
};

static void export_callback(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));

    err = nameservice_register("ioapic", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
    APIC_DEBUG("ioapic service exported\n");
}

static errval_t connect_callback(void *cst, struct ioapic_binding *b)
{
    APIC_DEBUG("ioapic service get connection\n");
    b->rx_vtbl = acpi_rx_vtbl;
    b->st = NULL;

    return SYS_ERR_OK;
}

errval_t start_service(void)
{
    return ioapic_export(NULL, export_callback, connect_callback,
                         get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
}
