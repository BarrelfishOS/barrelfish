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

#include "ioapic.h"
#include "ioapic_service.h"
#include "ioapic_debug.h"


static void enable_interrupt_handler(struct ioapic_binding* b, uint32_t gsi,
        coreid_t dest, uint32_t vector)
{
    errval_t err = SYS_ERR_OK;
    err = enable_and_route_interrupt(gsi, dest, vector);

    err = b->tx_vtbl.enable_and_route_interrupt_response(b, NOP_CONT, err);
    assert(err_is_ok(err));

}

extern struct capref phys_cap;
extern bool caps_received;
errval_t init_allocators(void);
static void transfer_physical_caps_handler(struct ioapic_binding* b,
        struct capref physical)
{
    APIC_DEBUG("transfer physical caps handler\n");

    phys_cap.cnode = build_cnoderef(physical, PAGE_CNODE_BITS);
    phys_cap.slot = 0;
    caps_received = true;

    errval_t err = init_allocators();
    assert(err_is_ok(err));

    err = init_all_apics();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "I/O APIC Initialization");
    }

    err = setup_interupt_override();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Setup interrupt overrides");
    }

    APIC_DEBUG("transfer physical caps handler done\n");

    debug_my_cspace();
    b->tx_vtbl.transfer_physical_caps_response(b, NOP_CONT);
}

struct ioapic_rx_vtbl acpi_rx_vtbl = {
    .transfer_physical_caps_call = transfer_physical_caps_handler,
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
