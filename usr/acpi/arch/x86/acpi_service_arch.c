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
#include "intel_vtd.h"

extern struct capref biosmem;
static void get_vbe_bios_cap(struct acpi_binding *b)
{
    errval_t err;
    err = b->tx_vtbl.get_vbe_bios_cap_response(b, NOP_CONT, SYS_ERR_OK, biosmem,
                                               1UL << BIOS_BITS);
    assert(err_is_ok(err));
}

static void create_domain(struct acpi_binding *b, struct capref pml4)
{
    errval_t err;
    err = vtd_create_domain(pml4);
    err = b->tx_vtbl.create_domain_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void delete_domain(struct acpi_binding *b, struct capref pml4)
{
    errval_t err;
    err = vtd_remove_domain(pml4);
    err = b->tx_vtbl.delete_domain_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void vtd_add_device(struct acpi_binding *b, uint32_t seg, uint32_t bus, 
                           uint32_t dev, uint32_t func, struct capref pml4)
{
    errval_t err;
    err = vtd_domain_add_device(seg, bus, dev, func, pml4);
    err = b->tx_vtbl.vtd_add_device_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void vtd_remove_device(struct acpi_binding *b, uint32_t seg, uint32_t bus, 
                              uint32_t dev, uint32_t func, struct capref pml4)
{
    errval_t err;
    err = vtd_domain_remove_device(seg, bus, dev, func, pml4);
    err = b->tx_vtbl.vtd_remove_device_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void vtd_id_dom_add_devices(struct acpi_binding *b)
{
    errval_t err;
    vtd_identity_domain_add_devices();
    err = b->tx_vtbl.vtd_id_dom_add_devices_response(b, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
}

void acpi_service_arch_init(struct acpi_rx_vtbl *acpi_rx_vtbl)
{
    acpi_rx_vtbl->get_vbe_bios_cap_call = get_vbe_bios_cap;

    acpi_rx_vtbl->create_domain_call = create_domain;
    acpi_rx_vtbl->delete_domain_call = delete_domain;
    acpi_rx_vtbl->vtd_add_device_call = vtd_add_device;
    acpi_rx_vtbl->vtd_remove_device_call = vtd_remove_device;
    acpi_rx_vtbl->vtd_id_dom_add_devices_call = vtd_id_dom_add_devices;
}
