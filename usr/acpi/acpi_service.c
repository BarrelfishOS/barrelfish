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
#include <if/acpi_defs.h>
#include <acpi.h>

#include "pci_acpi.h"

#include "acpi_shared.h"
#include "acpi_debug.h"

static inline bool mcfg_correct_length(uint32_t header_len)
{
    return header_len >=
            sizeof(ACPI_TABLE_MCFG) + sizeof(ACPI_MCFG_ALLOCATION);
}

/**
 * \brief Look for an MCFG table
 *
 * This tells us where the PCI express memory-mapped configuration area is
 */
static void get_pcie_confspace(struct acpi_binding* b)
{
    ACPI_DEBUG("get_pcie_confspace\n");

    errval_t err;
    ACPI_STATUS as;
    ACPI_TABLE_HEADER *mcfg_header;

    as = AcpiGetTable("MCFG", 1, &mcfg_header);
    if (ACPI_SUCCESS(as) && mcfg_correct_length(mcfg_header->Length)) {

        ACPI_MCFG_ALLOCATION *mcfg = (void*) mcfg_header +
                sizeof(ACPI_TABLE_MCFG);
        ACPI_DEBUG("PCIe enhanced configuration region at 0x%lx "
                   "(segment %u, buses %u-%u)\n", mcfg->Address,
                   mcfg->PciSegment, mcfg->StartBusNumber, mcfg->EndBusNumber);

        err = b->tx_vtbl.get_pcie_confspace_response(b, NOP_CONT,
                SYS_ERR_OK, mcfg->Address, mcfg->PciSegment,
                mcfg->StartBusNumber, mcfg->EndBusNumber);

    } else {
        ACPI_DEBUG("No MCFG table found -> no PCIe enhanced configuration\n");
        err = b->tx_vtbl.get_pcie_confspace_response(b, NOP_CONT,
                ACPI_ERR_NO_MCFG_TABLE, 0, 0, 0, 0);
    }

    assert(err_is_ok(err));
}

static void get_path_name(ACPI_HANDLE handle, char* name, size_t len) {
    ACPI_BUFFER buf = { .Length = len, .Pointer = name };
    ACPI_STATUS s;

    s = AcpiGetName(handle, ACPI_FULL_PATHNAME, &buf);
    assert(ACPI_SUCCESS(s));
}

static void read_irq_table(struct acpi_binding* b, char* pathname,
        acpi_pci_address_t addr, uint8_t bus)
{
    ACPI_DEBUG("read_irq_table: %s\n", pathname);

    errval_t err;
    ACPI_STATUS as;
    ACPI_HANDLE handle;

    as = AcpiGetHandle(NULL, pathname, &handle);
    if (ACPI_SUCCESS(as)) {
        ACPI_HANDLE child;
        acpi_get_irqtable_device(handle, addr, &child, bus);

        char name[128];
        get_path_name(child, name, 128);
        ACPI_DEBUG("Sending back path name: %s\n", name);

        err = b->tx_vtbl.read_irq_table_response(b, NOP_CONT, SYS_ERR_OK, name);
        assert(err_is_ok(err));
    }
    else {
        ACPI_DEBUG("Unknown ACPI Handle for path: %s\n", pathname);
        err = b->tx_vtbl.read_irq_table_response(b, NOP_CONT,
                ACPI_ERR_INVALID_PATH_NAME, NULL);
        assert(err_is_ok(err));
    }

    free(handle);
}

struct acpi_rx_vtbl acpi_rx_vtbl = {
    .get_pcie_confspace_call = get_pcie_confspace,
    .read_irq_table_call = read_irq_table
};

static void export_callback(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));

    err = nameservice_register("acpi", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
    ACPI_DEBUG("acpi service exported\n");
}

static errval_t connect_callback(void *cst, struct acpi_binding *b)
{
    ACPI_DEBUG("acpi service get connection\n");
    b->rx_vtbl = acpi_rx_vtbl;
    b->st = NULL;

    return SYS_ERR_OK;
}

void start_service(void)
{
    ACPI_DEBUG("start_service\n");
    errval_t r = acpi_export(NULL, export_callback, connect_callback,
                            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(r));

    ACPI_DEBUG("start_service: terminated\n");
}
