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
#include <mm/mm.h>
#include "acpi_shared.h"
#include "acpi_debug.h"

#ifdef ACPI_HAVE_VTD
#   include "intel_vtd.h"
#endif
extern bool mm_debug;

// XXX: proper cap handling (del etc.)
static void mm_alloc_range_proxy_handler(struct acpi_binding* b, uint8_t sizebits,
		                                 genpaddr_t minbase, genpaddr_t maxlimit)
{
    ACPI_DEBUG("mm_alloc_range_proxy_handler: sizebits: %d, minbase: 0x%lx maxlimit: 0x%lx\n",
	       sizebits, minbase, maxlimit);

    struct capref devframe = NULL_CAP;
    /* errval_t err = mm_alloc_range(&pci_mm_physaddr, sizebits, minbase, maxlimit, &devframe, NULL); */
    errval_t err = mm_realloc_range(&pci_mm_physaddr, sizebits, minbase, &devframe);
    if (err_is_fail(err)) {
    	DEBUG_ERR(err, "mm realloc range failed...\n");
    }

    err = b->tx_vtbl.mm_alloc_range_proxy_response(b, NOP_CONT, devframe, err);
    assert(err_is_ok(err));
}

static void mm_realloc_range_proxy_handler(struct acpi_binding* b, uint8_t sizebits,
                                           genpaddr_t minbase)
{
    ACPI_DEBUG("mm_realloc_range_proxy_handler: sizebits: %d, "
               "minbase: 0x%"PRIxGENPADDR"\n",
               sizebits, minbase);

    struct capref devframe = NULL_CAP;
    errval_t err = mm_realloc_range(&pci_mm_physaddr, sizebits, minbase, &devframe);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "mm alloc range failed...\n");
    }

    err = b->tx_vtbl.mm_realloc_range_proxy_response(b, NOP_CONT, devframe, err);
    assert(err_is_ok(err));
}

// XXX: proper cap handling
static void mm_free_proxy_handler(struct acpi_binding* b, struct capref devframe,
		                          uint64_t base, uint8_t sizebits)
{
    ACPI_DEBUG("mm_free_proxy_handler: base: 0x%"PRIx64", sizebits: %d\n", base, sizebits);

    errval_t err = mm_free(&pci_mm_physaddr, devframe, base, sizebits);
    if (err_is_fail(err)) {
    	DEBUG_ERR(err, "mm free failed...\n");
    }

    err = b->tx_vtbl.mm_free_proxy_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void enable_interrupt_handler(struct acpi_binding* b, uint32_t gsi,
        coreid_t dest, uint32_t vector)
{
    errval_t err = SYS_ERR_OK;
    err = enable_and_route_interrupt(gsi, dest, vector);

    err = b->tx_vtbl.enable_and_route_interrupt_response(b, NOP_CONT, err);
    assert(err_is_ok(err));

}

static inline bool mcfg_correct_length(uint32_t header_len)
{
    return header_len >=
            sizeof(ACPI_TABLE_MCFG) + sizeof(ACPI_MCFG_ALLOCATION);
}

static void get_pcie_confspace(struct acpi_binding* b)
{
    ACPI_DEBUG("get_pcie_confspace\n");

    errval_t err;
    ACPI_STATUS as;
    ACPI_TABLE_HEADER *mcfg_header;

    as = AcpiGetTable("MCFG", 1, &mcfg_header);
    if (ACPI_SUCCESS(as) && mcfg_correct_length(mcfg_header->Length)) {

        ACPI_MCFG_ALLOCATION *mcfg = (void*) mcfg_header
                + sizeof(ACPI_TABLE_MCFG);
        ACPI_DEBUG(
                "PCIe enhanced configuration region at 0x%"PRIx64" "
                "(segment %u, buses %u-%u)\n", mcfg->Address,
                mcfg->PciSegment, mcfg->StartBusNumber, mcfg->EndBusNumber);

        err = b->tx_vtbl.get_pcie_confspace_response(b, NOP_CONT, SYS_ERR_OK,
                mcfg->Address, mcfg->PciSegment, mcfg->StartBusNumber,
                mcfg->EndBusNumber);

    } else {
        ACPI_DEBUG("No MCFG table found -> no PCIe enhanced configuration\n");
        err = b->tx_vtbl.get_pcie_confspace_response(b, NOP_CONT,
                ACPI_ERR_NO_MCFG_TABLE, 0, 0, 0, 0);
    }

    assert(err_is_ok(err));
}

static void get_path_name(ACPI_HANDLE handle, char* name, size_t len)
{
    ACPI_BUFFER buf = { .Length = len, .Pointer = name };
    ACPI_STATUS s;

    s = AcpiGetName(handle, ACPI_FULL_PATHNAME, &buf);
    assert(ACPI_SUCCESS(s));
}

static void read_irq_table(struct acpi_binding* b, const char* pathname,
        acpi_pci_address_t addr, uint8_t bus)
{
    ACPI_DEBUG("read_irq_table: (parent)%s, (%"PRIu8",%"PRIu8",%"PRIu8"), %"PRIu8"\n",
            pathname == NULL ? "NULL" : pathname, addr.bus, addr.device, addr.function, bus);

    errval_t err;
    ACPI_STATUS as;
    ACPI_HANDLE handle;

    as = AcpiGetHandle(NULL, (CONST_CAST)pathname, &handle);
    if (ACPI_SUCCESS(as)) {
        ACPI_HANDLE child;
        err = acpi_get_irqtable_device(handle, addr, &child, bus);

        if(err_is_fail(err)){
            ACPI_DEBUG("get_irq_table failed.\n");
            err = b->tx_vtbl.read_irq_table_response(b, NOP_CONT, err, NULL);
            assert(err_is_ok(err));
        } else {
            char name[128];
            get_path_name(child, name, 128);
            ACPI_DEBUG("Sending back path name: %s\n", name);

            err = b->tx_vtbl.read_irq_table_response(b, NOP_CONT, SYS_ERR_OK, name);
            assert(err_is_ok(err));
        }
    }
    else {
        ACPI_DEBUG("Unknown ACPI Handle for path: %s\n", pathname);
        err = b->tx_vtbl.read_irq_table_response(b, NOP_CONT,
                ACPI_ERR_INVALID_PATH_NAME, NULL);
        assert(err_is_ok(err));
    }
}



static void set_device_irq_handler(struct acpi_binding *b, const char* device, uint32_t irq)
{
    errval_t err = set_device_irq(device,irq);
    err = b->tx_vtbl.set_device_irq_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void reset_handler(struct acpi_binding *b)
{
    if (AcpiGbl_FADT.Flags & ACPI_FADT_RESET_REGISTER) {
        printf("Resetting machine via ACPI...\n");
        ACPI_STATUS as = AcpiReset();
        if (ACPI_FAILURE(as)) {
            printf("ACPI reset failed\n");
        }
    }

    printf("Resetting machine via syscall...\n");
    errval_t err = sys_reboot();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "reboot syscall failed");
    }
}

static void sleep_handler(struct acpi_binding *b, uint32_t state)
{
    printf("Entering S%"PRIu32" sleep state via ACPI...\n", state);
    ACPI_STATUS as = AcpiEnterSleepStatePrep(state);
    if (!ACPI_SUCCESS(as)) {
        printf("AcpiEnterSleepStatePrep failed\n");
        return;
    }

    as = AcpiEnterSleepState(state);
    if (!ACPI_SUCCESS(as)) {
        printf("AcpiEnterSleepState failed\n");
    }
}


static
ACPI_STATUS get_handle_handler_callback(
    ACPI_HANDLE                     Object,
    UINT32                          NestingLevel,
    void                            *Context,
    void                            **ReturnValue) {

    ACPI_STATUS as;
    ACPI_DEVICE_INFO *device_info;
    as = AcpiGetObjectInfo(Object, &device_info);
    if (ACPI_FAILURE(as)) {
        debug_printf("AcpiGetObjectInfo failed: %x\n", as);
        return AE_OK;
    }

    if (device_info->HardwareId.Length &&
            !strncmp(device_info->HardwareId.String, Context, device_info->HardwareId.Length)) {
        debug_printf("device HardwareId=%s UniqueId=%s\n", device_info->HardwareId.String, device_info->UniqueId.String);
        *ReturnValue = Object;
    }
    ACPI_FREE(device_info);
    return AE_OK;
}

static void get_handle_handler(struct acpi_binding *b, const char *dev_id)
{
    errval_t err = SYS_ERR_OK;;

    debug_printf("Looking up handle for device '%s'\n", dev_id);

    ACPI_STATUS s;
    ACPI_HANDLE handle = NULL;

    s = AcpiGetDevices(NULL, get_handle_handler_callback, (CONST_CAST)dev_id, &handle);
    if (ACPI_FAILURE(s)) {
        debug_printf("Looking up handle failed: %d\n", s);
        err = ACPI_ERR_INVALID_HANDLE;
    } else if (handle == NULL) {
        err = ACPI_ERR_OBJECT_NOT_FOUND;
    }

    //out uint64 handle, out errval err
    err = b->tx_vtbl.get_handle_response(b, NOP_CONT, (uint64_t)handle, err);
    assert(err_is_ok(err));
}

static void eval_integer_handler(struct acpi_binding *b,
                                 uint64_t handle, const char *path)
{
    errval_t err = SYS_ERR_OK;

    ACPI_STATUS s;
    ACPI_INTEGER val = 0;
    s = acpi_eval_integer((ACPI_HANDLE)handle, path, &val);
    if (ACPI_FAILURE(s)) {
        if (s == AE_BAD_PATHNAME) {
            err = ACPI_ERR_INVALID_PATH_NAME;
        } else {
            err = ACPI_ERR_INVALID_HANDLE;
        }
        val = 0;
    }

    debug_printf("eval_integer_handler\n");
    err = b->tx_vtbl.eval_integer_response(b, NOP_CONT, val, err);
}


struct acpi_rx_vtbl acpi_rx_vtbl = {
    .get_pcie_confspace_call = get_pcie_confspace,
    .read_irq_table_call = read_irq_table,
    .set_device_irq_call = set_device_irq_handler,
    .enable_and_route_interrupt_call = enable_interrupt_handler,

    .get_handle_call = get_handle_handler,
    .eval_integer_call = eval_integer_handler,

    .mm_alloc_range_proxy_call = mm_alloc_range_proxy_handler,
    .mm_realloc_range_proxy_call = mm_realloc_range_proxy_handler,
    .mm_free_proxy_call = mm_free_proxy_handler,

    .reset_call = reset_handler,
    .sleep_call = sleep_handler,
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

    acpi_service_arch_init(&acpi_rx_vtbl);

    errval_t r = acpi_export(NULL, export_callback, connect_callback,
                            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(r));

    ACPI_DEBUG("start_service: terminated\n");
}
