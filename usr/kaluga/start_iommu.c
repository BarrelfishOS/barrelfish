/**
 * \file
 * \brief Code responsible for booting application cores
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>

#include <if/octopus_defs.h>
#include <if/octopus_thc.h>
#include <if/acpi_defs.h>
#include <pci/pci.h>
#include <acpi_client/acpi_client.h>
#include <octopus/octopus.h>
#include <octopus/trigger.h>

#include <skb/skb.h>

#include <hw_records.h>

#include "kaluga.h"


errval_t start_iommu_driver(coreid_t where, struct module_info* driver,
                        char* record, struct driver_argument* arg)
{
    errval_t err;

    debug_printf("Kaluga: starting driver for IOMMU '%s'\n", record);

    static struct domain_instance* inst;
    struct driver_instance *drv;

    if (!is_auto_driver(driver)) {
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    char *key = NULL;
    uint64_t type, flags, segment, address, idx;
    err = oct_read(record, "%s { " HW_PCI_IOMMU_RECORD_FIELDS_READ " }",
                   &key, &idx, &type, &flags, &segment, &address);
    if (err_is_fail(err)) {
        return err;
    }

    debug_printf("Kaluga: iommu idx: %" PRIu64 ", key: '%s', segment: %" PRIu64 ", address: 0x%"
                  PRIx64 "\n", idx, key, segment, address);

    /* record is no longer needed */
    free(record);

    char *iommu_module = NULL;
    switch(type) {
        case HW_PCI_IOMMU_INTEL :
            iommu_module = "iommu_intel_module";
            break;
        case HW_PCI_IOMMU_AMD :
            iommu_module = "iommu_amd_module";
            break;
        case HW_PCI_IOMMU_ARM :
            iommu_module = "iommu_arm_module";
            break;
        default :
            err = DRIVERKIT_ERR_NO_DRIVER_FOUND;
            goto out;
    }

    debug_printf("Kaluga: iommu module '%s'\n", iommu_module);

    /* we currently start all IOMMUss in the same domain */
    if (driver->driverinstance == NULL) {
        debug_printf("Driver instance not running, starting...\n");

        inst = instantiate_driver_domain(driver->binary, where);
        if (inst == NULL) {\
            err = DRIVERKIT_ERR_DRIVER_INIT;
            goto out;
        }

        driver->driverinstance = inst;

        while (inst->b == NULL) {
            event_dispatch(get_default_waitset());
        }

        err = connect_to_acpi();
        assert(err_is_ok(err));
    }

    struct acpi_binding* acpi = get_acpi_binding();

    errval_t msgerr;
    struct capref devcap = NULL_CAP;
    err = slot_alloc(&devcap);
    if (err_is_fail(err)) {
        goto out;
    }

    err = acpi->rpc_tx_vtbl.mm_alloc_range_proxy(acpi, BASE_PAGE_BITS, address,
                                                 address + BASE_PAGE_SIZE,
                                                 &devcap, &msgerr);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to allocate cap\n");
        goto out;
    }
    if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "failed to allocate cap\n");
        err = msgerr;
        goto out;
    }

    drv = ddomain_create_driver_instance(iommu_module, key);
    if (drv == NULL) {
        err = DRIVERKIT_ERR_DRIVER_INIT;
        goto out;
    }

    debug_printf("Kaluga: iommu with stubbed device cap\n");
    ddomain_driver_add_cap(drv, devcap);

    err = ddomain_instantiate_driver(inst, drv);
out:
    free(key);
    return err;
}

static void iommu_change_event(octopus_mode_t mode, const char* record,
                               void* st)
{
    if (mode & OCT_ON_SET) {
    
        errval_t err;

        struct module_info* mi = find_module("iommu");
        if (mi == NULL) {
            KALUGA_DEBUG("IOMMU driver not found or not declared as auto.");
            return;
        }

        // XXX: always spawn on my_core_id; otherwise we need to check that
        // the other core is already up
        err = mi->start_function(my_core_id, mi, (CONST_CAST)record, NULL);
        switch (err_no(err)) {
            case SYS_ERR_OK:
                KALUGA_DEBUG("Spawned IOMMU driver: %s\n", mi->binary);
                break;

            case KALUGA_ERR_DRIVER_NOT_AUTO:
                KALUGA_DEBUG("%s not declared as auto, ignore.\n", mi->binary);
                break;

            default:
                DEBUG_ERR(err, "Unhandled error while starting %s\n", mi->binary);
                break;
        }
    }
}

errval_t watch_for_iommu(void)
{
    errval_t err;
    char** names;
    size_t len = 0;

    err = oct_get_names(&names, &len, HW_PCI_IOMMU_RECORD_REGEX);
    if (err_is_fail(err)) {
        if (err == OCT_ERR_NO_RECORD) {
            debug_printf("######## Stop watching for IOMMU as since there are no records available\n");
            return SYS_ERR_OK;
        }
        return err;
    }

    char* record;
    char* key;
    uint64_t type, flags, segment, address, idx;
    for (int i = 0; i < len; i++) {
        err = oct_get(&record, names[i]);
        if (err_is_fail(err)) {
            goto out;
        }

        err = oct_read(record, "%s { " HW_PCI_IOMMU_RECORD_FIELDS_READ " }",
                       &key, &idx, &type, &flags, &segment, &address);
        if (err_is_fail(err)) {
            goto out;
        }
        
        if (type == HW_PCI_IOMMU_DMAR_FAIL) {
            // Failed reading DMAR 
            debug_printf("######## Stop watching for IOMMU as DMAR reading failed\n");
            len = 0;
            break;
        }

        iommu_change_event(OCT_ON_SET, record, NULL);
    }
 
    if (len > 0) {  
        err = oct_barrier_enter("barrier.iommu", &record ,2);
        if (err_is_fail(err)){
            goto out;
        }
        
        if (record) {
            free(record);
        }
    }
out:
    oct_free_names(names, len);
    return err;
}
