/**
 * \file
 * \brief DriverKit IOMMU Service
 *
 * Contains functions to request mappings from the IOMMU service
 */
/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich.
 * Attn: Systems Group.
 */
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <driverkit/iommu.h>
#include <skb/skb.h>

#include <hw_records.h>

#include <if/iommu_defs.h>
#include <if/iommu_rpcclient_defs.h>


#include "common.h"

#include "modules/intel_vtd/intel_vtd.h"


static void create_domain(struct iommu_binding *b, struct capref rootpt,
                          struct capref dev)
{
    errval_t err;

    struct iommu_device *d;
    err = iommu_device_get(dev, &d);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Obtaining the device failed\n");
        goto out;
    }

    struct iommu *iommu = iommu_device_get_iommu(d);
    assert(iommu);

    switch(iommu_get_type(iommu)) {
        case HW_PCI_IOMMU_INTEL: {

            struct vtd_domain * dom;
            err = vtd_domains_create((struct vtd *)iommu, rootpt, &dom);
            break;
        }
        case HW_PCI_IOMMU_AMD: {
            err = IOMMU_ERR_IOMMU_NOT_FOUND;
            break;
        }
        case HW_PCI_IOMMU_ARM: {
            err = IOMMU_ERR_IOMMU_NOT_FOUND;
            break;
        }
        default : {
            err = IOMMU_ERR_IOMMU_NOT_FOUND;
            break;
        }
    }

    if (err_is_fail(err)) {
        cap_destroy(rootpt);
    }

    /* clear slot */
    cap_destroy(dev);

    out:
    err = b->tx_vtbl.create_domain_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void delete_domain(struct iommu_binding *b, struct capref rootpt)
{
    errval_t err;

    USER_PANIC("NYi!\n");

    /* clear slot */
    cap_destroy(rootpt);

    err = b->tx_vtbl.delete_domain_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void add_device(struct iommu_binding *b, struct capref rootpt,
                       struct capref dev)
{
    errval_t err;

    struct iommu_device *d;
    err = iommu_device_get(dev, &d);
    if (err_is_fail(err)) {
        debug_printf("Obtaining the device failed\n");
        goto out;
    }

    struct iommu *iommu = iommu_device_get_iommu(d);
    assert(iommu);

    switch(iommu_get_type(iommu)) {
        case HW_PCI_IOMMU_INTEL: {
            struct vtd_domain *dom = vtd_domains_get_by_cap(rootpt);
            if (dom == NULL) {
                err = IOMMU_ERR_DOM_NOT_FOUND;
                goto out;
            }

            err = vtd_device_add_to_domain((struct vtd_device *)d, dom);

            break;
        }
        case HW_PCI_IOMMU_AMD: {
            err = IOMMU_ERR_IOMMU_NOT_FOUND;
            break;
        }
        case HW_PCI_IOMMU_ARM: {
            err = IOMMU_ERR_IOMMU_NOT_FOUND;
            break;
        }
        default : {
            err = IOMMU_ERR_IOMMU_NOT_FOUND;
            break;
        }
    }

    out:

    cap_destroy(dev);
    cap_destroy(rootpt);

    err = b->tx_vtbl.add_device_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void remove_device(struct iommu_binding *b, struct capref rootpt,
                          struct capref dev)
{
    errval_t err;

    struct iommu_device *d;
    err = iommu_device_get(dev, &d);
    if (err_is_fail(err)) {
        debug_printf("Obtaining the device failed\n");
        goto out;
    }

    struct iommu *iommu = iommu_device_get_iommu(d);
    assert(iommu);

    switch(iommu_get_type(iommu)) {
        case HW_PCI_IOMMU_INTEL: {
            err = vtd_device_remove_from_domain((struct vtd_device *)d);
            break;
        }
        case HW_PCI_IOMMU_AMD: {
            err = IOMMU_ERR_IOMMU_NOT_FOUND;
            break;
        }
        case HW_PCI_IOMMU_ARM: {
            err = IOMMU_ERR_IOMMU_NOT_FOUND;
            break;
        }
        default : {
            err = IOMMU_ERR_IOMMU_NOT_FOUND;
            break;
        }
    }
    out:

    cap_destroy(dev);
    cap_destroy(rootpt);

    err = b->tx_vtbl.remove_device_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void export_callback(void *st, errval_t err, iref_t iref)
{
    debug_printf("[iommu svc] Service exported: '%s'\n", err_getstring(err));

    if (err_is_fail(err)) {
        return;
    }

    debug_printf("[iommu svc] Registering %" PRIuIREF " with '%s\n",
                 iref, DRIVERKIT_IOMMU_SERVICE_NAME);

    err = nameservice_register(DRIVERKIT_IOMMU_SERVICE_NAME, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_callback(void *cst, struct iommu_binding *b)
{
    b->rx_vtbl.add_device_call = add_device;
    b->rx_vtbl.remove_device_call = remove_device;
    b->rx_vtbl.create_domain_call = create_domain;
    b->rx_vtbl.delete_domain_call = delete_domain;
    b->st = NULL;
    return SYS_ERR_OK;
}

errval_t iommu_service_init(void)
{
    debug_printf("[iommu svc] Initializing service\n");

    struct waitset *ws = get_default_waitset();
    return iommu_export(NULL, export_callback, connect_callback, ws,
                        IDC_EXPORT_FLAGS_DEFAULT);
}