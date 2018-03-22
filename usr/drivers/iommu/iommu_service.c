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

#if 0
static void create_domain(struct iommu_binding *b, struct capref rootpt,
                          struct capref dev)
{
    errval_t err;

    debug_printf("[iommu] creating domain\n");

    struct iommu_device *d;
    err = iommu_device_get(dev, &d);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Obtaining the device failed\n");
        goto out;
    }

    debug_printf("[iommu] found the device. getting IOMMU..\n");


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
            struct vtd *v = (struct vtd *)iommu;
            debug_printf("obtained vtd: %u %u\n", v->index, v->scope_all);

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
#endif

errval_t iommu_service_init(void)
{
    debug_printf("[iommu svc] Initializing service\n");


    return SYS_ERR_OK;
}

static struct iommu_rx_vtbl rx_vtbl;

errval_t iommu_service_new_endpoint(struct capref ep, struct iommu_device *dev,
                                    idc_endpoint_t type)
{
    struct waitset *ws = get_default_waitset();
    return iommu_create_endpoint(type, &rx_vtbl, dev, ws,
                                 IDC_ENDPOINT_FLAGS_DEFAULT, &dev->binding, ep);
}