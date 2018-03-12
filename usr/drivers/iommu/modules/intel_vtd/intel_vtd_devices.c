/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <numa.h>

#include "intel_vtd.h"

errval_t vtd_device_create_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                  uint8_t fun, struct vtd *vtd,
                                  struct vtd_device **rdev)
{
    errval_t err;

    assert(rdev);

    struct vtd_ctxt_table *ct;
    err = vtd_get_ctxt_table_by_id(vtd, bus, &ct);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "faled to get the ctxt table\n");
        return err;
    }

    struct vtd_device *vdev = calloc(1, sizeof(*vdev));
    if (vdev == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    vdev->ctxt_table = ct;

    *rdev = vdev;

    return SYS_ERR_OK;
}

errval_t vtd_device_create(struct device_identity id,
                           struct vtd *vtd, struct vtd_device **rdev)
{

    return vtd_device_create_by_pci(id.segment, id.bus, id.device, id.function,
                                    vtd, rdev);
}



errval_t vtd_device_destroy(struct vtd_device *dev)
{
    errval_t err;
    err = vtd_device_remove_from_domain(dev);
    if (err_is_fail(err)) {
        return err;
    }

    memset(dev, 0, sizeof(*dev));

    return SYS_ERR_OK;
}




errval_t vtd_device_remove_from_domain(struct vtd_device *dev)
{
    if (dev->domain == NULL) {
        return SYS_ERR_OK;
    }

    return vtd_domains_remove_device(dev->domain, dev);
}

errval_t vtd_device_add_to_domain(struct vtd_device *dev, struct vtd_domain *dom)
{
    if (dev->domain != NULL) {
        return IOMMU_ERR_DEV_USED;
    }
    return vtd_domains_add_device(dom, dev);
}

struct vtd_domain *vtd_device_get_domain(struct vtd_device *dev)
{
    return dev->domain;
}