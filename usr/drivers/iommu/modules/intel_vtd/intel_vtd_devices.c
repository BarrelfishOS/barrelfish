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
#include "../generic/common.h"

static errval_t vtd_device_set_root_vnode(struct iommu_device *idev,
                                          struct capref vnode)
{
    errval_t err;

    /* find the domain of the vnode or create a new domain for it*/
    struct vtd_device *dev = (struct vtd_device *)idev;
    struct vtd *vtd = (struct vtd *)idev->iommu;
    assert(vtd);

    bool domain_created = false;
    struct vtd_domain *dom = vtd_domains_get_by_cap(vnode);
    if (dom == NULL) {
        err = vtd_domains_create(vtd, vnode, &dom);
        if (err_is_fail(err)) {
            return err;
        }
        domain_created = true;
    }

    err = vtd_device_remove_from_domain(dev);
    if (err_is_fail(err)) {
        goto err_out;
    }

    /* add the device to the domain */
    err = vtd_domains_add_device(dom, dev);
    if (err_is_fail(err)) {
        goto err_out;
    }

    return SYS_ERR_OK;

    err_out:
    if (domain_created) {
        vtd_domains_destroy(dom);
    }

    return err;
}

errval_t vtd_device_create(struct vtd *vtd, uint16_t seg, uint8_t bus,
                           uint8_t dev, uint8_t fun,
                           struct vtd_device **rdev)
{
    errval_t err;

    assert(rdev);

    struct vtd_ctxt_table *ct;
    err = vtd_ctxt_table_get_by_id(vtd, bus, &ct);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "faled to get the ctxt table\n");
        return err;
    }

    struct vtd_device *vdev = calloc(1, sizeof(*vdev));
    if (vdev == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    vdev->ctxt_table = ct;
    vdev->ctxt_table_idx = iommu_devfn_to_idx(dev, fun);

    /* common init */
    vdev->dev.iommu = &vtd->iommu;
    vdev->dev.addr.pci.segment = seg;
    vdev->dev.addr.pci.bus = bus;
    vdev->dev.addr.pci.device = dev;
    vdev->dev.addr.pci.function = fun;
    vdev->dev.f.set_root = vtd_device_set_root_vnode;

    *rdev = vdev;

    return SYS_ERR_OK;
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


errval_t vtd_device_unmap(struct vtd_ctxt_table *ctxt, struct capref mapping)
{
    errval_t err;

    err = vnode_unmap(ctxt->ctcap, mapping);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}
