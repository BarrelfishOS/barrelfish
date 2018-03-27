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


static errval_t vtd_device_set_root_vnode(struct iommu_device *idev,
                                          struct capref vnode)
{
    errval_t err;

    assert(idev);

    INTEL_VTD_DEBUG_DEVICES("setting root vnode for %u.%u.%u\n",
                            idev->addr.pci.bus, idev->addr.pci.device,
                            idev->addr.pci.function);

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

    INTEL_VTD_DEBUG_DEVICES("added device %u.%u.%u to domain %u\n",
                            idev->addr.pci.bus, idev->addr.pci.device,
                            idev->addr.pci.function, dom->id);

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

    INTEL_VTD_DEBUG_DEVICES("create %u.%u.%u\n", bus, dev, fun);

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

    INTEL_VTD_DEBUG_DEVICES("created %u.%u.%u @ %p\n", bus, dev, fun, vdev);

    return SYS_ERR_OK;
}


errval_t vtd_device_destroy(struct vtd_device *dev)
{
    errval_t err;

    INTEL_VTD_DEBUG_DEVICES("destroy device %p\n", dev);

    err = vtd_device_remove_from_domain(dev);
    if (err_is_fail(err)) {
        return err;
    }

    memset(dev, 0, sizeof(*dev));

    return SYS_ERR_OK;
}


errval_t vtd_device_remove_from_domain(struct vtd_device *dev)
{
    INTEL_VTD_DEBUG_DEVICES("remove from domain %p\n", dev);

    if (dev->domain == NULL) {
        return SYS_ERR_OK;
    }

    return vtd_domains_remove_device(dev->domain, dev);
}
