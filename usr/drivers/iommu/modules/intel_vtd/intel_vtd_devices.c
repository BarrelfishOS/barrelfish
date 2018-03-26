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

static errval_t vtd_device_set_root_table(struct iommu_device *idev,
                                          struct capref src)
{
    /* find the domain */

    /* */

    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t vtd_device_create(struct vtd *vtd, uint16_t seg, uint8_t bus,
                           uint8_t dev, uint8_t fun,
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

    /* common init */
    vdev->dev.iommu = &vtd->iommu;
    vdev->dev.addr.pci.segment = seg;
    vdev->dev.addr.pci.bus = bus;
    vdev->dev.addr.pci.device = dev;
    vdev->dev.addr.pci.function = fun;
    vdev->dev.f.set_root = vtd_device_set_root_table;

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





errval_t vtd_device_map(struct vtd_ctxt_table *ctxt, uint8_t idx,
                            struct vtd_domain *dom, struct capref *mapping)
{
    errval_t err;

    struct vnode_identity id;
    err = invoke_vnode_identify(dom->ptroot, &id);
    if (err_is_fail(err)) {
        return err_push(err, IOMMU_ERR_INVALID_CAP);
    }

    switch(id.type) {
        case ObjType_VNode_x86_32_pdpt:
        case ObjType_VNode_x86_64_pml4:
        case ObjType_VNode_x86_64_pml5:
            break;
        default:
            return SYS_ERR_VNODE_TYPE;
    }

    uintptr_t flags = (uintptr_t)vtd_domains_get_id(dom) << 8;
    if (vtd_device_tlb_present(ctxt->root_table->vtd)) {
        flags |= 1;
    }

    ///tlb enabled

    struct capref mappingcap = {
            .cnode =ctxt->mappigncn,
            .slot = idx
    };

    debug_printf("[vtd] [ctxt] Mapping domaind %u in ctxttable %u\n",
                 dom->id, ctxt->root_table_idx);

    err = vnode_map(ctxt->ctcap, dom->ptroot, idx, flags, 0, 1,
                    mappingcap);
    if (err_is_fail(err))  {
        return err;
    }

    *mapping = mappingcap;

    return SYS_ERR_OK;
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
