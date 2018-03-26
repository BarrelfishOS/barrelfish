/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "intel_vtd.h"
#include "intel_vtd_ctxt_cache.h"
#include "../generic/common.h"


vtd_domid_t domains_count = 0;
vtd_domid_t domains_free = 0;
struct vtd_domain **domains_all = NULL;



errval_t vtd_domains_init(uint32_t num_domains)
{
    if (domains_all != NULL) {
        return SYS_ERR_OK;
    }

    if (num_domains > INTEL_VTD_MAX_DOMAINS) {
        INTEL_VTD_DEBUG("[domains] setting domain to %u\n", INTEL_VTD_MAX_DOMAINS);
        num_domains = INTEL_VTD_MAX_DOMAINS;
    }

    INTEL_VTD_DEBUG("[domains] initialize with %u domains\n", num_domains);


    INTEL_VTD_DEBUG("[domains] allocating %zu bytes to hold domains\n",
                    num_domains * sizeof(*domains_all));
    domains_all = calloc(num_domains, sizeof(*domains_all));
    if (domains_all == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    domains_free  = num_domains - 1;
    domains_count = num_domains;

    /* we don't use the first domain */
    domains_all[0] = (void *)0xdeadbeef;

    return SYS_ERR_OK;
}


errval_t vtd_domains_create(struct vtd *vtd, struct capref rootpt,
                            struct vtd_domain **domain)
{
    errval_t err;

    INTEL_VTD_DEBUG("[domains] create new\n");

    vtd_domid_t slot = 0;

    if (domains_free == 0) {
        return IOMMU_ERR_DOM_FULL;
    }

    struct vnode_identity id;
    err = invoke_vnode_identify(rootpt, &id);
    if (err_is_fail(err)) {
        return err_push(err, IOMMU_ERR_INVALID_CAP);
    }

    switch(id.type) {
        case ObjType_VNode_x86_64_pml4:
        case ObjType_VNode_x86_64_pml5:
        case ObjType_VNode_x86_64_pdpt:
            break;
        default:
            return SYS_ERR_VNODE_TYPE;
    }

    struct vtd_domain *dom = calloc(1, sizeof(*dom));
    if (dom == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    for (vtd_domid_t i = 1;  i < domains_count; i++) {
        if (domains_all[i] == NULL) {
            domains_all[slot] = dom;
            slot = i;
            domains_free--;
            break;
        }
    }

    if (domains_all[slot] != dom) {
        free(dom);
        return IOMMU_ERR_DOM_FULL;
    }

    assert(slot != 0);

    INTEL_VTD_DEBUG_DOMAINS("allocated domain id %u with pt 0x%" PRIxGENPADDR "\n",
                            slot, id.base);

    assert(domains_all[slot] == dom);

    dom->id          = slot;
    dom->ptroot      = rootpt;
    dom->ptroot_base = id.base;
    dom->devices     = NULL;

    if (domain) {
        *domain = dom;
    }

    return SYS_ERR_OK;
}


errval_t vtd_domains_destroy(struct vtd_domain *domain)
{
    errval_t err;

    INTEL_VTD_DEBUG("[domains] destroying domain %u\n", domain->id);

    assert(domains_all[domain->id] = domain);

    struct vtd_device *dev = domain->devices;
    while(dev) {
        err = vtd_domains_remove_device(domain, dev);
        if (err_is_fail(err)) {
            return err;
        }

        dev = dev->domain_next;
    }

    domains_all[domain->id] = NULL;
    memset(domain, 0, sizeof(*domain));
    free(domain);

    return SYS_ERR_OK;
}


errval_t vtd_domains_add_device(struct vtd_domain *d, struct vtd_device *dev)
{

    errval_t err;

    assert(dev->ctxt_table);

    INTEL_VTD_DEBUG_DOMAINS("adding %u.%u.%u dom %u\n",
                            dev->dev.addr.pci.bus, dev->dev.addr.pci.device,
                            dev->dev.addr.pci.function, d->id);

    if (dev->domain) {
        return IOMMU_ERR_DEV_USED;
    }

    uintptr_t flags = ((uintptr_t)d->id) << 8;
    /* we don't activate the device tlb for now */
    /* flags |= 1; */

    struct capref mappingcap = {
        .cnode =dev->ctxt_table->mappigncn,
        .slot = dev->ctxt_table_idx
    };

    err = vnode_map(dev->ctxt_table->ctcap, d->ptroot, dev->ctxt_table_idx,
                    flags, 0, 1, mappingcap);
    if (err_is_fail(err)) {
        return err;
    }

    /* invalidate context cache */
    struct vtd *v = (struct vtd *)dev->dev.iommu;
    vtd_ctxt_cache_invalidate_device(v, dev->dev.addr.pci.bus,
                                     dev->dev.addr.pci.device,
                                     dev->dev.addr.pci.function,
                                     d->id);

    dev->domain = d;
    dev->domain_next = d->devices;
    d->devices = dev;

    return SYS_ERR_OK;
}


errval_t vtd_domains_remove_device(struct vtd_domain *d, struct vtd_device *vdev)
{
    errval_t err = SYS_ERR_OK;



    struct capref mappingcap = {
        .cnode = vdev->ctxt_table->mappigncn,
        .slot  = vdev->ctxt_table_idx
    };

    struct vtd_device *dev = d->devices;
    struct vtd_device **prev = &d->devices;
    while(dev) {
        if (dev == vdev) {

            INTEL_VTD_DEBUG_DOMAINS("remove %u.%u.%u found, removing...\n",
                                    vdev->dev.addr.pci.bus,
                                    vdev->dev.addr.pci.device,
                                    vdev->dev.addr.pci.function);

            err = vnode_unmap(dev->ctxt_table->ctcap, mappingcap);
            if (err_is_fail(err)) {
                return err;
            }

            struct vtd *v = (struct vtd *)dev->dev.iommu;
            vtd_ctxt_cache_invalidate_device(v, dev->dev.addr.pci.bus,
                                             dev->dev.addr.pci.device,
                                             dev->dev.addr.pci.function,
                                             d->id);
            *prev = dev->domain_next;
            vdev->domain_next = NULL;
            vdev->domain = NULL;

            return SYS_ERR_OK;
        }
        prev = &dev->domain_next;
        dev = dev->domain_next;
    }

    return IOMMU_ERR_DEV_NOT_FOUND;
}


struct vtd_domain *vtd_domains_get_by_id(vtd_domid_t id)
{
    if (id < domains_count) {
        return domains_all[id];
    }
    return NULL;
}


struct vtd_domain *vtd_domains_get_by_cap(struct capref rootpt)
{
    errval_t err;

    struct vnode_identity id;
    err = invoke_vnode_identify(rootpt, &id);
    if (err_is_fail(err)) {
        return NULL;
    }

    for (vtd_domid_t i = 1; i < domains_count; i++) {
        if (domains_all[i]->ptroot_base == id.base) {
            return domains_all[i];
        }
    }

    return NULL;
}

