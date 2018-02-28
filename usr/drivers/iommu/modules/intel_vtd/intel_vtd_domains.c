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


vtd_domid_t domains_count = 0;
vtd_domid_t domains_free = 0;
vtd_domid_t domains_next = 1;
struct vtd_domain **domains_all = NULL;

#define PCI_DEVICES_MAX 256

static struct vtd_domain_mapping *device_mappings[PCI_DEVICES_MAX] = {0};


static struct vtd_domain_mapping *
vtd_domains_get_device_mapping(uint8_t bus, uint8_t dev, uint8_t fun)
{
    if (device_mappings[bus] == NULL) {
        device_mappings[bus] = calloc(PCI_DEVICES_MAX,
                                      sizeof(struct vtd_domain_mapping));
        for (size_t i = 0; i < PCI_DEVICES_MAX; i++) {
            device_mappings[bus][i].bus = bus;
            device_mappings[bus][i].idx = i;
        }

        assert(device_mappings[bus]);
    }

    return &device_mappings[bus][vtd_dev_fun_to_ctxt_id(dev, fun)];
}


errval_t vtd_domains_init(vtd_domid_t num_domains)
{
    if (domains_all != NULL) {
        return SYS_ERR_OK;
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

#if 0
errval_t vtd_domains_get_for_device(uint8_t bus, uint8_t dev, uint8_t fun,
                                    struct vtd_domain **dom)
{
    struct vtd_domain_mapping *dm = vtd_domains_get_device_mapping(bus, dev, fun);
    if (dm == NULL) {
        return VTD_ERR_DOM_NOT_FOUND;
    }
    if (dm->domain == NULL) {
        return VTD_ERR_DOM_NOT_FOUND;
    }

    if (dom) {
        *dom = dm->domain;
    }

    return SYS_ERR_OK;
}
#endif

errval_t vtd_domains_create(struct vtd_domain **domain, struct capref rootpt)
{
    errval_t err;

    INTEL_VTD_DEBUG("[domains] create new\n");

    vtd_domid_t slot = 0;

    if (domains_free == 0) {
        return VTD_ERR_FULL;
    }

    struct vnode_identity id;
    err = invoke_vnode_identify(rootpt, &id);
    if (err_is_fail(err)) {
        return err_push(err, VTD_ERR_INVALID_CAP);
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

    for (vtd_domid_t i = 0;  i < domains_count; i++) {
        slot = (i + domains_next) % domains_count;
        if (domains_all[slot] == NULL) {
            domains_all[slot] = dom;
            domains_next = (slot + 1) % domains_count;
            domains_free--;
            break;
        }
    }
    assert(slot != 0);

    INTEL_VTD_DEBUG("[domains] allocated domain id %u\n", slot);

    assert(domains_all[slot] == dom);

    dom->id          = slot;
    dom->ptroot      = rootpt;
    dom->ptroot_base = id.base;
    dom->devmappings = NULL;

    return SYS_ERR_OK;
}


errval_t vtd_domains_destroy(struct vtd_domain *domain)
{
    errval_t err;

    INTEL_VTD_DEBUG("[domains] destroying domain %u\n", domain->id);

    assert(domains_all[domain->id] = domain);

    /* we need to remove all mappings from the domain */
    while (domain->devmappings) {
        err = vtd_ctxt_table_unmap(domain->devmappings);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to remove mappings");
        }
        domain->devmappings = domain->devmappings->next;
    }

    domains_all[domain->id] = NULL;
    memset(domain, 0, sizeof(*domain));
    free(domain);

    return SYS_ERR_OK;
}

errval_t vtd_domains_add_device(struct vtd_domain *d, uint8_t bus, uint8_t dev,
                                uint8_t fun)
{
    errval_t err;

    //if (!valid_device(bus, dev, func)) ;
    struct vtd *vtd = vtd_get_for_device(bus, dev, fun);
    if (vtd == NULL) {
        return VTD_ERR_DEV_NOT_FOUND;
    }

    struct vtd_domain_mapping *devmap;
    devmap = vtd_domains_get_device_mapping(bus, dev, fun);
    if (!capref_is_null(devmap->mappingcap)) {
        return VTD_ERR_DEV_USED;
    }

    /* map the devices into the domain */
    err = vtd_ctxt_table_map(&vtd->ctxt_tables[bus], d, devmap);
    if (err_is_fail(err)) {
        return err;
    }

    devmap->domain = d;
    devmap->vtd    = vtd;

    /* store the devmap in the list of devices for this domain */
    d->devmappings->prev = devmap;
    devmap->next         = d->devmappings;
    d->devmappings       = devmap;

    return err;
}

errval_t vtd_domains_remove_device(struct vtd_domain *d, uint8_t bus,
                                   uint8_t dev, uint8_t fun)
{
    errval_t err;

    struct vtd_domain_mapping *devmap;
    devmap = vtd_domains_get_device_mapping(bus, dev, fun);
    if (!capref_is_null(devmap->mappingcap)) {
        return SYS_ERR_OK;
    }

    /* map the devices into the domain */
    err = vtd_ctxt_table_unmap(devmap);
    if (err_is_fail(err)) {
        return err;
    }

    /* remove the devmap from this domain */
    if (devmap->next) {
        devmap->next->prev = devmap->prev;
    }
    if (devmap->prev) {
        devmap->prev->next = devmap->next;
    } else {
        devmap->domain->devmappings = devmap->next;
    }

    return err;
}

#if 0
errval_t vtd_domain_lookup(struct capref rootcap)
{
    err = VTD_ERR_DEV_NOT_FOUND;
}
#endif


#if 0




errval_t vtd_domains_remove_device()
{


    genpaddr_t pt = pml4_base(pml4);
    if (pt == 0) return VTD_ERR_INVALID_CAP;

    // Find the domain in the list of domains
    struct vtd_domain *dom = NULL;
    VTD_FOR_EACH(dom, domains->head) {
        if (dom->pt_gp == pt) break;
    }

    if (dom == NULL) return VTD_ERR_DOM_NOT_FOUND;

    errval_t err =  VTD_ERR_DEV_NOT_FOUND;

    // Find the unit containing the device under its scope
    struct vtd_unit *u = NULL;
    VTD_FOR_EACH(u, dom->units) {
        if (u->pci_seg == seg) {
            vtd_context_entry_array_t *context_table = u->context_tables[bus];
            uint8_t id = (dev << 3) | func;

            // The device doesn't belong to this domain
            if (!vtd_context_entry_p_extract(context_table[id])) {
                return VTD_ERR_DEV_NOT_FOUND;
            }

            vtd_context_entry_p_insert(context_table[id], 0);
            vtd_context_entry_t_insert(context_table[id], 0);
            vtd_context_entry_slptptr_insert(context_table[id], 0);
            vtd_context_entry_did_insert(context_table[id], 0);
            vtd_context_entry_aw_insert(context_table[id], 0);

            // After removing the devices, we perform a context-cache device-selective
            // invalidation followed by an IOTLB domain-selective invalidation.
            int sid = (bus << 16) | id;
            vtd_context_cache_dev_inval(dom, sid, vtd_nomask);
            vtd_iotlb_dom_inval(dom);

            err = SYS_ERR_OK;
        }
    }

    return err;
};


#endif