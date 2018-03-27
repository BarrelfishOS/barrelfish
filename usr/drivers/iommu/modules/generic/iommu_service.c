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
#include <if/pci_iommu_defs.h>
#include <if/pci_iommu_rpcclient_defs.h>


#include "common.h"

#include "../intel_vtd/intel_vtd.h"

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

#define IOMMU_SVC_DEBUG(x...) debug_printf("[iommu] [svc] " x)

/*
 * ===========================================================================
 * Receive Handlers of the servie
 * ===========================================================================
 */
static void getvmconfig_request(struct iommu_binding *ib)
{
    errval_t err;

    IOMMU_SVC_DEBUG("%s\n", __FUNCTION__);

    struct iommu_device *idev = ib->st;
    assert(idev);
    assert(idev->iommu);

    err = ib->tx_vtbl.getvmconfig_response(ib, NOP_CONT, SYS_ERR_OK,
                                           idev->iommu->root_vnode_type,
                                           idev->iommu->max_page_bits);
    /* should not fail */
    assert(err_is_ok(err));
}


static void setroot_request(struct iommu_binding *ib, struct capref src)
{
    errval_t err;

    IOMMU_SVC_DEBUG("%s\n", __FUNCTION__);

    struct iommu_device *idev = ib->st;
    assert(idev);
    assert(idev->iommu);

    if (idev->f.set_root) {
        err = idev->f.set_root(idev, src);
    } else {
        err = IOMMU_ERR_NOT_SUPPORTED;
    }

    err = ib->tx_vtbl.setroot_response(ib, NOP_CONT, err);
    /* should not fail */
    assert(err_is_ok(err));
}


static void  retype_request(struct iommu_binding *ib, struct capref src,
                            uint8_t objtype)
{
    errval_t err;

    struct capref retcap = NULL_CAP;
    struct frame_identity id;

    IOMMU_SVC_DEBUG("%s\n", __FUNCTION__);

    switch(objtype) {
        case ObjType_VNode_x86_64_ptable :
        case ObjType_VNode_x86_64_pdir :
        case ObjType_VNode_x86_64_pdpt :
        case ObjType_VNode_x86_64_pml4 :
        case ObjType_VNode_x86_64_pml5 :

            err = invoke_frame_identify(src, &id);
            if (err_is_fail(err)) {
                err = err_push(err, LIB_ERR_CAP_INVOKE);
                goto send_reply;
            }

            /* we should be the only one that has it */
            err = cap_revoke(src);
            if (err_is_fail(err)) {
                err = err_push(err, LIB_ERR_CAP_DELETE_FAIL);
                goto send_reply;
            }

            /* allocate slot to store the new cap */
            err = slot_alloc(&retcap);
            if (err_is_fail(err)) {
                err = err_push(err, LIB_ERR_SLOT_ALLOC);
                retcap = src;
                goto send_reply;
            }

            /* retype it to a page table */
            err = cap_retype(retcap, src, 0, objtype, id.bytes, 1);
            if (err_is_fail(err)) {
                err = err_push(err, LIB_ERR_CAP_RETYPE);
                slot_free(retcap);
                retcap = src;
                goto send_reply;
            }

            /*
             * TODO: mint to readonly & store original
             */

            /* delete the source cap */
            cap_destroy(src);

            break;
        default:
            err = SYS_ERR_VNODE_TYPE;

    }
    send_reply:
    err = ib->tx_vtbl.retype_response(ib, NOP_CONT, err, retcap);
    assert(err_is_ok(err)); /* should not fail */
}


static void map_request(struct iommu_binding *ib, struct capref vnode_ro,
                        uint16_t slot, struct capref src)
{
    errval_t err;
    IOMMU_SVC_DEBUG("%s\n", __FUNCTION__);

    struct capref vnode;
    err = iommu_get_writable_vnode(vnode_ro, &vnode);
    if(err_is_fail(err)) {
        goto out;
    }

    uint64_t attr = 0;

    struct capref mapping;
    err = vnode_map(vnode, src, slot, attr, 0, 1, mapping);

    out:
    // delete the cap, we no longer need it
    cap_destroy(vnode_ro);

    err = ib->tx_vtbl.map_response(ib, NOP_CONT, LIB_ERR_NOT_IMPLEMENTED);
    assert(err_is_ok(err)); /* should not fail */
}


static void unmap_request(struct iommu_binding *ib, struct capref vnode_ro,
                          uint16_t slot)
{
    errval_t err;
    IOMMU_SVC_DEBUG("%s\n", __FUNCTION__);

    struct capref vnode;
    err = iommu_get_writable_vnode(vnode_ro, &vnode);
    if(err_is_fail(err)) {
        goto out;
    }

    /// XXX: we need to get the mapping cap somehow
    err = vnode_unmap(vnode, NULL_CAP);

    out:
    // delete the cap, we no longer need it
    cap_destroy(vnode_ro);

    err = ib->tx_vtbl.unmap_response(ib, NOP_CONT, LIB_ERR_NOT_IMPLEMENTED);
    assert(err_is_ok(err)); /* should not fail */
}

static void modify_request(struct iommu_binding *ib, struct capref vnode_ro,
                           uint64_t attr, uint16_t slot)
{
    errval_t err;
    IOMMU_SVC_DEBUG("%s\n", __FUNCTION__);

    struct capref vnode;
    err = iommu_get_writable_vnode(vnode_ro, &vnode);
    if(err_is_fail(err)) {
        goto out;
    }

    err = invoke_vnode_modify_flags(vnode, slot, 1, attr);

    out:
    // delete the cap, we no longer need it
    cap_destroy(vnode_ro);

    err = ib->tx_vtbl.modify_response(ib, NOP_CONT, err);
    assert(err_is_ok(err)); /* should not fail */
}

errval_t iommu_service_init(void)
{
    IOMMU_SVC_DEBUG("%s\n", __FUNCTION__);


    return SYS_ERR_OK;
}

static struct iommu_rx_vtbl rx_vtbl = {
        .getvmconfig_call = getvmconfig_request,
        .setroot_call = setroot_request,
        .retype_call = retype_request,
        .map_call = map_request,
        .unmap_call = unmap_request,
        .modify_call = modify_request,
};

/**
 * @brief creates a new endpoint for the IOMMU service
 *
 * @param ep    capref to create the EP in, slot must be allocated
 * @param dev   the pointer to the iommu device for this endpoint
 * @param type  what endpoint type to allcate
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t iommu_service_new_endpoint(struct capref ep, struct iommu_device *dev,
                                    idc_endpoint_t type)
{
    struct waitset *ws = get_default_waitset();
    return iommu_create_endpoint(type, &rx_vtbl, dev, ws,
                                 IDC_ENDPOINT_FLAGS_DEFAULT, &dev->binding, ep);
}

/*****************************************************************
 * Iommu PCI connection interface
 *****************************************************************/


static void request_iommu_endpoint_handler(struct pci_iommu_binding *b, uint8_t type, 
                                           uint32_t segment, uint32_t bus, 
                                           uint32_t device, uint32_t function)
{
    errval_t err, out_err;

    assert(b->st);
    struct iommu *io = (struct iommu *)b->st;

    struct iommu_device* dev;
    out_err = iommu_device_create_by_pci(io, segment, bus, device, function, &dev);
    if (err_is_fail(out_err)) {
        goto reply;
    }

    struct capref cap;
    out_err = slot_alloc(&cap);
    if (err_is_fail(out_err)) {
        iommu_device_destroy(dev);
        goto reply;
    }

    out_err = iommu_service_new_endpoint(cap, dev, type);
    if (err_is_fail(out_err)) {
        slot_free(cap);
        iommu_device_destroy(dev);
    }

reply:
    err = b->tx_vtbl.request_iommu_endpoint_response(b, NOP_CONT, cap, out_err);
    assert(err_is_ok(err));
}

struct pci_iommu_rx_vtbl pci_iommu_rx_vtbl = {
    .request_iommu_endpoint_call = request_iommu_endpoint_handler
};


struct iommu_wrapper {
    struct iommu* iommu;
    bool bound;
    errval_t err;    
};

static void bind_cont(void *st, errval_t err, struct pci_iommu_binding *b)
{
    struct iommu_wrapper* wrap = (struct iommu_wrapper*) st;
    if (err_is_fail(err)) {
        wrap->err = err;    
        return;
    }

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = pci_iommu_rx_vtbl;
    pci_iommu_rpc_client_init(b);
    b->st = wrap->iommu;

    wrap->bound = true;
}

errval_t iommu_bind_to_pci(struct capref ep, struct iommu* iommu)
{
    errval_t err;
    struct iommu_wrapper wrap;

    wrap.iommu = iommu;
    wrap.bound = false;
    wrap.err = SYS_ERR_OK;

    err = pci_iommu_bind_to_endpoint(ep, bind_cont, &wrap, get_default_waitset(), 
                                     IDC_BIND_FLAGS_DEFAULT);
    while(!wrap.bound) {
        event_dispatch(get_default_waitset());
    }

    return err;
}
