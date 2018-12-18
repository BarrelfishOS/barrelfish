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



#include "common.h"

#include "../intel_vtd/intel_vtd.h"


//#define IOMMU_SVC_DEBUG(x...) debug_printf("[iommu] [svc] " x)
#define IOMMU_SVC_DEBUG(x...)


static struct vnodest *vnodes_pml4;
static struct vnodest *vnodes_pdpt;
static struct vnodest *vnodes_pdir;



/**
 * @brief obtains the writable version of the cap
 *
 * @param in    the readonly cap
 * @param out   the writable version
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
static inline errval_t iommu_get_writable_vnode(struct vnode_identity id,
                                                struct capref *out)
{
    struct vnodest *vn;
    switch(id.type) {
        case ObjType_VNode_x86_64_pml4 :
            vn = vnodes_pml4;
            break;
        case ObjType_VNode_x86_64_pdpt :
            vn = vnodes_pdpt;
            break;
        case ObjType_VNode_x86_64_pdir :
            vn = vnodes_pdir;
            break;
        default:
            return SYS_ERR_VNODE_TYPE;
    }

    while(vn) {
        if (vn->id.type == id.type && vn->id.base == id.base) {
            *out = vn->cap;
            return SYS_ERR_OK;
        }
        vn = vn->next;
    }

    return SYS_ERR_CAP_NOT_FOUND;
}

static inline errval_t iommu_put_writable_vnode(struct vnode_identity id,
                                                struct capref in)
{
    struct vnodest *vn = calloc(1, sizeof(*vn));
    if (vn == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    vn->cap = in;
    vn->id = id;

    switch(id.type) {
        case ObjType_VNode_x86_64_pml4 :
            vn->next = vnodes_pml4;
            vnodes_pml4 = vn;
            break;
        case ObjType_VNode_x86_64_pdpt :
            vn->next = vnodes_pdpt;
            vnodes_pdpt = vn;
            break;
        case ObjType_VNode_x86_64_pdir :
            vn->next = vnodes_pdir;
            vnodes_pdir = vn;
            break;
        default:
            return SYS_ERR_VNODE_TYPE;
    }

    return SYS_ERR_OK;
}



/*
 * ===========================================================================
 * Receive Handlers of the servie
 * ===========================================================================
 */
static void getvmconfig_request(struct iommu_binding *ib)
{
    errval_t err = SYS_ERR_OK;
    int32_t nodeid = 1;

    IOMMU_SVC_DEBUG("%s\n", __FUNCTION__);

    struct iommu_device *idev = ib->st;
    assert(idev);
    assert(idev->iommu);

    err = skb_execute_query(
        "node_enum(addr(%i,%i,%i), Enum), write(Enum).",
        idev->addr.pci.bus,
        idev->addr.pci.device,
        idev->addr.pci.function);

    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err,"pci node id lookup");
    } else {
        err = skb_read_output("%"SCNi32, &nodeid);
    }

    err = ib->tx_vtbl.getvmconfig_response(ib, NOP_CONT, err,
                                           idev->iommu->root_vnode_type,
                                           idev->iommu->max_page_bits, nodeid);
    /* should not fail */
    assert(err_is_ok(err));
}


static void setroot_request(struct iommu_binding *ib, struct capref src)
{
    errval_t err;

    struct iommu_device *idev = ib->st;
    assert(idev);
    assert(idev->iommu);

    IOMMU_SVC_DEBUG("%s [%u][%p] %u.%u.%u\n", __FUNCTION__, idev->iommu->id,
                    idev->iommu, idev->addr.pci.bus, idev->addr.pci.device,
                    idev->addr.pci.function);

    struct vnode_identity id;
    err = invoke_vnode_identify(src, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed");
    }
    switch(id.type) {
        case ObjType_VNode_x86_64_pml4 :
            IOMMU_SVC_DEBUG("%s. PML4 @ 0x%lx as root vnode\n", __FUNCTION__, id.base);
            break;
        case ObjType_VNode_x86_64_pdpt :
            IOMMU_SVC_DEBUG("%s. PDPT @ 0x%lx as root vnode\n", __FUNCTION__, id.base);
            break;
        case ObjType_VNode_x86_64_pdir :
            IOMMU_SVC_DEBUG("%s. PDIR @ 0x%lx as root vnode\n", __FUNCTION__, id.base);
            break;
        case ObjType_VNode_VTd_ctxt_table :
            IOMMU_SVC_DEBUG("%s. CTXT @ 0x%lx as root vnode\n", __FUNCTION__, id.base );
            break;
    }

    if (idev->f.set_root) {
        err = idev->f.set_root(idev, src);
    } else {
        err = IOMMU_ERR_NOT_SUPPORTED;
    }

    err = ib->tx_vtbl.setroot_response(ib, NOP_CONT, err);
    /* should not fail */
    assert(err_is_ok(err));
}


static void sent_notification(void *arg)
{
    bool *state = arg;
    *state = true;
}

static void  retype_request(struct iommu_binding *ib, struct capref src,
                            uint8_t objtype)
{
    errval_t err;

    struct capref retcap = NULL_CAP;
    struct capref vnode = NULL_CAP;
    struct frame_identity id;

    IOMMU_SVC_DEBUG("%s\n", __FUNCTION__);


    switch(objtype) {
        case ObjType_VNode_x86_64_ptable :
        case ObjType_VNode_x86_64_pdir :
        case ObjType_VNode_x86_64_pdpt :
        case ObjType_VNode_x86_64_pml4 :
        case ObjType_VNode_x86_64_pml5 :

            err = frame_identify(src, &id);
            if (err_is_fail(err)) {
                err = err_push(err, LIB_ERR_CAP_INVOKE);
                goto send_reply;
            }

            IOMMU_SVC_DEBUG("%s retype 0x%" PRIxGENPADDR " to %d\n", __FUNCTION__,
                             id.base, objtype);

            /* we should be the only one that has it */
            err = cap_revoke(src);
            if (err_is_fail(err)) {
                err = err_push(err, LIB_ERR_CAP_DELETE_FAIL);
                goto send_reply;
            }

            /* allocate slot to store the new cap */
            err = slot_alloc(&vnode);
            if (err_is_fail(err)) {
                err = err_push(err, LIB_ERR_SLOT_ALLOC);
                goto out_err;
            }

            /* allocate slot to store readonly version of the cap */
            err = slot_alloc(&retcap);
            if (err_is_fail(err)) {
                err = err_push(err, LIB_ERR_SLOT_ALLOC);
                goto out_err2;
            }

            /* retype it to a page table */
            err = cap_retype(vnode, src, 0, objtype, vnode_objsize(objtype), 1);
            if (err_is_fail(err)) {
                err = err_push(err, LIB_ERR_CAP_RETYPE);
                goto out_err2;
            }

            struct vnode_identity vid;
            err = invoke_vnode_identify(vnode, &vid);
            assert(err_is_ok(err)); /// should not fail

        #define IOMMU_VNODE_CAPRIGHTS (CAPRIGHTS_READ | CAPRIGHTS_GRANT | CAPRIGHTS_IDENTIFY)

            err = cap_mint(retcap, vnode, IOMMU_VNODE_CAPRIGHTS, 0x0);
            if (err_is_fail(err)) {
                goto out_err3;
            }

            err = iommu_put_writable_vnode(vid, vnode);
            if(err_is_fail(err)) {
                err = err_push(err, LIB_ERR_CAP_RETYPE);
                goto out_err4;
            }

            /* delete the source cap */
            cap_destroy(src);

            break;
        default:
            err = SYS_ERR_VNODE_TYPE;

    }

    bool issent = false;
    struct event_closure cont;
    if (err_is_fail(err)) {
        cont = NOP_CONT;
    } else {
        cont = MKCLOSURE(sent_notification, (void *)&issent);
    }


    send_reply:
    err = ib->tx_vtbl.retype_response(ib, cont, err, retcap);
    assert(err_is_ok(err)); /* should not fail */

    while(!issent && err_is_ok(err)) {
        event_dispatch(get_default_waitset());
    }

    cap_destroy(retcap);

    return;

    out_err4:
    cap_destroy(retcap);
    out_err3:
    cap_destroy(vnode);
    out_err2:
    slot_free(retcap);
    out_err:
    slot_free(vnode);
    retcap = src;
    goto send_reply;
}


static void map_request(struct iommu_binding *ib, struct capref dst,
                        struct capref src, uint16_t slot, uint64_t attr,
                        uint64_t off, uint64_t pte_count)
{
    errval_t err;

    struct iommu_device *dev = ib->st;
    assert(dev);

    IOMMU_SVC_DEBUG("%s [%u][%p] %u.%u.%u\n", __FUNCTION__, dev->iommu->id,
                    dev->iommu, dev->addr.pci.bus, dev->addr.pci.device,
                    dev->addr.pci.function);

    if (dev->f.map == NULL) {
        err = IOMMU_ERR_NOT_SUPPORTED;
        goto out;
    }

    struct vnode_identity id;
    err = invoke_vnode_identify(dst, &id);
    if (err_is_fail(err)) {
        goto out;
    }

    switch(id.type) {
        case ObjType_VNode_x86_64_pml4 :
            IOMMU_SVC_DEBUG("%s. PML4 @ 0x%lx slot [%u..%lu] flags[%lx]\n", __FUNCTION__, id.base, slot, slot+pte_count - 1, attr);
            break;
        case ObjType_VNode_x86_64_pdpt :
            IOMMU_SVC_DEBUG("%s. PDPT @ 0x%lx slot [%u..%lu] flags[%lx]\n", __FUNCTION__, id.base, slot, slot+pte_count - 1, attr );
            break;
        case ObjType_VNode_x86_64_pdir :
            IOMMU_SVC_DEBUG("%s. PDIR @ 0x%lx slot [%u..%lu] flags[%lx]\n", __FUNCTION__, id.base, slot, slot+pte_count - 1, attr);
            break;
        case ObjType_VNode_VTd_ctxt_table :
            IOMMU_SVC_DEBUG("%s. CTXT @ 0x%lx slot [%u..%lu] flags[%lx]\n", __FUNCTION__, id.base, slot, slot+pte_count - 1, attr);
            break;
    }

    struct capref vnode;
    err = iommu_get_writable_vnode(id, &vnode);
    if(err_is_fail(err)) {
        goto out;
    }

    struct capref mapping;
    err = slot_alloc(&mapping);
    if (err_is_fail(err)) {
        goto out;
    }

    err = dev->f.map(dev, vnode, src, slot, attr, off, pte_count, mapping);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to map the frame\n");
    }

    out:
    // delete the cap, we no longer need it
    cap_destroy(dst);

    err = ib->tx_vtbl.map_response(ib, NOP_CONT, err);
    assert(err_is_ok(err)); /* should not fail */
}


static void unmap_request(struct iommu_binding *ib, struct capref vnode_ro,
                          uint16_t slot)
{
    errval_t err;

    struct iommu_device *dev = ib->st;
    assert(dev);

    IOMMU_SVC_DEBUG("%s %u.%u.%u\n", __FUNCTION__, dev->addr.pci.bus,
                    dev->addr.pci.device, dev->addr.pci.function);

    if (dev->f.unmap == NULL) {
        err = IOMMU_ERR_NOT_SUPPORTED;
        goto out;
    }

    struct vnode_identity id;
    err = invoke_vnode_identify(vnode_ro, &id);
    if (err_is_fail(err)) {
        goto out;
    }

    switch(id.type) {
        case ObjType_VNode_x86_64_pml4 :
            IOMMU_SVC_DEBUG("%s. PML4 @ 0x%lx slot [%u]\n", __FUNCTION__, id.base, slot);
            break;
        case ObjType_VNode_x86_64_pdpt :
            IOMMU_SVC_DEBUG("%s. PDPT @ 0x%lx slot [%u]\n", __FUNCTION__, id.base, slot );
            break;
        case ObjType_VNode_x86_64_pdir :
            IOMMU_SVC_DEBUG("%s. PDIR @ 0x%lx slot [%u]\n", __FUNCTION__, id.base, slot);
            break;
        case ObjType_VNode_VTd_ctxt_table :
            IOMMU_SVC_DEBUG("%s. CTXT @ 0x%lx slot [%u]\n", __FUNCTION__, id.base, slot );
            break;
    }

    struct capref vnode;
    err = iommu_get_writable_vnode(id, &vnode);
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


    struct vnode_identity id;
    err = invoke_vnode_identify(vnode_ro, &id);
    if (err_is_fail(err)) {
        goto out;
    }

    switch(id.type) {
        case ObjType_VNode_x86_64_pml4 :
            IOMMU_SVC_DEBUG("%s. PML4 @ 0x%lx slot [%u]\n", __FUNCTION__, id.base, slot);
            break;
        case ObjType_VNode_x86_64_pdpt :
            IOMMU_SVC_DEBUG("%s. PDPT @ 0x%lx slot [%u]\n", __FUNCTION__, id.base, slot );
            break;
        case ObjType_VNode_x86_64_pdir :
            IOMMU_SVC_DEBUG("%s. PDIR @ 0x%lx slot [%u]\n", __FUNCTION__, id.base, slot);
            break;
        case ObjType_VNode_VTd_ctxt_table :
            IOMMU_SVC_DEBUG("%s. CTXT @ 0x%lx slot [%u]\n", __FUNCTION__, id.base, slot );
            break;
    }

    struct capref vnode;
    err = iommu_get_writable_vnode(id, &vnode);
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

    IOMMU_SVC_DEBUG("%s [%u][%p] %u.%u.%u\n", __FUNCTION__, io->id, io, bus, device, function);

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

errval_t iommu_request_endpoint(uint8_t type, struct capref* cap, struct iommu* iommu)
{
    struct pci_iommu_binding* b;
    return pci_iommu_create_endpoint(type, &pci_iommu_rx_vtbl, iommu, get_default_waitset(), 
                                     IDC_ENDPOINT_FLAGS_DEFAULT, &b, *cap);
}
