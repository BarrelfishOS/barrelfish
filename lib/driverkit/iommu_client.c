/**
 * \file
 * \brief DriverKit IOMMU Client
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

#include <if/iommu_defs.h>
#include <if/iommu_rpcclient_defs.h>

#include "debug.h"

static bool iommu_enabled = false;

struct iommu_binding *iommu_binding = NULL;

static void iommu_bind_cb(void *st,  errval_t err, struct iommu_binding *ib)
{
    errval_t *binderr = st;

    if (err_is_fail(err)) {
        *binderr = err;
        iommu_binding = (void *)0xdeadbeef;
        return;
    }

    iommu_rpc_client_init(ib);
    iommu_binding = ib;
}

errval_t driverkit_iommu_client_init(void)
{
    errval_t err;

    debug_printf("[iommu client] Connecting to SKB.\n");

    err = skb_client_connect();
    if (err_is_fail(err)) {
        return err;
    }

    debug_printf("[iommu client] Execute query iommu_enabled(0,_).\n");

    err = skb_execute_query("iommu_enabled(0,_).");
    if (err_is_fail(err)) {
        iommu_enabled = false;
    } else {
        iommu_enabled = true;
    }

    debug_printf("[iommu client] IOMMU is %s.\n",
                 iommu_enabled ? "Enabled" : "Disabled");

    if (!iommu_enabled) {
        return SYS_ERR_OK;
    }

    debug_printf("[iommu client] looking up '%s'\n", DRIVERKIT_IOMMU_SERVICE_NAME);

    iref_t iref;
    err = nameservice_blocking_lookup(DRIVERKIT_IOMMU_SERVICE_NAME, &iref);
    if (err_is_fail(err)) {
        return err;
    }

    debug_printf("[iommu client] Binding to service at %" PRIuIREF "\n", iref);

    errval_t binderr;
    struct waitset *ws = get_default_waitset();
    err = iommu_bind(iref, iommu_bind_cb, (void *)&binderr, ws,
                     IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to bind to iommu service\n");
        return err;
    }

    while(iommu_binding == NULL) {
        event_dispatch(ws);
    }

    debug_printf("[iommu client] Binding %s \n", err_getstring(binderr));

    if (err_is_fail(binderr)) {
        return binderr;
    }


    return SYS_ERR_OK;
}

bool driverkit_iommu_present(void)
{
    return iommu_enabled;
}

static bool root_pt_valid(struct capref rootpt)
{
    errval_t err;

    struct vnode_identity vi;
    err = invoke_vnode_identify(rootpt, &vi);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to get vnode identity\n");
    }
    switch(vi.type) {
        case ObjType_VNode_x86_64_pml4 :
        case ObjType_VNode_x86_64_pml5 :
            return true;
        default:
            return false;
    }
}



errval_t driverkit_iommu_create_domain(struct capref rootpt)
{
    errval_t err, msgerr;

    assert(iommu_binding);

    debug_printf("[iommu client] Creating a new domain.\n");

    if (!root_pt_valid(rootpt)) {
        return SYS_ERR_VNODE_TYPE;
    }

    err = iommu_binding->rpc_tx_vtbl.create_domain(iommu_binding, rootpt, &msgerr);
    if (err_is_fail(err)) {
        return err;
    }

    return msgerr;
}

errval_t driverkit_iommu_delete_domain(struct capref rootpt)
{
    errval_t err, msgerr;

    assert(iommu_binding);

    debug_printf("[iommu client] Creating a new domain.\n");

    if (!root_pt_valid(rootpt)) {
        return SYS_ERR_VNODE_TYPE;
    }

    err = iommu_binding->rpc_tx_vtbl.delete_domain(iommu_binding, rootpt, &msgerr);
    if (err_is_fail(err)) {
        return err;
    }

    return msgerr;
}

errval_t driverkit_iommu_add_device(struct capref rootpt, struct capref dev)
{
    errval_t err, msgerr;

    assert(iommu_binding);

    debug_printf("[iommu client] Adding device to domain\n");

    err = iommu_binding->rpc_tx_vtbl.add_device(iommu_binding, rootpt, dev, &msgerr);
    if (err_is_fail(err)) {
        return err;
    }

    return msgerr;
}

errval_t driverkit_iommu_remove_device(struct capref rootpt, struct capref dev)
{
    errval_t err, msgerr;

    assert(iommu_binding);
    debug_printf("[iommu client] Remove device\n");

    err = iommu_binding->rpc_tx_vtbl.add_device(iommu_binding, rootpt, dev, &msgerr);
    if (err_is_fail(err)) {
        return err;
    }

    return msgerr;
}


