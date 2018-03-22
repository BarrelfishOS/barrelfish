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

struct iommu_bind_st
{
    bool     done;
    errval_t err;
};

static void iommu_bind_cb(void *argst,  errval_t err, struct iommu_binding *ib)
{
    DRIVERKIT_DEBUG("[iommu client] bound to service: %s\n", err_getstring(err));

    struct iommu_bind_st *st = argst;

    st->done = true;
    st->err = err;

    if (err_is_ok(err)) {
        iommu_rpc_client_init(ib);
        iommu_binding = ib;
    }
}

errval_t driverkit_iommu_client_init_with_endpoint(struct capref ep)
{
    errval_t err;

    DRIVERKIT_DEBUG("[iommu client] Connecting to SKB.\n");

    struct waitset *ws = get_default_waitset();

    err = skb_client_connect();
    if (err_is_fail(err)) {
        return err;
    }

    DRIVERKIT_DEBUG("[iommu client] Execute query iommu_enabled(0,_).\n");

    err = skb_execute_query("iommu_enabled(0,_).");
    if (err_is_fail(err)) {
        iommu_enabled = false;
        debug_printf("IOMMU Endpoint provided but IOMMU not enabled ?");
    } else {
        iommu_enabled = true;
    }

    struct iommu_bind_st st = {.done = 0, .err = SYS_ERR_OK};
    err = iommu_bind_to_endpoint(ep, iommu_bind_cb, &st, ws,
                                 IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    if (err_is_fail(err)) {
        return err;
    }

    while(!st.done) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to dispatch the event\n");
        }
    }

    return err;
}

#include <driverkit/driverkit.h>

errval_t driverkit_iommu_client_init(void)
{
    DRIVERKIT_DEBUG("[iommu client] IOMMU is %s.\n",
                    iommu_enabled ? "Enabled" : "Disabled");

    if (!iommu_enabled) {
        return SYS_ERR_OK;
    }

    struct capref ep = NULL_CAP;
    USER_PANIC("GETTING THE ENDPOINT CAP!\n");

    return driverkit_iommu_client_init_with_endpoint(ep);
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


/*
 * ===========================================================================
 *
 * ===========================================================================
 */

errval_t driverkit_iommu_set_root(struct capref rootvnode)
{
    errval_t err;

    if (!root_pt_valid(rootvnode)) {
        return SYS_ERR_VNODE_TYPE;
    }

    errval_t rpcerr;
    err = iommu_binding->rpc_tx_vtbl.setroot(iommu_binding, rootvnode, &rpcerr);
    if (err_is_fail(err)) {
        return err;
    }

    return rpcerr;
}


