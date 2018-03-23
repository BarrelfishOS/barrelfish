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
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>
#include <skb/skb.h>

#include <if/iommu_defs.h>
#include <if/iommu_rpcclient_defs.h>

#include "debug.h"

struct iommu_client
{
    ///< whether the IOMMU is enabled or not
    bool enabled;

    ///< the binding to the IOMMU service
    struct iommu_binding *binding;

    ///< endpoint to the iommu service
    struct capref endpoint;

    ///< the waitset to be used
    struct waitset *waitset;

    ///< ObjType of the root vnode
    enum objtype root_vnode_type;

    ///< the maximum supported page size
    size_t max_page_size;

    ///< error value for async errors
    errval_t error;

    ///< the capability to the root vnode
    struct capref rootvnode;
};

///< the default iommu client
static struct iommu_client *default_client;


static void iommu_bind_cb(void *argst,  errval_t err, struct iommu_binding *ib)
{
    DRIVERKIT_DEBUG("[iommu client] bound to service: %s\n", err_getstring(err));

    struct iommu_client *st = argst;

    if (err_is_ok(err)) {
        iommu_rpc_client_init(ib);
        st->binding = ib;
    }
    st->error = err;
}



/**
 * @brief initializes the IOMMU client library with the IOMMU endpoint
 *
 * @param ep the IOMMU endpoint
 * @param cl returns a pointer ot the iommu client
 *
 * @return SYS_ERR_OK on success, errval on failure
 *
 * This function initializes the connection, allocates the root vnode etc.
 */
errval_t driverkit_iommu_client_init(struct capref ep, struct iommu_client **cl)
{
    errval_t err;

    assert(cl);

    struct iommu_client *icl;
    err = driverkit_iommu_client_connect(ep, &icl);
    if (err_is_fail(err)) {
        return err;
    }

    errval_t msgerr;
    uint8_t type, bits;
    err = icl->binding->rpc_tx_vtbl.getvmconfig(icl->binding, &msgerr, &type, &bits);
    if (err_is_fail(err)) {
        driverkit_iommu_client_disconnect(icl);
        goto err_out;
    }

    if (err_is_fail(msgerr)) {
        err = msgerr;
        goto err_out;
    }

    icl->root_vnode_type = (enum objtype)type;
    icl->max_page_size = (1UL << bits);

    /* allocate memory for the vnode */

    err = driverkit_iommu_alloc_vnode(icl, icl->root_vnode_type ,
                                      &icl->rootvnode);
    if (err_is_fail(err)) {
        goto err_out;
    }

    err = driverkit_iommu_set_root_vnode(icl, icl->rootvnode);
    if (err_is_fail(err)) {
        /* make use of the capability protocol to release the resources */
        cap_revoke(icl->rootvnode);
        cap_destroy(icl->rootvnode);
        goto err_out;
    }

    *cl = icl;

    return SYS_ERR_OK;

    err_out:
    driverkit_iommu_client_disconnect(icl);
    return err;
}


/**
 * @brief connects to the IOMMU service
 *
 * @param ep the IOMMU endpoint
 * @param cl returns a pointer ot the iommu client
 *
 * @return SYS_ERR_OK on success, errval on failure
 *
 * This just initializes the connecton to the IOMMU
 */
errval_t driverkit_iommu_client_connect(struct capref ep,
                                        struct iommu_client **cl)
{
    errval_t err;

    assert(cl);

    struct endpoint_identity id;
    err = invoke_endpoint_identify(ep, &id);
    if (err_is_fail(err)) {
        DRIVERKIT_DEBUG("[iommu client] invalid endpoint to the iommu\n");
        return err;
    }

    struct iommu_client *icl = calloc(1, sizeof(*icl));
    if (icl == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    DRIVERKIT_DEBUG("[iommu client] Connecting to SKB.\n");

    err = skb_client_connect();
    if (err_is_fail(err)) {
        return err;
    }

    DRIVERKIT_DEBUG("[iommu client] Execute query iommu_enabled(0,_).\n");

    err = skb_execute_query("iommu_enabled(0,_).");
    if (err_is_fail(err)) {
        icl->enabled = false;
        debug_printf("IOMMU Endpoint provided but IOMMU not enabled ?");
    } else {
        icl->enabled = true;
    }

    DRIVERKIT_DEBUG("[iommu client] IOMMU is %s.\n",
                    icl->enabled ? "Enabled" : "Disabled");

    icl->waitset = get_default_waitset();
    icl->endpoint = ep;
    icl->binding = NULL;
    icl->error = SYS_ERR_OK;
    icl->root_vnode_type = ObjType_Null;

    err = iommu_bind_to_endpoint(ep, iommu_bind_cb, icl,  icl->waitset ,
                                 IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    if (err_is_fail(err)) {
        free(icl);
        return err;
    }

    while(icl->binding == NULL && err_is_ok(icl->error)) {
        err = event_dispatch(icl->waitset);
        if (err_is_fail(err)){
            DEBUG_ERR(err, "failed to dispatch the event\n");
        }
    }

    *cl = icl;

    DRIVERKIT_DEBUG("[iommu client] Connected to the IOMMU service. \n");

    return err;
}


/**
 * @brief tears down a connection to the IOMMU service
 *
 * @param cl the iommu client
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_client_disconnect(struct iommu_client *cl)
{

    free(cl);
    USER_PANIC("PROPER CLEAN UP NYI!\n");
    return SYS_ERR_OK;
}


/**
 * @brief checks if there is an IOMMU present
 *
 * @param the pointer ot the IOMMU client state
 *
 * @return True if there is an IOMMU present
 *         False if there is no IOMMU present
 */
bool driverkit_iommu_present(struct iommu_client *cl)
{
    if (cl) {
        return cl->enabled;
    }
    return false;
}


/**
 * @brief sets the default iommu client to be used
 *
 * @param cl    the iommu client should be taken as default
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_set_default_client(struct iommu_client *cl)
{
    if (default_client == NULL) {
        default_client = cl;
        return SYS_ERR_OK;
    }
    return -1; /// TODO set the error number
}


/**
 * @brief returns the default iommu client
 *
 * @return pointer to the default iommu state
 */
struct iommu_client *driverkit_iommu_get_default_client(void)
{
    return default_client;
}


/*
 * ============================================================================
 * Low-level interface
 * ============================================================================
 */


/**
 * @brief sets the root table pointer of the IOMMU
 *
 * @param rootvnode the root page table (vnode)
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_set_root_vnode(struct iommu_client *cl,
                                        struct capref rootvnode)
{
    errval_t err;

    assert(cl);

    enum objtype rootvnodetype = driverkit_iommu_get_root_vnode_type(cl);
    if (rootvnodetype == ObjType_Null) {
        return IOMMU_ERR_INVALID_CAP;
    }

    struct vnode_identity id;
    err = invoke_vnode_identify(rootvnode, &id);
    if (err_is_fail(err)) {
        return err;
    }

    if (id.type != rootvnodetype) {
        return IOMMU_ERR_INVALID_CAP;
    }

    errval_t msgerr;
    err = cl->binding->rpc_tx_vtbl.setroot(cl->binding, rootvnode, &msgerr);
    if (err_is_fail(err)) {
        return err;
    }

    cl->rootvnode = rootvnode;

    return msgerr;
}


/**
 * @brief obtains the capability type for the root level vnode
 *
 * @param type returned capability type
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
enum objtype driverkit_iommu_get_root_vnode_type(struct iommu_client *cl)
{
    errval_t err;

    assert(cl);

    if (cl->root_vnode_type == ObjType_Null) {
        errval_t msgerr = SYS_ERR_OK;
        uint8_t type, bits;
        err = cl->binding->rpc_tx_vtbl.getvmconfig(cl->binding, &msgerr, &type,
                                                   &bits);
        if (err_is_fail(err) || err_is_fail(msgerr)) {
            return ObjType_Null;
        }
        cl->root_vnode_type = (enum objtype)type;
        cl->max_page_size = (1UL << bits);
    }

    return cl->root_vnode_type;
}


/**
 * @brief obtains the maximu supported page size
 *
 * @param pgsize  the maximum supported page size
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
size_t driverkit_iommu_get_max_pagesize(struct iommu_client *cl)
{
    errval_t err;

    assert(cl);

    if (cl->max_page_size == 0) {
        errval_t msgerr = SYS_ERR_OK;
        uint8_t type, bits;
        err = cl->binding->rpc_tx_vtbl.getvmconfig(cl->binding, &msgerr, &type,
                                                   &bits);
        if (err_is_fail(err) || err_is_fail(msgerr)) {
            return 0;
        }
        cl->root_vnode_type = (enum objtype)type;
        cl->max_page_size = (1UL << bits);
    }

    return cl->max_page_size;
}


static inline bool iommu_vnode_type_supported(enum objtype type)
{
    return type_is_vnode(type);
}

static inline errval_t iommu_alloc_ram_for_vnode(enum objtype type,
                                                 struct capref *retcap)
{
    return ram_alloc(retcap, vnode_objbits(type));
}

static inline errval_t iommu_free_ram(struct capref ram)
{
    cap_revoke(ram);
    cap_destroy(ram);
    return SYS_ERR_OK;
}

/**
 * @brief allocates a vnode for the iommu
 *
 * @param type      vnode type to be allocated
 * @param retvnode  returned capability to the vnode
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_alloc_vnode(struct iommu_client *cl, enum objtype type,
                                     struct capref *retvnode)
{
    errval_t err;

    assert(cl);

    if(!iommu_vnode_type_supported(type)) {
        return SYS_ERR_VNODE_TYPE;
    }

    struct capref ram;
    err = iommu_alloc_ram_for_vnode(type, &ram);
    if (err_is_fail(err)) {
        return err;
    }

    errval_t msgerr;
    err = cl->binding->rpc_tx_vtbl.retype(cl->binding, ram, type, &msgerr,
                                          retvnode);
    if (err_is_fail(err)) {
        iommu_free_ram(ram);
        return err;
    }

    if (err_is_fail(msgerr)) {
        iommu_free_ram(ram);
        return msgerr;
    }

    return SYS_ERR_OK;
}






/*
 * ===========================================================================
 *
 * ===========================================================================
 */
