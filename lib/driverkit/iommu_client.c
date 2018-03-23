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

struct iommu_vnode_l2
{
    struct capref          vnode;
    lvaddr_t               address;
    size_t                 num_frames;
    struct capref          frames[];
};


struct iommu_vnode_l3
{
    enum objtype           vnode_type;
    struct capref          vnode;
    dmem_daddr_t           address_start;
    dmem_daddr_t           address_end;
    size_t                 num_children;
    struct iommu_vnode_l2 *children[];
};


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

    ///< the maximum supported page size
    size_t max_page_size;

    ///< error value for async errors
    errval_t error;

    ///< the policy to be employed by the mapping function
    iommu_vspace_policy_t policy;

    ///< ObjType of the root vnode
    enum objtype root_vnode_type;

    ///< the capability to the root vnode
    struct capref rootvnode;

    ///< slot used in the root vnode
    uint16_t rootvnode_slot;

    ///< pointer to the vnode information
    struct iommu_vnode_l3 *vnode_l3;
};

///< the default iommu client
static struct iommu_client *default_client;


/*
 * TODO: proper implementation of this
 */
static inline bool iommu_vnode_type_supported(struct iommu_client *st,
                                              enum objtype type)
{
    return type_is_vnode(type);
}

static inline errval_t iommu_alloc_ram_for_vnode(struct iommu_client *st,
                                                 enum objtype type,
                                                 struct capref *retcap)
{
    return ram_alloc(retcap, vnode_objbits(type));
}

static inline errval_t iommu_alloc_ram_for_frame(struct iommu_client *st,
                                                 size_t bytes,
                                                 struct capref *retcap)
{
    if (bytes < (LARGE_PAGE_SIZE)) {
        bytes = LARGE_PAGE_SIZE;
    }
    return frame_alloc(retcap, bytes, NULL);
}

static inline errval_t iommu_alloc_vregion(struct iommu_client *st,
                                           size_t bytes,
                                           lvaddr_t *driver,
                                           dmem_daddr_t *device)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}

static inline errval_t iommu_free_vregion(struct iommu_client *st,
                                          lvaddr_t driver,
                                          dmem_daddr_t device)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}

static inline errval_t iommu_free_ram(struct capref ram)
{
    cap_revoke(ram);
    cap_destroy(ram);
    return SYS_ERR_OK;
}

static inline errval_t iommu_get_mapping_region(struct iommu_client *cl,
                                                dmem_daddr_t *start,
                                                dmem_daddr_t *end,
                                                uint16_t *rootvnodeslot)
{
    *start = (512UL << 30);
    *end = *start + (512UL << 30) - 1;
    *rootvnodeslot = 1;
    return SYS_ERR_OK;
}


/*
 * ============================================================================
 * Management of Page Tables
 * ============================================================================
 */

#if 0

static errval_t driverkit_iommu_vnode_get_l2(struct iommu_client *cl,
                                             dmem_daddr_t addr,
                                             struct iommu_vnode_l2 **ret_vnode)
{
    errval_t err;

    uint16_t idx;

    if (cl->vnode_l3->children[idx]) {
        *ret_vnode = cl->vnode_l3->children[idx];
        return SYS_ERR_OK;
    }
    enum objtype l3_vnode_type;

    size_t l3_vnode_size = sizeof(struct iommu_vnode_l3);
    switch(cl->root_vnode_type) {
        case ObjType_VNode_x86_64_pml5 :
            l3_vnode_type =ObjType_VNode_x86_64_pml4;
            break;
        case ObjType_VNode_x86_64_pml4 :
            l3_vnode_type = ObjType_VNode_x86_64_pdpt;
            break;
        case ObjType_VNode_x86_64_pdpt :
            l3_vnode_type = ObjType_VNode_x86_64_pdir;
            break;
        default:
            return SYS_ERR_VNODE_TYPE;
    }

    size_t l3_vnode_children =(1UL << (vnode_objbits(l3_vnode_type) -
                                       vnode_entry_bits(l3_vnode_type)));
    l3_vnode_size += (l3_vnode_children * sizeof(void *));

    cl->vnode_l3 = calloc(1, l3_vnode_size);
    if (cl->vnode_l3 == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    cl->vnode_l3->vnode_type = l3_vnode_type;
    cl->vnode_l3->num_children = l3_vnode_children;

    err = iommu_get_mapping_region(cl, &cl->vnode_l3->address_start,
                                   &cl->vnode_l3->address_end,
                                   &cl->rootvnode_slot);
    if (err_is_fail(err)) {
        goto err_out;
    }

    err = driverkit_iommu_alloc_vnode_cl(cl, l3_vnode_type, &cl->vnode_l3->vnode);
    if (err_is_fail(err)) {
        goto err_out2;
    }

    err = driverkit_iommu_map(cl, cl->rootvnode, cl->rootvnode_slot,
                              cl->vnode_l3->vnode);
    if (err_is_fail(err)) {
        goto err_out3;
    }

    return SYS_ERR_OK;

    err_out3:
    /* TODO: free vnode */
    err_out2:
    /* todo: free the mapping region */
    err_out:
    free(cl->vnode_l3);
    cl->vnode_l3 = NULL;
    return err;
}

static errval_t driverkit_iommu_vnode_create_l3(struct iommu_client *cl)
{
    errval_t err;

    if (cl->vnode_l3) {
        return SYS_ERR_OK;
    }
    enum objtype l3_vnode_type;

    size_t l3_vnode_size = sizeof(struct iommu_vnode_l3);
    switch(cl->root_vnode_type) {
        case ObjType_VNode_x86_64_pml5 :
            l3_vnode_type =ObjType_VNode_x86_64_pml4;
            break;
        case ObjType_VNode_x86_64_pml4 :
            l3_vnode_type = ObjType_VNode_x86_64_pdpt;
            break;
        case ObjType_VNode_x86_64_pdpt :
            l3_vnode_type = ObjType_VNode_x86_64_pdir;
            break;
        default:
            return SYS_ERR_VNODE_TYPE;
    }

    size_t l3_vnode_children =(1UL << (vnode_objbits(l3_vnode_type) -
                                         vnode_entry_bits(l3_vnode_type)));
    l3_vnode_size += (l3_vnode_children * sizeof(void *));

    cl->vnode_l3 = calloc(1, l3_vnode_size);
    if (cl->vnode_l3 == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    cl->vnode_l3->vnode_type = l3_vnode_type;
    cl->vnode_l3->num_children = l3_vnode_children;

    err = iommu_get_mapping_region(cl, &cl->vnode_l3->address_start,
                                   &cl->vnode_l3->address_end,
                                   &cl->rootvnode_slot);
    if (err_is_fail(err)) {
        goto err_out;
    }

    err = driverkit_iommu_alloc_vnode_cl(cl, l3_vnode_type, &cl->vnode_l3->vnode);
    if (err_is_fail(err)) {
        goto err_out2;
    }

    err = driverkit_iommu_map(cl, cl->rootvnode, cl->rootvnode_slot,
                              cl->vnode_l3->vnode);
    if (err_is_fail(err)) {
        goto err_out3;
    }

    return SYS_ERR_OK;

    err_out3:
    /* TODO: free vnode */
    err_out2:
    /* todo: free the mapping region */
    err_out:
    free(cl->vnode_l3);
    cl->vnode_l3 = NULL;
    return err;
}

#endif

/*
 * ============================================================================
 * Initialization
 * ============================================================================
 */


static void iommu_bind_cb(void *argst,  errval_t err, struct iommu_binding *ib)
{
    DRIVERKIT_DEBUG("[iommu client] bound to service: %s\n",
                    err_getstring(err));

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
errval_t driverkit_iommu_client_init_cl(struct capref ep, struct iommu_client **cl)
{
    errval_t err;

    assert(cl);

    struct iommu_client *icl;
    err = driverkit_iommu_client_connect_cl(ep, &icl);
    if (err_is_fail(err)) {
        return err;
    }

    errval_t msgerr;
    uint8_t type, bits;
    err = icl->binding->rpc_tx_vtbl.getvmconfig(icl->binding, &msgerr, &type,
                                                &bits);
    if (err_is_fail(err)) {
        driverkit_iommu_client_disconnect_cl(icl);
        goto err_out;
    }

    if (err_is_fail(msgerr)) {
        err = msgerr;
        goto err_out;
    }

    icl->root_vnode_type = (enum objtype)type;
    icl->max_page_size = (1UL << bits);

    /* allocate memory for the vnode */

    err = driverkit_iommu_alloc_vnode_cl(icl, icl->root_vnode_type,
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
    driverkit_iommu_client_disconnect_cl(icl);
    return err;
}


/**
 * @brief initializes the IOMMU client library with the IOMMU endpoint
 *
 * @param ep the IOMMU endpoint
 *
 * @return SYS_ERR_OK on success, errval on failure
 *
 * This function initializes the connection, allocates the root vnode etc.
 * and sets the default client pointer
 */
errval_t driverkit_iommu_client_init(struct capref ep)
{
    errval_t err;

    assert(default_client == NULL);

    struct iommu_client *cl;
    err = driverkit_iommu_client_init_cl(ep, &cl);
    if (err_is_fail(err)) {
        return err;
    }

    return driverkit_iommu_set_default_client(cl);
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
errval_t driverkit_iommu_client_connect_cl(struct capref ep,
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
 * @brief connects to the IOMMU service using the default client
 *
 * @param ep the IOMMU endpoint
 *
 * @return SYS_ERR_OK on success, errval on failure
 *
 * This just initializes the connecton to the IOMMU, and sets the default
 * client pointer
 */
errval_t driverkit_iommu_client_connect(struct capref ep)
{
    errval_t err;

    assert(default_client == NULL);

    struct iommu_client *cl;
    err = driverkit_iommu_client_connect_cl(ep, &cl);
    if (err_is_fail(err)) {
        return err;
    }

    return driverkit_iommu_set_default_client(cl);
}


/**
 * @brief tears down a connection to the IOMMU service
 *
 * @param cl the iommu client
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_client_disconnect_cl(struct iommu_client *cl)
{

    free(cl);
    USER_PANIC("PROPER CLEAN UP NYI!\n");
    return SYS_ERR_OK;
}


/**
 * @brief tears down a connection to the IOMMU service
 *
 * @param cl the iommu client
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_client_disconnect(void)
{
    struct iommu_client *cl = driverkit_iommu_get_default_client();
    if (cl) {
        default_client = NULL;
        return driverkit_iommu_client_disconnect_cl(cl);
    }
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


/**
 * @brief maps a vnode or a frame cap into a vnode cap
 *
 * @param cl        the iommu client
 * @param dst   destination vnode to map into
 * @param slot  the slot to map into
 * @param src   the source capability to be mapped
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_map(struct iommu_client *cl, struct capref dst,
                             uint16_t slot, struct capref src)
{
    errval_t err;

    assert(cl);

    errval_t msgerr;
    err = cl->binding->rpc_tx_vtbl.map(cl->binding, dst, slot, src, &msgerr);
    if (err_is_ok(err)) {
        err = msgerr;
    }

    return err;
}


/**
 * @brief unmaps a slot in a vnode
 *
 * @param cl        the iommu client
 * @param dst   the vnode containing the mapping
 * @param slot  the slot to be unmapped
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_unmap(struct iommu_client *cl, struct capref dst,
                               uint16_t slot)
{
    errval_t err;

    assert(cl);

    errval_t msgerr;
    err = cl->binding->rpc_tx_vtbl.unmap(cl->binding, dst, slot, &msgerr);
    if (err_is_ok(err)) {
        err = msgerr;
    }

    return err;
}


/**
 * @brief changes the flags of the mapping
 *
 * @param cl    the iommu client
 * @param dest  the destination vnode to change the mapping
 * @param slot  the slot to change the mapping
 * @param attrs the new attributes to set
 *
 * @return SYS_ERR_OK on success, erval on failure
 */
errval_t driverkit_iommu_modify(struct iommu_client *cl, struct capref dest,
                                uint16_t slot, uint64_t attrs)
{
    errval_t err;

    assert(cl);

    errval_t msgerr;
    err = cl->binding->rpc_tx_vtbl.modify(cl->binding, dest, slot, attrs,
                                          &msgerr);
    if (err_is_ok(err)) {
        err = msgerr;
    }

    return err;
}



/*
 * ============================================================================
 * High-level VSpace Management Interface
 * ============================================================================
 */



/**
 * @brief maps a frame in the device and driver space
 *
 * @param frame the frame to be mapped
 * @param flags attributes for the mapping
 * @param dmem  the device memory struct
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_vspace_map_cl(struct iommu_client *cl,
                                       struct capref frame,
                                       vregion_flags_t flags,
                                       struct dmem *dmem)
{
    errval_t err;

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    memset(dmem, 0, sizeof(*dmem));

    dmem->mem = frame;
    dmem->cl = cl;
    dmem->size = id.bytes;

    err = iommu_alloc_vregion(cl, id.bytes, &dmem->vbase, &dmem->devaddr);
    if (err_is_fail(err)) {
        return err;
    }

    /*
     * if driver vbase is null, then we map it at any address in the driver's
     * vspace. Only if the policy is not shared, then we have to map it.
     */
    if (cl->policy != IOMMU_VSPACE_POLICY_SHARED) {
        if (dmem->vbase == 0) {
            err = vspace_map_one_frame_attr((void **)&dmem->vbase, dmem->size,
                                            dmem->mem, flags, NULL, NULL);
        } else {
            err = vspace_map_one_frame_fixed_attr(dmem->vbase, dmem->size,
                                                  dmem->mem, flags, NULL, NULL);
        }

        if (err_is_fail(err)) {
            iommu_free_vregion(cl, dmem->vbase, dmem->devaddr);
            return err;
        }
    }




    return LIB_ERR_NOT_IMPLEMENTED;
}


/**
 * @brief maps a frame in the device and driver space using the default connectin
 *
 * @param frame the frame to be mapped
 * @param flags attributes for the mapping
 * @param dmem  the device memory struct
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_vspace_map(struct capref frame, vregion_flags_t flags,
                                    struct dmem *dmem)
{
    struct iommu_client *cl = driverkit_iommu_get_default_client();
    if (cl == NULL) {
        return IOMMU_ERR_IOMMU_NOT_FOUND;
    }
    return driverkit_iommu_vspace_map_cl(cl, frame, flags, dmem);
}


/**
 * @brief unmaps a previoiusly mapped device memory region
 *
 * @param dmem  the device memory region
 *
 * @return SYS_ERR_OK on succes, errval on failure
 */
errval_t driverkit_iommu_vspace_unmap(struct dmem *dmem)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}


/**
 * @brief modifies an existing mapping
 *
 * @param dmem  the device mem region
 * @param flags new attributes for the mapping
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_vspace_modify_flags(struct dmem *dmem,
                                             vregion_flags_t flags)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}






/*
 * ============================================================================
 * Memory Allocation
 * ============================================================================
 */


/**
 * @brief allocates a frame to be mapped accessible by the device and the driver
 *
 * @param cl        the iommu client
 * @param bytes     number of bytes to allocate
 * @param retframe  returned frame capability
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_alloc_frame(struct iommu_client *cl, size_t bytes,
                                     struct capref *retframe)
{
    return iommu_alloc_ram_for_frame(cl, bytes, retframe);
}


/**
 * @brief allocates a vnode for the iommu
 *
 * @param type      vnode type to be allocated
 * @param retvnode  returned capability to the vnode
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_alloc_vnode_cl(struct iommu_client *cl, enum objtype type,
                                        struct capref *retvnode)
{
    errval_t err;

    assert(cl);

    if(!iommu_vnode_type_supported(cl, type)) {
        return SYS_ERR_VNODE_TYPE;
    }

    struct capref ram;
    err = iommu_alloc_ram_for_vnode(cl, type, &ram);
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


/**
 * @brief allocates a vnode for the iommu
 *
 * @param type      vnode type to be allocated
 * @param retvnode  returned capability to the vnode
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_alloc_vnode(enum objtype type, struct capref *retvnode)
{
    struct iommu_client *cl = driverkit_iommu_get_default_client();
    if (cl == NULL) {
        return IOMMU_ERR_IOMMU_NOT_FOUND;
    }
    return driverkit_iommu_alloc_vnode_cl(cl, type, retvnode);
}



/**
 * @brief allocates and maps a region of memory
 *
 * @param cl    the iommu client
 * @param bytes bytes to be allocated
 * @param mem   returned dmem
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_mmap(struct iommu_client *cl, size_t bytes,
                              struct dmem *mem)
{
    driverkit_iommu_alloc_frame(cl, bytes, &mem->mem);

    /* find free virtual address */

    /* allocate the vnodes */

    /* map the vnodes */

    /* map the frame */

    return LIB_ERR_NOT_IMPLEMENTED;
}




/*
 * ===========================================================================
 *
 * ===========================================================================
 */
