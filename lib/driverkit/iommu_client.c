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
#include <target/x86_64/barrelfish_kpi/paging_target.h>
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>
#include <driverkit/hwmodel.h>
#include <skb/skb.h>

#include <if/iommu_defs.h>
#include <if/mem_defs.h>
#include <if/iommu_rpcclient_defs.h>


#include "debug.h"

struct iommu_vnode_l2
{
    enum objtype           vnode_type;
    struct capref          vnode;
    dmem_daddr_t           address_start;
    dmem_daddr_t           address_end;
    size_t                 num_frames;
    struct capref          frames[];
};


struct iommu_vnode_l3
{
    enum objtype            vnode_type;
    struct capref           vnode;
    dmem_daddr_t            address_start;
    dmem_daddr_t            address_end;
    size_t                  num_children;
    struct iommu_vnode_l2  *children[];
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

    ///< model node of the protected device
    int32_t nodeid;

    ///< pointer to the vnode information
    struct iommu_vnode_l3 **vnode_l3;
};

///< the default iommu client
static struct iommu_client *default_client;

///< the default policy to be used
static iommu_vspace_policy_t iomm_vspace_default_policy = IOMMU_VSPACE_POLICY_MIRROR;


/*
 * TODO: proper implementation of this
 */

static inline bool iommu_vnode_type_supported(struct iommu_client *st,
                                              enum objtype type)
{
    return type_is_vnode(type);
}

/*
 * allocates a piece of RAM that is suitable for holdings page tables.
 * ideally this should be in the region of "protected" or reserved memory.
 * (see IOMMU. this region can be above 4G, there are two regions)\
 *
 * The ram should be accessible by the IOMMU and/or the device driver
 */
static inline errval_t iommu_alloc_ram_for_vnode(struct iommu_client *st,
                                                 enum objtype type,
                                                 struct capref *retcap)
{

    return ram_alloc(retcap, vnode_objbits(type));
}


/*
 * allocates a piece of ram to be mapped into the driver and the devices
 * address spaces
 */
__attribute__((unused))
static errval_t iommu_alloc_ram(struct iommu_client *cl,
                                 size_t bytes,
                                 struct capref *retcap)
{
    // TODO: This function has to pass the rootvnodeslot from iommu_get_mapping
    // region to the allocator function to get mem from that region.
    bytes = ROUND_UP(bytes, LARGE_PAGE_SIZE);

    int32_t nodes[3];
    nodes[0] = driverkit_iommu_get_nodeid(cl);
    nodes[1] = driverkit_hwmodel_get_my_node_id();
    nodes[2] = 0;
    int32_t dest_nodeid = driverkit_hwmodel_lookup_dram_node_id();
    return driverkit_hwmodel_ram_alloc(retcap, bytes, dest_nodeid,
            nodes);
}

static errval_t iommu_alloc_frame(struct iommu_client *cl,
                                  size_t bytes,
                                  struct capref *retcap)
{
    bytes = ROUND_UP(bytes, LARGE_PAGE_SIZE);

    int32_t nodes[3];
    nodes[0] = driverkit_iommu_get_nodeid(cl);
    nodes[1] = driverkit_hwmodel_get_my_node_id();
    nodes[2] = 0;
    int32_t dest_nodeid = driverkit_hwmodel_lookup_dram_node_id();
    return driverkit_hwmodel_frame_alloc(retcap, bytes, dest_nodeid,
            nodes);
}

#define MAPPING_REGION_START (512UL << 31)
#define MAPPING_REGION_SIZE (512UL << 30)

#ifdef DISABLE_MODEL
static lvaddr_t vregion_map_base = MAPPING_REGION_START;
#endif
/*
 * returns a region of memory
 */

#if 0
static errval_t iommu_free_vregion(struct iommu_client *st,
                                   lvaddr_t driver,
                                   dmem_daddr_t device)
{
    errval_t err;

    // Free device region
    int32_t device_nodeid = driverkit_iommu_get_nodeid(st);
    err = skb_execute_query("enum_node_id(%d, Id), mark_range_free(Id, %lu)",
           device_nodeid, device);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "free device mem");
        return err;
    }

    // Free my region
    int32_t my_nodeid = driverkit_hwmodel_get_my_node_id();
    err = skb_execute_query("enum_node_id(%d, Id), mark_range_free(Id, %lu)",
           my_nodeid, driver);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "free process mem");
        return err;
    }
    // TODO: Actually free the own VSPACE

    return SYS_ERR_OK;
}
#endif

static errval_t iommu_free_ram(struct capref ram)
{
    struct frame_identity id;
    errval_t err = invoke_frame_identify(ram, &id);
    if (err_is_fail(err)) {
        return err;
    }
    err = skb_execute_query("dram_nodeid(X), mark_range_free(X, %lu)", id.base);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "free ram");
        return err;
    }

    err = cap_revoke(ram);
    if(err_is_fail(err)) return err;

    err = cap_destroy(ram);
    if(err_is_fail(err)) return err;

    return SYS_ERR_OK;
}


/*
 * ============================================================================
 * Management of Page Tables
 * ============================================================================
 *
 * Note: curretly we assume that there are is a three level x86_64 paging
 *       structure used, and one slot in the pml4 can be mapped in the MMU
 *       to simplify the implementation. This gives 512G of possible mappings
 *       per device.
 */

#include <barrelfish_kpi/paging_arch.h>
#define IOMMU_DEFAULT_VREGION_FLAGS VREGION_FLAGS_READ_WRITE
#define IOMMU_DEFAULT_VNODE_FLAGS (PTABLE_EXECUTE_DISABLE | PTABLE_READ_WRITE | PTABLE_USER_SUPERVISOR)


static errval_t driverkit_iommu_vnode_create_l3(struct iommu_client *cl,
                                                dmem_daddr_t addr, uint64_t *retslot,
                                                struct iommu_vnode_l3 **ret_vnode)
{
    errval_t err;

    assert(cl->vnode_l3);

    enum objtype l3_vnode_type;
    struct capref mapping;

    size_t slot;
    size_t l3_vnode_size = sizeof(struct iommu_vnode_l3);
    dmem_daddr_t end_addr;
    switch(cl->root_vnode_type) {
        case ObjType_VNode_x86_64_pml5 :
            l3_vnode_type =ObjType_VNode_x86_64_pml4;
            slot = X86_64_PML5_BASE(addr);
            *retslot = X86_64_PML4_BASE(addr);
            addr = addr & ~((512UL << 30) - 1);
            end_addr = addr + (512UL << 30) - 1;
            break;
        case ObjType_VNode_x86_64_pml4 :
            l3_vnode_type = ObjType_VNode_x86_64_pdpt;
            slot = X86_64_PML4_BASE(addr);
            *retslot = X86_64_PDPT_BASE(addr);
            addr = addr & ~X86_64_HUGE_PAGE_MASK;
            end_addr = addr + X86_64_HUGE_PAGE_SIZE - 1;
            break;
        case ObjType_VNode_x86_64_pdpt :
            l3_vnode_type = ObjType_VNode_x86_64_pdir;
            slot = X86_64_PDPT_BASE(addr);
            *retslot = X86_64_PDIR_BASE(addr);
            addr = addr & ~X86_64_LARGE_PAGE_MASK;
            end_addr = addr + X86_64_LARGE_PAGE_SIZE - 1;
            break;
        default:
            return SYS_ERR_VNODE_TYPE;
    }

    if (cl->vnode_l3[slot]) {
        *ret_vnode = cl->vnode_l3[slot];
        return SYS_ERR_OK;
    }


    size_t l3_vnode_children =(1UL << vnode_entry_bits(l3_vnode_type));

    DRIVERKIT_DEBUG("l3_vnode_children=%zu (%u - %zu)\n", l3_vnode_children,
                 vnode_objbits(l3_vnode_type),
                 vnode_entry_bits(l3_vnode_type));


    /* XXX: this should be 512 based on our assumptions */
    assert(l3_vnode_children == 512);

    l3_vnode_size += (l3_vnode_children * sizeof(void *));

    struct iommu_vnode_l3 *vnode_l3 = calloc(1, l3_vnode_size);
    if (vnode_l3 == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }


    vnode_l3->vnode_type = l3_vnode_type;
    vnode_l3->num_children = l3_vnode_children;
    vnode_l3->address_start = addr;
    vnode_l3->address_end = end_addr;


    err = driverkit_iommu_alloc_vnode_cl(cl, l3_vnode_type, &vnode_l3->vnode);
    if (err_is_fail(err)) {
        goto err_out2;
    }

    DRIVERKIT_DEBUG("MAPPING ROOT VNODE!\n");
    err = driverkit_iommu_map(cl, cl->rootvnode, vnode_l3->vnode,
                              slot, IOMMU_DEFAULT_VREGION_FLAGS,
                              0, 1);
    if (err_is_fail(err)) {
        goto err_out3;
    }

    if (cl->policy == IOMMU_VSPACE_POLICY_SHARED) {

        DRIVERKIT_DEBUG("Mapping cap in the vroot of the driver\n");

        err = slot_alloc(&mapping);
        if (err_is_fail(err)) {
            goto err_out4;
        }

        err = vnode_map(cap_vroot, vnode_l3->vnode, slot,
                        IOMMU_DEFAULT_VNODE_FLAGS, 0, 1, mapping);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to map the l3 vnode in drivers space");
            goto err_out5;
        }

        DRIVERKIT_DEBUG("Mapped at vroot [%u]\n", cl->rootvnode_slot);
    }

    cl->vnode_l3[slot] = vnode_l3;
    *ret_vnode = vnode_l3;

    return SYS_ERR_OK;

    err_out5:
    slot_free(mapping);
    err_out4:
    driverkit_iommu_unmap(cl, cl->rootvnode, slot);
    err_out3:
    /* TODO: free vnode */
    err_out2:
    free(cl->vnode_l3);
    cl->vnode_l3 = NULL;
    return err;
}



static errval_t driverkit_iommu_vnode_get_l2(struct iommu_client *cl,
                                             dmem_daddr_t addr, uint64_t *retslot,
                                             struct iommu_vnode_l2 **ret_vnode)
{
    errval_t err;

    struct iommu_vnode_l3 *vnode_l3;
    uint64_t slot;
    err = driverkit_iommu_vnode_create_l3(cl, addr, &slot, &vnode_l3);
    if (err_is_fail(err)) {
        return err;
    }

    enum objtype l2_vnode_type;

    size_t l2_vnode_size = sizeof(struct iommu_vnode_l2);
    dmem_daddr_t end_addr;

    switch(vnode_l3->vnode_type) {
        case ObjType_VNode_x86_64_pml4 :
            DRIVERKIT_DEBUG("Allocate PDPT for L2 node\n");
            l2_vnode_type = ObjType_VNode_x86_64_pdpt;
            addr = addr & ~X86_64_HUGE_PAGE_MASK;
            end_addr = addr + X86_64_HUGE_PAGE_SIZE - 1;
            assert(slot == X86_64_PML4_BASE(addr));
            *retslot = X86_64_PDPT_BASE(addr);
            break;
        case ObjType_VNode_x86_64_pdpt :
            DRIVERKIT_DEBUG("Allocate PDIR for L2 node\n");
            l2_vnode_type = ObjType_VNode_x86_64_pdir;
            addr = addr & ~X86_64_LARGE_PAGE_MASK;
            end_addr = addr + X86_64_LARGE_PAGE_SIZE - 1;
            assert(slot == X86_64_PDPT_BASE(addr));
            *retslot = X86_64_PDIR_BASE(addr);
            break;
        case ObjType_VNode_x86_64_pdir :
            DRIVERKIT_DEBUG("Allocate PTABLE for L2 node\n");
            l2_vnode_type = ObjType_VNode_x86_64_ptable;
            addr = addr & ~X86_64_BASE_PAGE_MASK;
            end_addr = addr + X86_64_BASE_PAGE_SIZE - 1;
            assert(slot == X86_64_PDIR_BASE(addr));
            *retslot = X86_64_PTABLE_BASE(addr);
            break;
        default:
            return SYS_ERR_VNODE_TYPE;
    }

    if (vnode_l3->children[slot]) {
        *ret_vnode = vnode_l3->children[slot];
        return SYS_ERR_OK;
    }

    size_t l2_vnode_children =(1UL << vnode_entry_bits(l2_vnode_type));
    assert(l2_vnode_children == 512);
    l2_vnode_size += (l2_vnode_children * sizeof(void *));

    struct iommu_vnode_l2 *vnode_l2 = calloc(1, l2_vnode_size);
    if (vnode_l2 == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    vnode_l2->vnode_type = l2_vnode_type;
    vnode_l2->num_frames = l2_vnode_children;
    vnode_l2->address_start = addr;
    vnode_l2->address_end = end_addr;


    err = driverkit_iommu_alloc_vnode_cl(cl, l2_vnode_type, &vnode_l2->vnode);
    if (err_is_fail(err)) {
        goto err_out;
    }

    err = driverkit_iommu_map(cl, vnode_l3->vnode, vnode_l2->vnode, slot,
                              IOMMU_DEFAULT_VREGION_FLAGS, 0, 1);
    if (err_is_fail(err)) {
        goto err_out2;
    }

    vnode_l3->children[slot] = vnode_l2;

    *ret_vnode = vnode_l3->children[slot];

    return SYS_ERR_OK;

    err_out2:
    /* todo: free the mapping region */
    err_out:
    free(vnode_l2);
    return err;
}


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
    int32_t nodeid;
    err = icl->binding->rpc_tx_vtbl.getvmconfig(icl->binding, &msgerr, &type,
                                                &bits, &nodeid);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to send the message\n");
        goto err_out;
    }

    if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "reply failed\n");
        err = msgerr;
        goto err_out;
    }

    icl->root_vnode_type = (enum objtype)type;
    icl->max_page_size = (1UL << bits);
    icl->nodeid = nodeid;

    icl->vnode_l3 = calloc((1UL << vnode_entry_bits(icl->root_vnode_type)),
                           sizeof(void *));
    if (icl->vnode_l3 == NULL) {
        err = LIB_ERR_MALLOC_FAIL;
        goto err_out;
    }

    /* allocate memory for the vnode */

    err = driverkit_iommu_alloc_vnode_cl(icl, icl->root_vnode_type,
                                         &icl->rootvnode);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vnode alloc failed\n");
        goto err_out;
    }

    struct vnode_identity id;
    invoke_vnode_identify(icl->rootvnode, &id);
    assert(id.type == icl->root_vnode_type);
    switch(icl->root_vnode_type) {
        case ObjType_VNode_x86_64_pml4 :
            DRIVERKIT_DEBUG("%s. PML4 @ 0x%lx for rootvnode\n", __FUNCTION__, id.base);
            break;
        case ObjType_VNode_x86_64_pdpt :
            DRIVERKIT_DEBUG("%s. PDPT @ 0x%lx for rootvnode\n", __FUNCTION__, id.base);
            break;
        case ObjType_VNode_x86_64_pdir :
            DRIVERKIT_DEBUG("%s. PDIR @ 0x%lx for rootvnode\n", __FUNCTION__, id.base);
            break;
        case ObjType_VNode_VTd_ctxt_table :
            DRIVERKIT_DEBUG("%s. CTXT @ 0x%lx for rootvnode\n", __FUNCTION__, id.base);
            break;
        default:
            break;
    }

    err = driverkit_iommu_set_root_vnode(icl, icl->rootvnode);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "set root vnode failed\n");

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

    if (capref_is_null(ep)) {
        DRIVERKIT_DEBUG("[iommu client] invalid endpoint to the iommu\n");
        return IOMMU_ERR_INVALID_EP;
    }

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
    icl->nodeid = -1;

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
    icl->policy = iomm_vspace_default_policy;

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

    if (err_is_fail(msgerr)) {
        return msgerr;
    }

    cl->rootvnode = rootvnode;

    return SYS_ERR_OK;
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
        int32_t nodeid;
        err = cl->binding->rpc_tx_vtbl.getvmconfig(cl->binding, &msgerr, &type,
                                                   &bits, &nodeid);
        if (err_is_fail(err) || err_is_fail(msgerr)) {
            return ObjType_Null;
        }
        cl->root_vnode_type = (enum objtype)type;
        cl->max_page_size = (1UL << bits);
    }

    return cl->root_vnode_type;
}


/**
 * @brief obtains the maximum supported page size
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
        int32_t nodeid;
        err = cl->binding->rpc_tx_vtbl.getvmconfig(cl->binding, &msgerr, &type,
                                                   &bits, &nodeid);
        if (err_is_fail(err) || err_is_fail(msgerr)) {
            return 0;
        }
        cl->root_vnode_type = (enum objtype)type;
        cl->max_page_size = (1UL << bits);
    }

    return cl->max_page_size;
}

/**
 * @brief obtains the model node id for the protected device
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
int32_t driverkit_iommu_get_nodeid(struct iommu_client *cl)
{
    errval_t err;

    assert(cl);

    if (cl->nodeid == -1) {
        errval_t msgerr = SYS_ERR_OK;
        uint8_t type, bits;
        int32_t nodeid;
        err = cl->binding->rpc_tx_vtbl.getvmconfig(cl->binding, &msgerr, &type,
                                                   &bits, &nodeid);
        if (err_is_fail(err) || err_is_fail(msgerr)) {
            return 0;
        }
        cl->nodeid = nodeid;
    }

    return cl->nodeid;
}


/**
 * @brief maps a vnode or a frame cap into a vnode cap
 *
 * @param cl    the iommu client
 * @param dst   destination vnode to map into
 * @param src   the source capability to be mapped
 * @param slot  the slot to map into
 * @param attr  attributes for the mapping
 * @param off   offset into the frame
 * @param count number of page-table entries to be mapped
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_map(struct iommu_client *cl, struct capref dst,
                             struct capref src, uint16_t slot, uint64_t attr,
                             uint64_t off, uint64_t count)
{
    errval_t err;

    assert(cl);

    DRIVERKIT_DEBUG("mapping slot=%u, attr=%lx, offset=%lx, count=%lu]\n",
                            slot, attr, off, count);

    errval_t msgerr;
    err = cl->binding->rpc_tx_vtbl.map(cl->binding, dst, src, slot, attr,
                                       off, count, &msgerr);
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
                                       struct dmem *dmem) {
    errval_t err;

    char conf_buf[512];

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    assert(id.bytes >= LARGE_PAGE_SIZE);

    if (cl == NULL) {
        // TODO is this a good idea? 
        dmem->devaddr = id.base;
        dmem->size = id.bytes;
        return vspace_map_one_frame_attr((void **)&dmem->vbase, id.bytes, frame,
                flags, NULL, NULL);
    }

    DRIVERKIT_DEBUG("[iommu client] allocate driver vspace\n");

    int32_t my_nodeid = driverkit_hwmodel_get_my_node_id();
    err = driverkit_hwmodel_vspace_map(my_nodeid, frame, flags, dmem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed");
        return err;
    }
    // Alloc space in my vspace


    DRIVERKIT_DEBUG("[iommu client] allocate device vspace\n");

    // Map into dev vspace
    int32_t device_nodeid = driverkit_iommu_get_nodeid(cl);
    err = driverkit_hwmodel_get_map_conf(frame, device_nodeid, conf_buf,
                                         sizeof(conf_buf), &dmem->devaddr);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map local");
        return err;
    }

    uint64_t inaddr, outaddr;
    int32_t nodeid;
    struct list_parser_status status;
    skb_read_list_init_offset(&status, conf_buf, 0);
    while(skb_read_list(&status, "c(%"SCNi32", %"SCNu64", %"SCNu64")",
                        &nodeid, &inaddr, &outaddr)) {
        debug_printf("%s:%u %i, %i, inaddr=%lx, vbase=%lx\n", __FUNCTION__, __LINE__,
                     nodeid, nodeid, inaddr, dmem->devaddr);
        err = driverkit_iommu_vspace_map_fixed_cl(cl, frame, flags, dmem);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed/ todo: cleanup");
        //    iommu_free_vregion(cl, dmem->vbase, dmem->devaddr);
            return err;
        }
    }

    return err;
}



/**
 * @brief maps a frame in the device and driver space
 *
 * @param frame the frame to be mapped
 * @param flags attributes for the mapping
 * @param dmem  the device memory struct
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_vspace_map_fixed_cl(struct iommu_client *cl,
                                             struct capref frame,
                                             vregion_flags_t flags,
                                             struct dmem *dmem)
{
    errval_t err;

    dmem->cl = cl;
    dmem->mem = frame;

    debug_printf("%s:%u Allocated VREGIONs 0x%" PRIxLVADDR " 0x%" PRIxLVADDR "\n",
                    __FUNCTION__, __LINE__, dmem->vbase, dmem->devaddr);


    /*
     * if driver vbase is null, then we map it at any address in the driver's
     * vspace. Only if the policy is not shared, then we have to map it.
     */
    if (cl == NULL || cl->policy != IOMMU_VSPACE_POLICY_SHARED || !cl->enabled) {
        if (dmem->vbase == 0) {
            DRIVERKIT_DEBUG("vspace_map_one_frame_attr(?, %lu)\n", dmem->size >> 20);
            err = vspace_map_one_frame_attr((void **)&dmem->vbase, dmem->size,
                                            dmem->mem, flags, NULL, NULL);
        } else {
            DRIVERKIT_DEBUG("vspace_map_one_frame_fixed_attr(%lx, %lu)\n",
                            dmem->vbase, dmem->size >> 20);
            err = vspace_map_one_frame_fixed_attr(dmem->vbase, dmem->size,
                                                  dmem->mem, flags, NULL, NULL);
            if (err_is_fail(err)) {

                if (err_no(err) == LIB_ERR_VREGION_MAP) {
                    err = SYS_ERR_OK;
                } else {
                    DEBUG_ERR(err, "failed to map the frame");
                }
            }
        }
        DRIVERKIT_DEBUG("%s:%u mapping in driver at 0x%" PRIxLVADDR "\n",
                     __FUNCTION__, __LINE__, dmem->vbase);

        if (err_is_fail(err)) {
            goto err_out;
        }
    }

    if (cl == NULL || !cl->enabled) {
        return SYS_ERR_OK;
    }

    assert(dmem->vbase);
    assert(dmem->devaddr || dmem->devaddr == 0); //TODO: Maybe dont give 0 as valid dev address



    uint64_t ptecount = 1;
    uint64_t pagesize = 0;
    switch(cl->root_vnode_type) {
        case ObjType_VNode_x86_64_pml5 :
            assert(dmem->size >= X86_64_HUGE_PAGE_SIZE &&
                   !(dmem->size & X86_64_HUGE_PAGE_MASK));
            ptecount = dmem->size / X86_64_HUGE_PAGE_SIZE;
            pagesize = X86_64_HUGE_PAGE_SIZE;
            break;
        case ObjType_VNode_x86_64_pml4 :
            assert(dmem->size >= X86_64_LARGE_PAGE_SIZE &&
                   !(dmem->size & X86_64_LARGE_PAGE_MASK));
            ptecount = dmem->size / X86_64_LARGE_PAGE_SIZE;
            pagesize = X86_64_LARGE_PAGE_SIZE;
            break;
        case ObjType_VNode_x86_64_pdpt :
            assert(dmem->size >= X86_64_BASE_PAGE_SIZE &&
                   !(dmem->size & X86_64_BASE_PAGE_MASK));
            ptecount = dmem->size / X86_64_BASE_PAGE_SIZE;
            pagesize = X86_64_BASE_PAGE_SIZE;
            break;
        default:
            err = SYS_ERR_VNODE_TYPE;
            goto err_out2;
    }

    DRIVERKIT_DEBUG("%s:%u pagesize=%zu, ptecount=%zu\n",
                    __FUNCTION__, __LINE__, pagesize, ptecount);

    uint64_t offset = 0;
    while(ptecount > 0) {
        assert((offset & (pagesize - 1)) == 0);
        assert(offset < dmem->size);

        /* allocate the vnodes */
        struct iommu_vnode_l2 *vnode;
        uint64_t slot;
        err = driverkit_iommu_vnode_get_l2(cl, dmem->devaddr + offset,
                                           &slot, &vnode);
        if (err_is_fail(err)) {
            goto err_out2;
        }

        uint64_t max_pte = (1UL << vnode_entry_bits(vnode->vnode_type));
        assert(slot < max_pte);
        assert(ptecount > 0);

        /* fits all in one */
        if ((ptecount + slot) < max_pte) {
            /* map the vnodes */
            DRIVERKIT_DEBUG("%s:%u mapping in device address space 0x%" PRIxGENVADDR "\n",
                         __FUNCTION__, __LINE__, dmem->devaddr+offset);
            err = driverkit_iommu_map(cl, vnode->vnode, dmem->mem, slot, flags,
                                      offset, ptecount);
            if (err_is_fail(err)) {
                goto err_out3;
            }

            return SYS_ERR_OK;
        } else {

            DRIVERKIT_DEBUG("%s:%u mapping in device address space 0x%" PRIxGENVADDR "\n",
                         __FUNCTION__, __LINE__, dmem->devaddr+offset);

            err = driverkit_iommu_map(cl, vnode->vnode, dmem->mem, slot, flags,
                                      offset, max_pte - slot);
            if (err_is_fail(err)) {
                goto err_out3;
            }


            offset += (pagesize * (max_pte - slot));
            ptecount -= (max_pte - slot);

            DRIVERKIT_DEBUG("mapped slots [%lu..%lu], offset now %lx (%lx), ptecount=%lu\n",
                         slot, max_pte, offset, pagesize, ptecount);
        }
    }

    return SYS_ERR_OK;

    err_out3:
    USER_PANIC("NYI: cleanup mapped frames!\n");
    err_out2:
    if (cl == NULL || cl->policy != IOMMU_VSPACE_POLICY_SHARED) {
        vspace_unmap((void *)dmem->vbase);
    }

    err_out:
    return err;
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
    if (bytes < (LARGE_PAGE_SIZE)) {
        bytes = LARGE_PAGE_SIZE;
    }

    if (cl == NULL) {
        return frame_alloc(retframe, bytes, NULL);
    } else {
        return iommu_alloc_frame(cl, bytes, retframe);
    }
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

    err = slot_alloc(retvnode);
    if (err_is_fail(err)) {
        goto err_out;
    }

    errval_t msgerr;
    err = cl->binding->rpc_tx_vtbl.retype(cl->binding, ram, type, &msgerr,
                                          retvnode);
    if (err_is_fail(err)) {
        goto err_out;
    }

    if (err_is_fail(msgerr)) {
        err = msgerr;
        goto err_out;
    }

    return SYS_ERR_OK;

    err_out:
    iommu_free_ram(ram);
    return err;

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
errval_t driverkit_iommu_mmap_cl(struct iommu_client *cl, size_t bytes,
                                 vregion_flags_t flags, struct dmem *mem)
{
    errval_t err;

    struct capref frame;
#ifdef DISABLE_MODEL
    err = driverkit_iommu_alloc_frame(NULL, bytes, &frame);
    if (err_is_fail(err)) {
        return err;
    }
#else
    err = driverkit_iommu_alloc_frame(cl, bytes, &frame);
    if (err_is_fail(err)) {
        return err;
    }
#endif

    err = driverkit_iommu_vspace_map_cl(cl, frame, flags, mem);
    if (err_is_fail(err)) {
        iommu_free_ram(mem->mem);
        return err;
    }

    return SYS_ERR_OK;
}

errval_t driverkit_iommu_mmap(size_t bytes, vregion_flags_t flags, struct dmem *mem)
{
    return driverkit_iommu_mmap_cl(driverkit_iommu_get_default_client(),
                                   bytes, flags, mem);
}

errval_t driverkit_iommu_munmap(struct dmem *mem)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}


/**
 * @brief sets the iommu vspace managemet policy
 *
 * @param cl     the iommu client to set the policy for
 * @param policy the new policy
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_vspace_set_policy(struct iommu_client *cl,
                                           iommu_vspace_policy_t policy)
{
    if (!capref_is_null(cl->rootvnode)) {
        return IOMMU_ERR_NOT_SUPPORTED;
    }
    cl->policy = policy;

    return SYS_ERR_OK;
}


/**
 * @brief sets the default iommu vspace managemet policy
 *
 * @param policy the new policy
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_vspace_set_default_policy(iommu_vspace_policy_t policy)
{
    iomm_vspace_default_policy = policy;
    return SYS_ERR_OK;
}

