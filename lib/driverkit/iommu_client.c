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

    ///< model node of the protected device
    int32_t nodeid;
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
 *  Returns this process nodeid. It lazily adds the process' model node
 *  and returns it's identifier.
 */
static int32_t get_own_nodeid(void) {
    static int32_t nodeid = -1;
    errval_t err;
    if(nodeid == -1){
        err = skb_execute_query("add_process_alloc(X), write(X)");
        assert(err_is_ok(err));
        err = skb_read_output("%d", &nodeid);
        assert(err_is_ok(err));
        DRIVERKIT_DEBUG("Instantiated new process model node, nodeid=%"PRIi32"\n",
                nodeid);
    }
    return nodeid;
}

#define MODE_ALLOC_COMMON 0 
#define MODE_MAP_COMMON 1 

static errval_t alloc_common(int mode, int bits,
    int32_t nodeid1, uint64_t *node1addr,
    int32_t nodeid2, uint64_t *node2addr,
    uint64_t *physaddr) {

    debug_printf("alloc_common for mode=%d, bits=%d, nodeid1=%d, nodeid2=%d",
            mode, bits, nodeid1, nodeid2);

    errval_t err;
    if(mode == MODE_ALLOC_COMMON){
        err = skb_execute_query("alloc_common(%d,%"PRIi32",%"PRIi32").",
                bits, nodeid1, nodeid2);
    } else {
        assert(mode == MODE_MAP_COMMON);
        // We need physaddr to be passed
        assert(physaddr != NULL);
        assert(*physaddr != 0);
        err = skb_execute_query("map_common(%d,%"PRIu64",%"PRIi32",%"PRIi32").",
                bits, *physaddr, nodeid1, nodeid2);
    }

    DEBUG_SKB_ERR(err,"in alloc_common");
    if(err_is_fail(err)){
        skb_execute_query("dec_net_debug");
        return err;
    }
    char * skb_list_output =  strdup(skb_get_output());
    assert(skb_list_output);
    struct list_parser_status status;
    skb_read_list_init_offset(&status, skb_list_output, 0);

    uint64_t address = 0, nodeid=0;
    int list_idx = 0;
    while(skb_read_list(&status, "name(%"SCNu64", %"SCNu64")", &address, &nodeid)) {
        uint64_t *dest = NULL;
        switch(list_idx){
            case 0: dest = node1addr; break;
            case 1: dest = node2addr; break;
            case 2: dest = physaddr; break;
        }
        if(dest) *dest = address;
        list_idx++;
    }
    free(skb_list_output);
    if(list_idx != 3){
        return SKB_ERR_CONVERSION_ERROR;
    }

    return SYS_ERR_OK;
}

/*
 * allocates a piece of ram to be mapped into the driver and the devices
 * address spaces
 */
static errval_t iommu_alloc_ram(struct iommu_client *st,
                                 size_t bytes,
                                 struct capref *retcap)
{
    // TODO: This function has to pass the rootvnodeslot from iommu_get_mapping
    // region to the allocator function to get mem from that region.
    errval_t err, msgerr;

    if (bytes < (LARGE_PAGE_SIZE)) {
        bytes = LARGE_PAGE_SIZE;
    }
    int bits = log2ceil(bytes);
    bytes = 1 << bits;

    debug_printf("iommu_alloc_ram bytes=%lu\n", bytes);

    // Alloc cap slot
    err = slot_alloc(retcap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    struct mem_binding * b = get_mem_client();

    int32_t device_nodeid = driverkit_iommu_get_nodeid(st);
    int32_t my_nodeid = get_own_nodeid();
    uint64_t base_addr=0;

    err = alloc_common(MODE_ALLOC_COMMON, bits, device_nodeid, NULL, my_nodeid,
            NULL, &base_addr);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "allocate_common");
        return err;
    }

    debug_printf("Determined addr=0x%"PRIx64" as base address for bits=%d request\n",
            base_addr, bits);

    err = b->rpc_tx_vtbl.allocate(b, bits, base_addr, base_addr + bytes,
            &msgerr, retcap);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "allocate RPC");
        return err;
    }
    if(err_is_fail(msgerr)){
        DEBUG_ERR(msgerr, "allocate");
        return msgerr;
    }
    //debug_printf("alloc_ram_for_frame success\n");
    return SYS_ERR_OK;
}

static errval_t iommu_alloc_frame(struct iommu_client *cl,
                                  size_t bytes,
                                  struct capref *retcap)
{
    if (bytes < (LARGE_PAGE_SIZE)) {
        bytes = LARGE_PAGE_SIZE;
    }

    // Allocate RAM cap
    struct capref ramcap;
    errval_t err = iommu_alloc_ram(cl, bytes, &ramcap);
    if(err_is_fail(err)){
        return err;            
    }

    // Alloc cap slot
    err = slot_alloc(retcap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    // Get bits
    assert(bytes > 0);
    uint8_t bits = log2ceil(bytes);
    assert((1UL << bits) >= bytes);

    // This is doing what "create_ram_descendant" in
    // lib/barrelfish/capabilities.c is doing.
    err = cap_retype(*retcap, ramcap, 0, ObjType_Frame, (1UL << bits), 1);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_RETYPE);
    }

    err = cap_destroy(ramcap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_DESTROY);
    }

    return SYS_ERR_OK;
}

#define MAPPING_REGION_START (512UL << 31)
#define MAPPING_REGION_SIZE (512UL << 30)

#ifdef DISABLE_MODEL
static lvaddr_t vregion_map_base = MAPPING_REGION_START;
#endif
/*
 * returns a region of memory
 */
static errval_t iommu_alloc_vregion(struct iommu_client *st,
                                    struct capref mem,
                                    lvaddr_t *driver,
                                    dmem_daddr_t *device)
{
    errval_t err;
    struct frame_identity id;
    err = invoke_frame_identify(mem, &id);
    if (err_is_fail(err)) {
        return err;
    }

    if(st == NULL){
        *device = id.base;   
        *driver = 0;
        return SYS_ERR_OK;
    }

    assert(id.bytes >= LARGE_PAGE_SIZE);
    assert(st != NULL);

    int bits = log2ceil(id.bytes);
    int32_t device_nodeid = driverkit_iommu_get_nodeid(st);
    int32_t my_nodeid = get_own_nodeid();

    err = alloc_common(MODE_MAP_COMMON, bits, device_nodeid, device, 
            my_nodeid, driver, &id.base);

    if(err_is_fail(err)){
        DEBUG_ERR(err,"alloc_common"); 
        return err;
    }

    return SYS_ERR_OK;
}

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
    int32_t my_nodeid = get_own_nodeid();
    err = skb_execute_query("enum_node_id(%d, Id), mark_range_free(Id, %lu)",
           my_nodeid, driver);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "free process mem");
        return err;
    }
    // TODO: Actually free the own VSPACE

    return SYS_ERR_OK;
}

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

#define ROOT_SLOT_MEM_SIZE (512UL << 30)

static errval_t iommu_get_mapping_region(struct iommu_client *cl,
                                                dmem_daddr_t *start,
                                                dmem_daddr_t *end,
                                                uint16_t *rootvnodeslot)
{
    assert(rootvnodeslot != NULL);
    int32_t device_nodeid = driverkit_iommu_get_nodeid(cl);
    errval_t err = skb_execute_query(
        "enum_node_id(%d,Id),alloc_root_vnodeslot(Id, Slot),write(Slot)",
        device_nodeid);

    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err, "get root_vnodeslot");
        return err;
    }

    err = skb_read_output("%"SCNu16, rootvnodeslot);
    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err, "parse get root_vnodeslot output");
        return err;
    }

    debug_printf("%s: determined rootvnodeslot=%"PRIu16"\n", __FUNCTION__, *rootvnodeslot);

    *start = ROOT_SLOT_MEM_SIZE * (*rootvnodeslot) ;
    *end = ROOT_SLOT_MEM_SIZE * (*rootvnodeslot) + ROOT_SLOT_MEM_SIZE-1;

    assert(*rootvnodeslot == X86_64_PML4_BASE(*start));
    return err;
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


static errval_t driverkit_iommu_vnode_create_l3(struct iommu_client *cl)
{
    errval_t err;

    if (cl->vnode_l3) {
        return SYS_ERR_OK;
    }
    enum objtype l3_vnode_type;
    struct capref mapping;

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

    size_t l3_vnode_children =(1UL << vnode_entry_bits(l3_vnode_type));

    DRIVERKIT_DEBUG("l3_vnode_children=%zu (%u - %zu)\n", l3_vnode_children,
                 vnode_objbits(l3_vnode_type),
                 vnode_entry_bits(l3_vnode_type));

    /* XXX: this should be 512 based on our assumptions */
    assert(l3_vnode_children == 512);

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

    DRIVERKIT_DEBUG("MAPPING ROOT VNODE!\n");
    err = driverkit_iommu_map(cl, cl->rootvnode, cl->vnode_l3->vnode,
                              cl->rootvnode_slot, IOMMU_DEFAULT_VREGION_FLAGS,
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

        err = vnode_map(cap_vroot, cl->vnode_l3->vnode, cl->rootvnode_slot,
                        IOMMU_DEFAULT_VNODE_FLAGS, 0, 1, mapping);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to map the l3 vnode in drivers space");
            goto err_out5;
        }

        DRIVERKIT_DEBUG("Mapped at vroot [%u]\n", cl->rootvnode_slot);
    }

    return SYS_ERR_OK;
    err_out5:
    slot_free(mapping);
    err_out4:
    err = driverkit_iommu_unmap(cl, cl->rootvnode, cl->rootvnode_slot);
    err_out3:
    /* TODO: free vnode */
    err_out2:
    /* todo: free the mapping region */
    err_out:
    free(cl->vnode_l3);
    cl->vnode_l3 = NULL;
    return err;
}



static errval_t driverkit_iommu_vnode_get_l2(struct iommu_client *cl,
                                             dmem_daddr_t addr, uint64_t *retslot,
                                             struct iommu_vnode_l2 **ret_vnode)
{
    errval_t err;

    err = driverkit_iommu_vnode_create_l3(cl);
    if (err_is_fail(err)) {
        return err;
    }

    enum objtype l2_vnode_type;

    size_t l2_vnode_size = sizeof(struct iommu_vnode_l2);
    dmem_daddr_t end_addr;
    uint64_t slot;
    switch(cl->vnode_l3->vnode_type) {
        case ObjType_VNode_x86_64_pml5 :
            DRIVERKIT_DEBUG("Allocate PML4 for L2 node\n");
            l2_vnode_type = ObjType_VNode_x86_64_pml4;
            addr = addr & ~((512UL << 30) - 1);
            end_addr = addr + (512UL << 30) - 1;
            slot = X86_64_PML5_BASE(addr);
            *retslot = X86_64_PML4_BASE(addr);
            break;
        case ObjType_VNode_x86_64_pml4 :
            DRIVERKIT_DEBUG("Allocate PDPT for L2 node\n");
            l2_vnode_type = ObjType_VNode_x86_64_pdpt;
            addr = addr & ~X86_64_HUGE_PAGE_MASK;
            end_addr = addr + X86_64_HUGE_PAGE_SIZE - 1;
            slot = X86_64_PML4_BASE(addr);
            *retslot = X86_64_PDPT_BASE(addr);
            break;
        case ObjType_VNode_x86_64_pdpt :
            DRIVERKIT_DEBUG("Allocate PDIR for L2 node\n");
            l2_vnode_type = ObjType_VNode_x86_64_pdir;
            addr = addr & ~X86_64_LARGE_PAGE_MASK;
            end_addr = addr + X86_64_LARGE_PAGE_SIZE - 1;
            slot = X86_64_PDPT_BASE(addr);
            *retslot = X86_64_PDIR_BASE(addr);
            break;
        case ObjType_VNode_x86_64_pdir :
            DRIVERKIT_DEBUG("Allocate PTABLE for L2 node\n");
            l2_vnode_type = ObjType_VNode_x86_64_ptable;
            addr = addr & ~X86_64_BASE_PAGE_MASK;
            end_addr = addr + X86_64_BASE_PAGE_SIZE - 1;
            slot = X86_64_PDIR_BASE(addr);
            *retslot = X86_64_PTABLE_BASE(addr);
            break;
        default:
            return SYS_ERR_VNODE_TYPE;
    }

    if (cl->vnode_l3->children[slot]) {
        *ret_vnode = cl->vnode_l3->children[slot];
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

    err = driverkit_iommu_map(cl, cl->vnode_l3->vnode, vnode_l2->vnode, slot,
                              IOMMU_DEFAULT_VREGION_FLAGS, 0, 1);
    if (err_is_fail(err)) {
        goto err_out2;
    }

    cl->vnode_l3->children[slot] = vnode_l2;

    *ret_vnode = cl->vnode_l3->children[slot];

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
                                       struct dmem *dmem)
{
    errval_t err;

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    assert(id.bytes >= LARGE_PAGE_SIZE);

    dmem->vbase = 0;
    dmem->devaddr = 0;
    dmem->mem = frame;
    dmem->cl = cl;
    dmem->size = id.bytes;

    err = iommu_alloc_vregion(cl, frame, &dmem->vbase, &dmem->devaddr);
    if (err_is_fail(err)) {
        return err;
    }

    /*
     * if driver vbase is null, then we map it at any address in the driver's
     * vspace. Only if the policy is not shared, then we have to map it.
     */
    if (cl == NULL || cl->policy != IOMMU_VSPACE_POLICY_SHARED || !cl->enabled) {
        DRIVERKIT_DEBUG("%s:%u mapping in driver at 0x%" PRIxLVADDR "\n",
                     __FUNCTION__, __LINE__, dmem->vbase);
        if (dmem->vbase == 0) {
            err = vspace_map_one_frame_attr((void **)&dmem->vbase, dmem->size,
                                            dmem->mem, flags, NULL, NULL);
        } else {
            err = vspace_map_one_frame_fixed_attr(dmem->vbase, dmem->size,
                                                  dmem->mem, flags, NULL, NULL);
        }

        if (err_is_fail(err)) {
            goto err_out;
        }
    }

    if (cl == NULL || !cl->enabled) {
        return SYS_ERR_OK;
    }

    assert(dmem->vbase);
    assert(dmem->devaddr || dmem->devaddr == 0); //TODO: Maybe dont give 0 as valid dev address

    /* create L3 table if not created already */
    err = driverkit_iommu_vnode_create_l3(cl);
    if (err_is_fail(err)) {
        goto err_out;
    }

    assert(cl->vnode_l3);

    uint64_t ptecount = 1;
    uint64_t pagesize = 0;
    switch(cl->vnode_l3->vnode_type) {
        case ObjType_VNode_x86_64_pml4 :
            assert(dmem->size >= X86_64_HUGE_PAGE_SIZE &&
                   !(dmem->size & X86_64_HUGE_PAGE_MASK));
            ptecount = dmem->size / X86_64_HUGE_PAGE_SIZE;
            pagesize = X86_64_HUGE_PAGE_SIZE;
            break;
        case ObjType_VNode_x86_64_pdpt :
            assert(dmem->size >= X86_64_LARGE_PAGE_SIZE &&
                   !(dmem->size & X86_64_LARGE_PAGE_MASK));
            ptecount = dmem->size / X86_64_LARGE_PAGE_SIZE;
            pagesize = X86_64_LARGE_PAGE_SIZE;
            break;
        case ObjType_VNode_x86_64_pdir :
            assert(dmem->size >= X86_64_BASE_PAGE_SIZE &&
                   !(dmem->size & X86_64_BASE_PAGE_MASK));
            ptecount = dmem->size / X86_64_BASE_PAGE_SIZE;
            pagesize = X86_64_BASE_PAGE_SIZE;
            break;
        default:
            err = SYS_ERR_VNODE_TYPE;
            goto err_out2;
    }

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
    iommu_free_vregion(cl, dmem->vbase, dmem->devaddr);
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
    err = driverkit_iommu_alloc_frame(cl, bytes, &frame);
    if (err_is_fail(err)) {
        return err;
    }

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

