/**
 * \file
 * \brief Driverkit module implementation.
 *
 * Contians helper functions to iterate over driver modules in a domain
 * and create driver instances from driver modules.
 */
/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>
#include <driverkit/hwmodel.h>
#include <collections/hash_table.h>
#include <skb/skb.h>
#include <if/mem_defs.h>
#include "debug.h"



__attribute__((unused))
static void format_nodelist(int32_t *nodes, char *out){
    *out = '\0';
    sprintf(out + strlen(out), "[");
    int first = 1;
    while(*nodes != 0){
        if(!first) sprintf(out + strlen(out), ",");
        sprintf(out + strlen(out), "%" PRIi32, *nodes);
        nodes++;
        first = 0;
    }
    sprintf(out + strlen(out), "]");
}

void driverkit_parse_namelist(char *in, struct hwmodel_name *names, int *conversions){
    assert(in);
    *conversions = 0;
    struct list_parser_status status;
    skb_read_list_init_offset(&status, in, 0);
    while(skb_read_list(&status, "name(%"SCNu64", %"SCNi32")",
                &names->address, &names->nodeid)) {
        debug_printf("parse_namelist: %lx\n", names->address);
        names++;
        *conversions += 1;
    }
}

#define ALLOC_WRAP_Q "state_get(S)," \
                     "alloc_wrap(S, %zu, %d, %"PRIi32",%s, NewS)," \
                     "state_set(NewS)."

errval_t driverkit_hwmodel_ram_alloc(struct capref *dst,
                                     size_t bytes, int32_t dstnode,
                                     int32_t *nodes)
{

    if (bytes < (LARGE_PAGE_SIZE)) {
        bytes = LARGE_PAGE_SIZE;
    }


    int bits = log2ceil(bytes);
    bytes = 1 << bits;
    assert(bits >= 21);
    // The PT configuration in the SKB is currently using 2M pages.

#ifdef DISABLE_MODEL
    if (dstnode != driverkit_hwmodel_lookup_dram_node_id()) {
        return LIB_ERR_RAM_ALLOC_MS_CONSTRAINTS;
    }
    return ram_alloc(dst, bits);
#else
    errval_t err, msgerr;
    char nodes_str[128];
    format_nodelist(nodes, nodes_str);


    int alloc_bits = 21;
    debug_printf("Query: " ALLOC_WRAP_Q "\n", bytes, alloc_bits, dstnode, nodes_str);
    err = skb_execute_query(ALLOC_WRAP_Q, bytes, alloc_bits, dstnode, nodes_str);

    DEBUG_SKB_ERR(err, "alloc_wrap");
    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err, "alloc_wrap");
        return err;
    }

    // Alloc cap slot
    err = slot_alloc(dst);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    struct hwmodel_name names[16];
    int num_conversions = 0;
    driverkit_parse_namelist(skb_get_output(), names, &num_conversions);
    assert(num_conversions > 0);

    struct mem_binding * b = get_mem_client();
    debug_printf("Determined addr=0x%"PRIx64" as address for (nodeid=%d, size=%zu) request\n",
            names[0].address, dstnode, bytes);

    err = b->rpc_tx_vtbl.allocate(b, bits, names[0].address, names[0].address + bytes,
            &msgerr, dst);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "allocate RPC");
        return err;
    }
    if(err_is_fail(msgerr)){
        DEBUG_ERR(msgerr, "allocate");
        return msgerr;
    }
    return SYS_ERR_OK;

#endif
}

errval_t driverkit_hwmodel_frame_alloc(struct capref *dst,
                                                     size_t bytes, int32_t dstnode,
                                                     int32_t *nodes)
{
#ifdef DISABLE_MODEL
    if (dstnode != driverkit_hwmodel_lookup_dram_node_id()) {
        return LIB_ERR_RAM_ALLOC_MS_CONSTRAINTS;
    }
    return frame_alloc(dst, bytes, NULL);
#else
    errval_t err;
    struct capref ram_cap; 

    if(bytes < LARGE_PAGE_SIZE) bytes = LARGE_PAGE_SIZE;

    // Allocate RAM cap
    err = driverkit_hwmodel_ram_alloc(&ram_cap, bytes, dstnode, nodes);
    if(err_is_fail(err)){
        return err;            
    }

    // Alloc cap slot
    err = slot_alloc(dst);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    // Get bits
    assert(bytes > 0);
    uint8_t bits = log2ceil(bytes);
    assert((1UL << bits) >= bytes);

    // This is doing what "create_ram_descendant" in
    // lib/barrelfish/capabilities.c is doing.
    err = cap_retype(*dst, ram_cap, 0, ObjType_Frame, (1UL << bits), 1);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_RETYPE);
    }

    err = cap_destroy(ram_cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_DESTROY);
    }

    return SYS_ERR_OK;
#endif
}


/**
 * fills in dmem->vbase + maps frame
 */
errval_t driverkit_hwmodel_vspace_map(int32_t nodeid, struct capref frame,
                                      vregion_flags_t flags, struct dmem *dmem)
{
    errval_t err;
    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

#ifdef DISABLE_MODEL
    dmem->devaddr = id.base;
    dmem->size = id.bytes;
    return vspace_map_one_frame_attr((void **)&dmem->vbase, id.bytes, frame,
            flags, NULL, NULL);
#else
    genpaddr_t addr;

    // Alloc vspace
    err = driverkit_hwmodel_vspace_alloc(frame, nodeid,  &addr);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "vspace_alloc");
        return err;
    }

    dmem->devaddr = id.base;
    dmem->size = id.bytes;

    return driverkit_hwmodel_vspace_map_fixed(nodeid, addr, frame, flags, dmem);
#endif

}

errval_t driverkit_hwmodel_vspace_map_fixed(int32_t nodeid,
                                                          genvaddr_t addr,
                                                          struct capref frame,
                                                          vregion_flags_t flags,
                                                          struct dmem *dmem)
{
    errval_t err;

    if(nodeid != driverkit_hwmodel_get_my_node_id()){
        return LIB_ERR_NOT_IMPLEMENTED;
    }

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    dmem->vbase = addr;

    return vspace_map_one_frame_fixed_attr(addr, id.bytes, frame, flags, NULL, NULL);
}


#define MAP_WRAP_Q  "state_get(S)," \
                    "map_wrap(S, %zu, 21, %"PRIi32", %"PRIu64", %s, NewS)," \
                    "state_set(NewS)."

errval_t driverkit_hwmodel_vspace_alloc(struct capref frame,
                                        int32_t nodeid, genvaddr_t *addr)
{
    errval_t err;

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    int32_t src_nodeid[2];
    char src_nodeid_str[128];
    src_nodeid[0] = nodeid;
    src_nodeid[1] = 0;
    format_nodelist(src_nodeid, src_nodeid_str);

    //int32_t mem_nodeid = id.pasid;
    int32_t mem_nodeid = driverkit_hwmodel_lookup_dram_node_id();
    uint64_t mem_addr = id.base;
    debug_printf("Query: " MAP_WRAP_Q "\n",
            id.bytes, mem_nodeid, mem_addr, src_nodeid_str);
    err = skb_execute_query(MAP_WRAP_Q,
            id.bytes, mem_nodeid, mem_addr, src_nodeid_str);
    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err, "map_wrap");
        return err;
    }
    
    struct hwmodel_name names[2];
    int num_conversions = 0;
    driverkit_parse_namelist(skb_get_output(), names, &num_conversions);
    assert(num_conversions == 2);
    //ignore, names[0] it is the resolved name as stored in frame
    *addr = names[1].address;
    debug_printf("Determined addr=0x%"PRIx64" as vbase for (nodeid=%d, size=%zu) request\n",
            *addr, nodeid, id.bytes);
    return SYS_ERR_OK;
}

/*
 *  Returns this process nodeid. It lazily adds the process' model node
 *  and returns it's identifier.
 */
int32_t driverkit_hwmodel_get_my_node_id(void)
{
    errval_t err;

    err = skb_client_connect();
    if (err_is_fail(err)) {
        return -1;
    }
    /*
     * XXX: this assumes the domain only runs on a single core!
     */
    static int32_t nodeid = -1;

    if(nodeid == -1){
        err = skb_execute_query(
            "state_get(S), "
            "add_process(S, E, NewS), writeln(E), "
            "state_set(NewS)");
        if (err_is_fail(err)) {
            DEBUG_SKB_ERR(err, "add_process");
            return -1;
        }
        err = skb_read_output("%d", &nodeid);
        assert(err_is_ok(err));
        DRIVERKIT_DEBUG("Instantiated new process model node, nodeid=%"PRIi32"\n",
                        nodeid);
    }
    return nodeid;
}

int32_t driverkit_hwmodel_lookup_dram_node_id(void)
{
#ifdef DISABLE_MODEL
    return 1;
#else
    return driverkit_hwmodel_lookup_node_id("[\"DRAM\"]");
#endif
}

int32_t driverkit_hwmodel_lookup_node_id(const char *path)
{
    errval_t err;
    err = skb_execute_query(
        "state_get(S), "
        "node_enum(S, %s, E, NewS), writeln(E), "
        "state_set(NewS)",
        path);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "query node_enum");
    }
    int32_t nodeid;
    err = skb_read_output("%d", &nodeid);
    assert(err_is_ok(err));
    return nodeid;
}

#define REVERSE_RESOLVE_Q "state_get(S)," \
                    "reverse_wrap(S, %"PRIi32", %"PRIu64", %zu, %"PRIi32")."

// Without reconfiguration, under what ret_addr can you reach dst
// from nodeid?
errval_t driverkit_hwmodel_reverse_resolve(struct capref dst, int32_t nodeid,
                                     genpaddr_t *ret_addr)
{

    errval_t err;
    struct frame_identity id;
    err = invoke_frame_identify(dst, &id);
    if (err_is_fail(err)) {
        return err;
    }
    assert(ret_addr);
#ifdef DISABLE_MODEL
    *ret_addr = id.base;
    return SYS_ERR_OK;
#else
    int dst_enum = id.pasid;
    dst_enum = driverkit_hwmodel_lookup_dram_node_id(); // Workaround

    debug_printf("Query: " REVERSE_RESOLVE_Q "\n", dst_enum, id.base, id.bytes, nodeid);
    err = skb_execute_query(REVERSE_RESOLVE_Q, dst_enum, id.base, id.bytes, nodeid);

    DEBUG_SKB_ERR(err, "reverse_resolve");
    if(err_is_fail(err)){
        DEBUG_SKB_ERR(err, "reverse_resolve");
        return err;
    }

    struct hwmodel_name names[1];
    int num_conversions = 0;
    driverkit_parse_namelist(skb_get_output(), names, &num_conversions);
    assert(num_conversions == 1);
    *ret_addr = names[0].address;

    debug_printf("Determined (0x%"PRIx64", %d) is alias of (0x%"PRIx64", %d)\n",
            names[0].address, nodeid, id.base, dst_enum);

    return SYS_ERR_OK;
#endif
}
