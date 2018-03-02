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

/* XXX: have this as caps, VTD_ROOT_TABLE and VTD_CTX_TABLE */

errval_t vtd_root_table_create(struct vtd_root_table *rt, nodeid_t proximity)
{
    errval_t err;

    INTEL_VTD_DEBUG_RTABLE("creating root table\n");

    /* allocate slots for capability and */

    err = slot_alloc_root(&rt->mappingcncap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    err = slot_alloc(&rt->rtcap);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_SLOT_ALLOC);
        goto err_out_1;
    }

    struct capref ramcap;
    err = iommu_allocate_ram(&ramcap, BASE_PAGE_BITS, proximity);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_RAM_ALLOC);
        goto err_out_2;
    }

    struct capref ramcap2;
    err = iommu_allocate_ram(&ramcap2, L2_CNODE_BITS + OBJBITS_CTE, proximity);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_RAM_ALLOC);
        goto err_out_3;
    }

    err = cap_retype(rt->rtcap, ramcap, 0, ObjType_VNode_VTd_root_table,
                     vnode_objsize(ObjType_VNode_VTd_root_table), 1);
    if (err_is_fail(err)) {
        err =  err_push(err, LIB_ERR_CAP_RETYPE);
        goto err_out_4;
    }

    err = cnode_create_from_mem(rt->mappingcncap, ramcap2, ObjType_L2CNode,
                                &rt->mappigncn, L2_CNODE_SLOTS);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_CNODE_CREATE_FROM_MEM);
        goto err_out_5;
    }

    /* clean up */

    err = cap_destroy(ramcap);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_CAP_DESTROY);
        DEBUG_ERR(err, "ignoring destorying of ramcap\n");
    }

    err = cap_destroy(ramcap2);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_CAP_DESTROY);
        DEBUG_ERR(err, "ignoring destorying of ramcap\n");
    }

    return SYS_ERR_OK;

    err_out_5:
    cap_destroy(rt->rtcap);
    err_out_4:
    cap_destroy(ramcap2);
    err_out_3:
    cap_destroy(ramcap);
    err_out_2:
    slot_free(rt->rtcap);
    err_out_1:
    slot_free(rt->mappingcncap);
    return err;
}


errval_t vtd_root_table_destroy(struct vtd_root_table *rt)
{
    INTEL_VTD_DEBUG_RTABLE("destroying root table\n");

    errval_t err;

    /* delete the cnode cap */
    if (!capref_is_null(rt->rtcap)) {
        err = cap_destroy(rt->rtcap);
        assert(err_is_ok(err));
    }

    if (!capref_is_null(rt->mappingcncap)) {
        err = cap_destroy(rt->mappingcncap);
        assert(err_is_ok(err));
    }

    return SYS_ERR_OK;
}


errval_t vtd_root_table_map(struct vtd_root_table *rt, size_t idx,
                            struct vtd_ctxt_table *ctx)
{
    errval_t err;

    if (!(idx < VTD_NUM_ROOT_ENTRIES)) {
        return SYS_ERR_SLOTS_INVALID;
    }

    struct capref mappingcap = {
        .cnode =rt->mappigncn,
        .slot = idx
    };

    struct vnode_identity id;
    err = invoke_vnode_identify(ctx->ctcap, &id);

    INTEL_VTD_DEBUG_RTABLE("mapping root table [%zu] -> 0x%" PRIx64 "\n",
                    idx, id.base);

    return vnode_map(rt->rtcap, ctx->ctcap, idx, 0, 0, 1, mappingcap);
}

errval_t vtd_root_table_map_all(struct vtd_root_table *rt,
                                struct vtd_ctxt_table *ctx)
{
    errval_t err;

    size_t i;
    for (i = 0; i < VTD_NUM_ROOT_ENTRIES; i++) {
        err = vtd_root_table_map(rt, i, &ctx[i]);
        if (err_is_fail(err)) {
            return err;
        }
    }

    return SYS_ERR_OK;
}

errval_t vtd_root_table_unmap(struct vtd_root_table *rt, size_t idx)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}

