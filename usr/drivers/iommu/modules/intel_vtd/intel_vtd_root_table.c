/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <numa.h>

#include "intel_vtd.h"

/* XXX: have this as caps, VTD_ROOT_TABLE and VTD_CTX_TABLE */

errval_t vtd_root_table_create(struct vtd_root_table *rt, struct vtd *vtd)
{
    errval_t err;

    INTEL_VTD_DEBUG_RTABLE("creating root table\n");

    assert(capref_is_null(rt->rtcap));
    assert(capref_is_null(rt->mappingcncap));

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
    err = numa_ram_alloc_on_node(&ramcap, BASE_PAGE_SIZE, vtd->proximity_domain,
                                 NULL);
    if (err_is_fail(err)) {
        err = err_push(err, LIB_ERR_RAM_ALLOC);
        goto err_out_2;
    }

    struct capref ramcap2;
    err = numa_ram_alloc_on_node(&ramcap2, (1UL << (L2_CNODE_BITS + OBJBITS_CTE)),
                                 vtd->proximity_domain, NULL);
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

    rt->vtd = vtd;

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

    memset(rt, 0, sizeof(*rt));

    return SYS_ERR_OK;
}


errval_t vtd_root_table_map(struct vtd_root_table *rt, uint8_t idx,
                            struct vtd_ctxt_table *ctx)
{
    errval_t err;

    debug_printf("mapping root table[%u]\n", idx);

    struct capref mappingcap = {
        .cnode =rt->mappigncn,
        .slot = idx
    };

    err =vnode_map(rt->rtcap, ctx->ctcap, idx, 0, 0, 1, mappingcap);
    if (err_is_fail(err)) {
        return err;
    }

    ctx->root_table = rt;
    ctx->root_table_idx = idx;

    return SYS_ERR_OK;
}


errval_t vtd_root_table_unmap(struct vtd_root_table *rt, size_t idx)
{
    struct capref mappingcap = {
        .cnode =rt->mappigncn,
        .slot = idx
    };

    return vnode_unmap(rt->rtcap, mappingcap);
}

