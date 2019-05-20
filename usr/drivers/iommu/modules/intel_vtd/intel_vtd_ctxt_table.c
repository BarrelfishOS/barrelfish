/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <numa.h>

#include "intel_vtd.h"
#include "intel_vtd_ctxt_cache.h"


errval_t vtd_ctxt_table_create(struct vtd_ctxt_table *ct, struct vtd *vtd)
{

    errval_t err;

    INTEL_VTD_DEBUG_CTABLE("creating context table\n");
    debug_printf("creating context table\n");
    assert(capref_is_null(ct->ctcap));
    assert(capref_is_null(ct->mappingcncap));

    /* allocate slots for capability and */
    err = slot_alloc_root(&ct->mappingcncap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    err = slot_alloc(&ct->ctcap);
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

    err = cap_retype(ct->ctcap, ramcap, 0, ObjType_VNode_VTd_ctxt_table,
                     vnode_objsize(ObjType_VNode_VTd_ctxt_table), 1);
    if (err_is_fail(err)) {
        err =  err_push(err, LIB_ERR_CAP_RETYPE);
        goto err_out_4;
    }

    err = cnode_create_from_mem(ct->mappingcncap, ramcap2, ObjType_L2CNode,
                                &ct->mappigncn, L2_CNODE_SLOTS);
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
    cap_destroy(ct->ctcap);
    err_out_4:
    cap_destroy(ramcap2);
    err_out_3:
    cap_destroy(ramcap);
    err_out_2:
    slot_free(ct->ctcap);
    err_out_1:
    slot_free(ct->mappingcncap);
    return err;
}


errval_t vtd_ctxt_table_destroy(struct vtd_ctxt_table *ct)
{
    INTEL_VTD_DEBUG_CTABLE("destroying context table\n");
    errval_t err;

    /* delete the cnode cap */
    if (!capref_is_null(ct->ctcap)) {
        err = cap_destroy(ct->ctcap);
        assert(err_is_ok(err));
    }

    if (!capref_is_null(ct->mappingcncap)) {
        err = cap_destroy(ct->mappingcncap);
        assert(err_is_ok(err));
    }

    memset(ct, 0, sizeof(*ct));

    return SYS_ERR_OK;
}


errval_t vtd_ctxt_table_map(struct vtd_root_table *rt, uint8_t idx,
                            struct vtd_ctxt_table *ctx)
{
    errval_t err;

    INTEL_VTD_DEBUG_CTABLE("mapping at [%u]\n", idx);

    assert(ctx->root_table == NULL);
    assert(rt->ctxt_tables[idx] == NULL);

    struct capref mappingcap = {
        .cnode =rt->mappigncn,
        .slot = idx
    };

    err = vnode_map(rt->rtcap, ctx->ctcap, idx, 0, 0, 1, mappingcap);
    if (err_is_fail(err)) {
        return err;
    }

    vtd_ctxt_cache_invalidate(rt->vtd);
    ctx->root_table = rt;
    ctx->root_table_idx = idx;

    rt->ctxt_tables[idx] = ctx;

    return SYS_ERR_OK;
}


errval_t vtd_ctxt_table_unmap(struct vtd_ctxt_table *ct)
{
    errval_t err;

    INTEL_VTD_DEBUG_CTABLE("unmapping [%u]\n", ct->root_table_idx);

    struct vtd_root_table *rt = ct->root_table;
    assert(rt);
    assert(rt->ctxt_tables[ct->root_table_idx] == ct);

    struct capref mappingcap = {
        .cnode =rt->mappigncn,
        .slot = ct->root_table_idx
    };

    err = vnode_unmap(rt->rtcap, mappingcap);
    if (err_is_fail(err)) {
        return err;
    }

    // TODO seems to not work in vacherin
    //vtd_ctxt_cache_invalidate(rt->vtd);

    rt->ctxt_tables[ct->root_table_idx] = NULL;

    return SYS_ERR_OK;
}


bool vtd_ctxt_table_valid(struct vtd_ctxt_table *ct)
{
    return ct->root_table && !capref_is_null(ct->ctcap);
}


errval_t vtd_ctxt_table_get_by_id(struct vtd *vtd, uint8_t idx,
                                  struct vtd_ctxt_table **table)
{
    errval_t err;

    INTEL_VTD_DEBUG_CTABLE("get [%u]\n", idx);

    if (!vtd_ctxt_table_valid(&vtd->ctxt_tables[idx])) {
        err = vtd_ctxt_table_create(&vtd->ctxt_tables[idx], vtd);
        if (err_is_fail(err)) {
            return err;
        }

        err = vtd_ctxt_table_map(&vtd->root_table, idx, &vtd->ctxt_tables[idx]);
        if (err_is_fail(err)) {
            return err;
        }
    }

    *table = &vtd->ctxt_tables[idx];

    return SYS_ERR_OK;
}
