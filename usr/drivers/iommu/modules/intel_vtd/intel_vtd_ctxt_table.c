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


errval_t vtd_ctxt_table_create(struct vtd_ctxt_table *ct, struct vtd *vtd)
{

    errval_t err;

    INTEL_VTD_DEBUG_CTABLE("creating context table\n");
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


errval_t vtd_ctxt_table_map(struct vtd_ctxt_table *ctxt, struct vtd_domain *dom,
                            struct vtd_domain_mapping *mapping)
{
    errval_t err;

    struct vnode_identity id;
    err = invoke_vnode_identify(dom->ptroot, &id);
    if (err_is_fail(err)) {
        return err_push(err, VTD_ERR_INVALID_CAP);
    }

    switch(id.type) {
        case ObjType_VNode_x86_32_pdpt:
        case ObjType_VNode_x86_64_pml4:
        case ObjType_VNode_x86_64_pml5:
            break;
        default:
            return SYS_ERR_VNODE_TYPE;
    }

    uintptr_t flags = (uintptr_t)vtd_domains_get_id(dom) << 8;
    if (vtd_device_tlb_present(mapping->vtd)) {
        flags |= 1;
    }

    ///tlb enabled

    struct capref mappingcap = {
        .cnode =ctxt->mappigncn,
        .slot = mapping->idx
    };

    err = vnode_map(ctxt->ctcap, dom->ptroot, mapping->idx, flags, 0, 1,
                    mappingcap);
    if (err_is_fail(err))  {
        return err;
    }

    mapping->mappingcap = mappingcap;


    USER_PANIC("NYI");
    return SYS_ERR_OK;
}

errval_t vtd_ctxt_table_unmap(struct vtd_domain_mapping *mapping)
{
#if 0
    errval_t err;

    struct vtd_ctxt_table *ct = vtd_get_ctxt_table(mapping->vtd, mapping->idx);

    err = vnode_unmap(ct->ctcap, mapping->mappingcap);
    if (err_is_fail(err)) {
        return err;
    }

    mapping->mappingcap = NULL_CAP;
#endif
    return SYS_ERR_OK;
}


bool vtd_ctxt_table_valid(struct vtd_ctxt_table *ct)
{
    return ct->root_table && !capref_is_null(ct->ctcap);
}