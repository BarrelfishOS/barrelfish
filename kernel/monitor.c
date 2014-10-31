/**
 * \file
 * \brief Arch-generic system calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <string.h>
#include <syscall.h>
#include <barrelfish_kpi/syscalls.h>
//#include <capabilities.h>
//#include <mdb/mdb.h>
#include <mdb/mdb_tree.h>
//#include <cap_predicates.h>
#include <dispatch.h>
#include <distcaps.h>
//#include <wakeup.h>
//#include <paging_kernel_helper.h>
//#include <exec.h>
//#include <irq.h>
//#include <trace/trace.h>

static errval_t sys_double_lookup(capaddr_t rptr, uint8_t rbits,
                                  capaddr_t tptr, uint8_t tbits,
                                  struct cte **cte);
static errval_t sys_retslot_lookup(capaddr_t cnptr, uint8_t cnbits,
                                   cslot_t slot, struct cte **cte);

struct sysret sys_monitor_register(capaddr_t ep_caddr)
{
    errval_t err;
    struct capability *ep;
    err = caps_lookup_cap(&dcb_current->cspace.cap, ep_caddr, CPTR_BITS, &ep,
                          CAPRIGHTS_READ);

    if(err_is_fail(err)) {
        printf("Failure looking up endpoint!\n");
        return SYSRET(err);
    }

    monitor_ep = *ep;

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_cap_has_relations(capaddr_t caddr, uint8_t vbits,
                                    uint8_t mask)
{
    errval_t err;

    struct cte *cap;
    err = caps_lookup_slot(&dcb_current->cspace.cap, caddr, vbits, &cap,
                           CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    uint8_t res = 0;
    if (mask & RRELS_COPY_BIT && has_copies(cap)) {
        res |= RRELS_COPY_BIT;
    }
    if (mask & RRELS_ANCS_BIT && has_ancestors(cap)) {
        res |= RRELS_ANCS_BIT;
    }
    if (mask & RRELS_DESC_BIT && has_descendants(cap)) {
        res |= RRELS_DESC_BIT;
    }

    return (struct sysret) { .error = SYS_ERR_OK, .value = res };
}

struct sysret sys_monitor_remote_relations(capaddr_t root_addr, uint8_t root_bits,
                                           capaddr_t cptr, uint8_t bits,
                                           uint8_t relations, uint8_t mask)
{
    errval_t err;

    struct cte *cte;
    err = sys_double_lookup(root_addr, root_bits, cptr, bits, &cte);
    if (err_is_fail(err)) {
        printf("%s: error in double_lookup: %"PRIuERRV"\n", __FUNCTION__, err);
        return SYSRET(err);
    }

#ifdef TRACE_PMEM_CAPS
    if (caps_should_trace(&cte->cap)) {
        char buf[512];
        static const char chars[] = "~~01";
#define MK01(b) ((int)((b)!=0))
#define BITC(BIT) (chars[(2*MK01(mask & BIT)+MK01(relations & BIT))])
        snprintf(buf, 512, "set remote: c %c, a %c, d %c",
                 BITC(RRELS_COPY_BIT), BITC(RRELS_ANCS_BIT),
                 BITC(RRELS_DESC_BIT));
#undef BITC
#undef MK01
        TRACE_CAP_MSG(buf, cte);
    }
#endif

    if (mask) {
        mdb_set_relations(cte, relations, mask);
    }

    relations = 0;
    if (cte->mdbnode.remote_copies) {
        relations |= RRELS_COPY_BIT;
    }
    if (cte->mdbnode.remote_ancs) {
        relations |= RRELS_ANCS_BIT;
    }
    if (cte->mdbnode.remote_descs) {
        relations |= RRELS_DESC_BIT;
    }

    return (struct sysret){ .error = SYS_ERR_OK, .value = relations };
}

struct sysret sys_monitor_identify_cap(struct capability *root,
                                       capaddr_t cptr, uint8_t bits,
                                       struct capability *retbuf)
{
    struct capability *cap;
    errval_t err = caps_lookup_cap(root, cptr, bits, &cap, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_IDENTIFY_LOOKUP));
    }

    // XXX: Write cap data directly back to user-space
    // FIXME: this should involve a pointer/range check for reliability,
    // but because the monitor is inherently trusted it's not a security hole
    *retbuf = *cap;

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_monitor_nullify_cap(capaddr_t cptr, uint8_t bits)
{
    struct capability *root = &dcb_current->cspace.cap;
    struct cte *cte;
    errval_t err = caps_lookup_slot(root, cptr, bits, &cte,
                                    CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    // remove from MDB
    remove_mapping(cte);

    // zero-out cap entry
    assert(!mdb_reachable(cte));
    memset(cte, 0, sizeof(*cte));

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_monitor_domain_id(capaddr_t cptr, domainid_t domain_id)
{
    struct capability *root = &dcb_current->cspace.cap;
    struct capability *disp;

    errval_t err = caps_lookup_cap(root, cptr, CPTR_BITS, &disp,
                                   CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    disp->u.dispatcher.dcb->domain_id = domain_id;

    return SYSRET(SYS_ERR_OK);
}

static errval_t sys_double_lookup(capaddr_t rptr, uint8_t rbits,
                                  capaddr_t tptr, uint8_t tbits,
                                  struct cte **cte)
{
    errval_t err;

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, rptr, rbits,
                          &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_ROOT_CAP_LOOKUP);
    }

    err = caps_lookup_slot(root, tptr, tbits, cte, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_IDENTIFY_LOOKUP);
    }

    return SYS_ERR_OK;
}

struct sysret sys_get_cap_owner(capaddr_t root_addr, uint8_t root_bits, capaddr_t cptr, uint8_t bits)
{
    errval_t err;

    struct cte *cte;
    err = sys_double_lookup(root_addr, root_bits, cptr, bits, &cte);
    if (err_is_fail(err)) {
        printf("%s: error in double_lookup: %"PRIuERRV"\n", __FUNCTION__, err);
        return SYSRET(err);
    }

    return (struct sysret) { .error = SYS_ERR_OK, .value = cte->mdbnode.owner };
}

struct sysret sys_set_cap_owner(capaddr_t root_addr, uint8_t root_bits, capaddr_t cptr, uint8_t bits, coreid_t owner)
{
    errval_t err;

    struct cte *cte;
    err = sys_double_lookup(root_addr, root_bits, cptr, bits, &cte);
    if (err_is_fail(err)) {
        printf("%s: error in double_lookup: %"PRIuERRV"\n", __FUNCTION__, err);
        return SYSRET(err);
    }

    cte->mdbnode.owner = owner;

    TRACE_CAP(cte);

    struct cte *pred = cte;
    do {
        pred->mdbnode.owner = owner;
        pred = mdb_predecessor(pred);
    } while (is_copy(&pred->cap, &cte->cap));

    struct cte *succ = cte;
    do {
        succ->mdbnode.owner = owner;
        succ = mdb_successor(succ);
    } while (is_copy(&succ->cap, &cte->cap));

    return SYSRET(SYS_ERR_OK);
}

static void sys_lock_cap_common(struct cte *cte, bool lock)
{
    struct cte *pred = cte;
    do {
        pred->mdbnode.locked = lock;
        pred = mdb_predecessor(pred);
    } while (is_copy(&pred->cap, &cte->cap));

    struct cte *succ = cte;
    do {
        succ->mdbnode.locked = lock;
        succ = mdb_successor(succ);
    } while (is_copy(&succ->cap, &cte->cap));
}

struct sysret sys_lock_cap(capaddr_t root_addr, uint8_t root_bits, capaddr_t target_addr, uint8_t target_bits)
{
    errval_t err;

    struct cte *target;
    err = sys_double_lookup(root_addr, root_bits, target_addr, target_bits, &target);
    if (err_is_fail(err)) {
        printf("%s: error in double_lookup: %"PRIuERRV"\n", __FUNCTION__, err);
        return SYSRET(err);
    }

    if (target->mdbnode.locked) {
        return SYSRET(SYS_ERR_CAP_LOCKED);
    }

    TRACE_CAP(target);

    sys_lock_cap_common(target, true);
    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_unlock_cap(capaddr_t root_addr, uint8_t root_bits, capaddr_t target_addr, uint8_t target_bits)
{
    errval_t err;

    struct cte *target;
    err = sys_double_lookup(root_addr, root_bits, target_addr, target_bits, &target);
    if (err_is_fail(err)) {
        printf("%s: error in double_lookup: %"PRIuERRV"\n", __FUNCTION__, err);
        return SYSRET(err);
    }

    TRACE_CAP(target);

    // XXX: check if already unlocked? -MN
    sys_lock_cap_common(target, false);
    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_monitor_copy_existing(struct capability *src,
                                        capaddr_t cnode_cptr,
                                        uint8_t cnode_vbits,
                                        cslot_t slot)
{
    struct cte *copy = mdb_find_equal(src);
    if (!copy || copy->mdbnode.in_delete) {
        return SYSRET(SYS_ERR_CAP_NOT_FOUND);
    }

    struct cte *cnode;
    errval_t err = caps_lookup_slot(&dcb_current->cspace.cap, cnode_cptr,
                                    cnode_vbits, &cnode, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_SLOT_LOOKUP_FAIL));
    }
    if (cnode->cap.type != ObjType_CNode) {
        return SYSRET(SYS_ERR_CNODE_TYPE);
    }

    return SYSRET(caps_copy_to_cnode(cnode, slot, copy, false, 0, 0));
}

struct sysret sys_monitor_delete_last(capaddr_t root_addr, uint8_t root_bits,
                                      capaddr_t target_addr, uint8_t target_bits,
                                      capaddr_t ret_cn_addr, uint8_t ret_cn_bits,
                                      cslot_t ret_slot)
{
    errval_t err;

    struct cte *target;
    err = sys_double_lookup(root_addr, root_bits, target_addr, target_bits, &target);
    if (err_is_fail(err)) {
        printf("%s: root_addr: %"PRIxCADDR", root_bits: %"PRIu8
               ", target_addr: %"PRIxCADDR", target_bits: %"PRIu8"\n",
               __FUNCTION__, root_addr, root_bits, target_addr, target_bits);

        printf("%s: error in double_lookup: %"PRIxERRV"\n", __FUNCTION__, err);
        return SYSRET(err);
    }

    struct capability *retcn;
    err = caps_lookup_cap(&dcb_current->cspace.cap, ret_cn_addr, ret_cn_bits, &retcn, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_CNODE_LOOKUP));
    }

    if (retcn->type != ObjType_CNode) {
        return SYSRET(SYS_ERR_DEST_CNODE_INVALID);
    }
    if (ret_slot > (1<<retcn->u.cnode.bits)) {
        return SYSRET(SYS_ERR_SLOTS_INVALID);
    }

    struct cte *retslot = caps_locate_slot(retcn->u.cnode.cnode, ret_slot);

    return SYSRET(caps_delete_last(target, retslot));
}

struct sysret sys_monitor_delete_foreigns(capaddr_t cptr, uint8_t bits)
{
    errval_t err;

    struct cte *cte;
    err = caps_lookup_slot(&dcb_current->cspace.cap, cptr, bits, &cte, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    return SYSRET(caps_delete_foreigns(cte));
}

struct sysret sys_monitor_revoke_mark_tgt(capaddr_t root_addr, uint8_t root_bits,
                                          capaddr_t target_addr, uint8_t target_bits)
{
    errval_t err;

    struct cte *target;
    err = sys_double_lookup(root_addr, root_bits, target_addr, target_bits, &target);
    if (err_is_fail(err)) {
        printf("%s: error in double_lookup: %"PRIuERRV"\n", __FUNCTION__, err);
        return SYSRET(err);
    }

    return SYSRET(caps_mark_revoke(&target->cap, target));
}

struct sysret sys_monitor_revoke_mark_rels(struct capability *base)
{
    return SYSRET(caps_mark_revoke(base, NULL));
}

static errval_t sys_retslot_lookup(capaddr_t cnptr, uint8_t cnbits,
                                   cslot_t slot, struct cte **cte)
{
    errval_t err;

    struct capability *retcn;
    err = caps_lookup_cap(&dcb_current->cspace.cap, cnptr, cnbits, &retcn, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_DEST_CNODE_LOOKUP);
    }

    if (retcn->type != ObjType_CNode) {
        return SYS_ERR_DEST_CNODE_INVALID;
    }
    if (slot > (1<<retcn->u.cnode.bits)) {
        return SYS_ERR_SLOTS_INVALID;
    }

    struct cte *retslot;
    retslot = caps_locate_slot(retcn->u.cnode.cnode, slot);

    if (retslot->cap.type != ObjType_Null) {
        return SYS_ERR_SLOT_IN_USE;
    }

    *cte = retslot;
    return SYS_ERR_OK;
}

struct sysret sys_monitor_delete_step(capaddr_t ret_cn_addr,
                                     uint8_t ret_cn_bits,
                                     cslot_t ret_slot)
{
    errval_t err;

    struct cte *retslot;
    err = sys_retslot_lookup(ret_cn_addr, ret_cn_bits, ret_slot, &retslot);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    return SYSRET(caps_delete_step(retslot));
}

struct sysret sys_monitor_clear_step(capaddr_t ret_cn_addr,
                                     uint8_t ret_cn_bits,
                                     cslot_t ret_slot)
{
    errval_t err;

    struct cte *retslot;
    err = sys_retslot_lookup(ret_cn_addr, ret_cn_bits, ret_slot, &retslot);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    return SYSRET(caps_clear_step(retslot));
}
