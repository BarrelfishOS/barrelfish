/**
 * \file
 * \brief Arch-generic system calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <string.h>
#include <syscall.h>
#include <barrelfish_kpi/syscalls.h>
#include <mdb/mdb_tree.h>
#include <dispatch.h>
#include <distcaps.h>

static errval_t sys_double_lookup(capaddr_t rptr, uint8_t rlevel,
                                  capaddr_t tptr, uint8_t tlevel,
                                  struct cte **cte);
static errval_t sys_retslot_lookup(capaddr_t cnptr, uint8_t cnlevel,
                                   cslot_t slot, struct cte **cte);

struct sysret sys_monitor_register(capaddr_t ep_caddr)
{
    errval_t err;
    struct capability *ep;
    err = caps_lookup_cap(&dcb_current->cspace.cap, ep_caddr, 2, &ep,
                          CAPRIGHTS_READ);

    if(err_is_fail(err)) {
        printf("Failure looking up endpoint!\n");
        return SYSRET(err);
    }

    monitor_ep = *ep;

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_cap_has_relations(capaddr_t caddr, uint8_t level,
                                    uint8_t mask)
{
    errval_t err;

    struct cte *cap;
    err = caps_lookup_slot(&dcb_current->cspace.cap, caddr, level, &cap,
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

struct sysret sys_monitor_remote_relations(capaddr_t root_addr, uint8_t root_level,
                                           capaddr_t cptr, uint8_t level,
                                           uint8_t relations, uint8_t mask)
{
    errval_t err;

    struct cte *cte;
    err = sys_double_lookup(root_addr, root_level, cptr, level, &cte);
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
                                       capaddr_t cptr, uint8_t level,
                                       struct capability *retbuf)
{
    struct capability *cap;
    errval_t err = caps_lookup_cap(root, cptr, level, &cap, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_IDENTIFY_LOOKUP));
    }

    // XXX: Write cap data directly back to user-space
    // FIXME: this should involve a pointer/range check for reliability,
    // but because the monitor is inherently trusted it's not a security hole
    *retbuf = *cap;

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_monitor_nullify_cap(capaddr_t cptr, uint8_t level)
{
    struct capability *root = &dcb_current->cspace.cap;
    struct cte *cte;
    errval_t err = caps_lookup_slot(root, cptr, level, &cte,
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

static errval_t sys_double_lookup(capaddr_t rptr, uint8_t rlevel,
                                  capaddr_t tptr, uint8_t tlevel,
                                  struct cte **cte)
{
    errval_t err;

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, rptr, rlevel,
                          &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_ROOT_CAP_LOOKUP);
    }

    err = caps_lookup_slot(root, tptr, tlevel, cte, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_IDENTIFY_LOOKUP);
    }

    return SYS_ERR_OK;
}

struct sysret sys_get_cap_owner(capaddr_t root_addr, uint8_t root_level, capaddr_t cptr, uint8_t level)
{
    errval_t err;

    struct cte *cte;
    err = sys_double_lookup(root_addr, root_level, cptr, level, &cte);
    if (err_is_fail(err)) {
        printf("%s: error in double_lookup: %"PRIuERRV"\n", __FUNCTION__, err);
        return SYSRET(err);
    }

    return (struct sysret) { .error = SYS_ERR_OK, .value = cte->mdbnode.owner };
}

struct sysret sys_set_cap_owner(capaddr_t root_addr, uint8_t root_level, capaddr_t cptr, uint8_t level, coreid_t owner)
{
    errval_t err;

    struct cte *cte;
    err = sys_double_lookup(root_addr, root_level, cptr, level, &cte);
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

struct sysret sys_lock_cap(capaddr_t root_addr, uint8_t root_level, capaddr_t target_addr, uint8_t target_level)
{
    errval_t err;

    struct cte *target;
    err = sys_double_lookup(root_addr, root_level, target_addr, target_level, &target);
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

struct sysret sys_unlock_cap(capaddr_t root_addr, uint8_t root_level, capaddr_t target_addr, uint8_t target_level)
{
    errval_t err;

    struct cte *target;
    err = sys_double_lookup(root_addr, root_level, target_addr, target_level, &target);
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
                                        capaddr_t croot_cptr,
                                        capaddr_t cnode_cptr,
                                        uint8_t cnode_level,
                                        cslot_t slot)
{
    struct cte *copy = mdb_find_equal(src);
    if (!copy || copy->mdbnode.in_delete) {
        return SYSRET(SYS_ERR_CAP_NOT_FOUND);
    }

    /* lookup cspace for cnode_cptr */
    struct capability *root;
    errval_t err = caps_lookup_cap(&dcb_current->cspace.cap, croot_cptr, 2,
                                   &root, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_ROOTCN_LOOKUP));
    }
    struct cte *cnode;
    err = caps_lookup_slot(root, cnode_cptr, cnode_level, &cnode,
                           CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_CNODE_LOOKUP));
    }
    // XXX: allow L1 Cnode here?
    if (cnode->cap.type != ObjType_L2CNode) {
        return SYSRET(SYS_ERR_CNODE_TYPE);
    }

    return SYSRET(caps_copy_to_cnode(cnode, slot, copy, false, 0, 0));
}

/**
 * \brief Check whether source has overlapping descendants
 */
struct sysret sys_monitor_is_retypeable(struct capability *source, gensize_t offset,
                                        gensize_t objsize, size_t count)
{
    struct cte *next = mdb_find_greater(source, false);
    if (!next || !is_ancestor(&next->cap, source)) {
        // next does not exist or is not a descendant of source
        return SYSRET(SYS_ERR_OK);
    }

    // Here: next is descendant of source; check for overlapping descendants
    // XXX: this is copied from caps_retype()
    errval_t err;
    int find_range_result = 0;
    struct cte *found_cte = NULL;
    err = mdb_find_range(get_type_root(source->type),
                         get_address(source) + offset,
                         objsize * count,
                         MDB_RANGE_FOUND_SURROUNDING,
                         &found_cte,
                         &find_range_result);
    // this should never return an error unless we mess up the
    // non-user supplied arguments
    if (err_is_fail(err)) {
        printk(LOG_WARN, "%s: mdb_find_range returned: %"PRIuERRV"\n", __FUNCTION__, err);
        // XXX: error
        return SYSRET(SYS_ERR_CAP_NOT_FOUND);
    }
    assert(err_is_ok(err));
    // return REVOKE_FIRST, if we found a cap inside the region
    // (FOUND_INNER == 2) or overlapping the region (FOUND_PARTIAL == 3)
    if (find_range_result >= MDB_RANGE_FOUND_INNER) {
        debug(SUBSYS_CAPS,
                "%s: found existing region inside, or overlapping requested region:\n",
                __FUNCTION__);
        return SYSRET(SYS_ERR_REVOKE_FIRST);
    }
    // return REVOKE_FIRST, if we found a cap that isn't our source
    // (or a copy of our source) covering the whole requested region.
    else if (find_range_result == MDB_RANGE_FOUND_SURROUNDING &&
            !is_copy(&found_cte->cap, source))
    {
        debug(SUBSYS_CAPS,
                "%s: found non source region fully covering requested region\n",
                __FUNCTION__);
        return SYSRET(SYS_ERR_REVOKE_FIRST);
    }

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_monitor_delete_last(capaddr_t root_addr, uint8_t root_level,
                                      capaddr_t target_addr, uint8_t target_level,
                                      capaddr_t ret_cn_addr, uint8_t ret_cn_level,
                                      cslot_t ret_slot)
{
    errval_t err;

    struct cte *target;
    err = sys_double_lookup(root_addr, root_level, target_addr, target_level, &target);
    if (err_is_fail(err)) {
        printf("%s: root_addr: %"PRIxCADDR", root_level: %"PRIu8
               ", target_addr: %"PRIxCADDR", target_level: %"PRIu8"\n",
               __FUNCTION__, root_addr, root_level, target_addr, target_level);

        printf("%s: error in double_lookup: %"PRIxERRV"\n", __FUNCTION__, err);
        return SYSRET(err);
    }

    struct capability *retcn;
    err = caps_lookup_cap(&dcb_current->cspace.cap, ret_cn_addr,
                          ret_cn_level, &retcn, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_CNODE_LOOKUP));
    }

    if (retcn->type != ObjType_L1CNode &&
        retcn->type != ObjType_L2CNode) {
        return SYSRET(SYS_ERR_DEST_CNODE_INVALID);
    }
    if (ret_slot > cnode_get_slots(retcn)) {
        return SYSRET(SYS_ERR_SLOTS_INVALID);
    }

    struct cte *retslot = caps_locate_slot(get_address(retcn), ret_slot);

    return SYSRET(caps_delete_last(target, retslot));
}

struct sysret sys_monitor_delete_foreigns(capaddr_t cptr, uint8_t level)
{
    errval_t err;

    struct cte *cte;
    err = caps_lookup_slot(&dcb_current->cspace.cap, cptr, level, &cte, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    return SYSRET(caps_delete_foreigns(cte));
}

struct sysret sys_monitor_revoke_mark_tgt(capaddr_t root_addr, uint8_t root_level,
                                          capaddr_t target_addr, uint8_t target_level)
{
    errval_t err;

    struct cte *target;
    err = sys_double_lookup(root_addr, root_level, target_addr, target_level, &target);
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

static errval_t sys_retslot_lookup(capaddr_t cnptr, uint8_t cnlevel,
                                   cslot_t slot, struct cte **cte)
{
    errval_t err;

    struct capability *retcn;
    err = caps_lookup_cap(&dcb_current->cspace.cap, cnptr, cnlevel,
                          &retcn, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_DEST_CNODE_LOOKUP);
    }

    if (retcn->type != ObjType_L1CNode &&
        retcn->type != ObjType_L2CNode) {
        return SYS_ERR_DEST_CNODE_INVALID;
    }
    if (slot > cnode_get_slots(retcn)) {
        return SYS_ERR_SLOTS_INVALID;
    }

    struct cte *retslot;
    retslot = caps_locate_slot(get_address(retcn), slot);

    if (retslot->cap.type != ObjType_Null) {
        return SYS_ERR_SLOT_IN_USE;
    }

    *cte = retslot;
    return SYS_ERR_OK;
}

struct sysret sys_monitor_delete_step(capaddr_t ret_cn_addr,
                                     uint8_t ret_cn_level,
                                     cslot_t ret_slot)
{
    errval_t err;

    struct cte *retslot;
    err = sys_retslot_lookup(ret_cn_addr, ret_cn_level, ret_slot, &retslot);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    return SYSRET(caps_delete_step(retslot));
}

struct sysret sys_monitor_clear_step(capaddr_t ret_cn_addr,
                                     uint8_t ret_cn_level,
                                     cslot_t ret_slot)
{
    errval_t err;

    struct cte *retslot;
    err = sys_retslot_lookup(ret_cn_addr, ret_cn_level, ret_slot, &retslot);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    return SYSRET(caps_clear_step(retslot));
}
