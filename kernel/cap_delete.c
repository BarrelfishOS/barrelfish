/**
 * \file
 * \brief Kernel capability deletion-related operations
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <kernel.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/lmp.h>
#include <offsets.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <distcaps.h>
#include <dispatch.h>
#include <paging_kernel_arch.h>
#include <mdb/mdb.h>
#include <mdb/mdb_tree.h>
#include <trace/trace.h>
#include <wakeup.h>
#include <kcb.h>

struct cte *clear_head, *clear_tail;
struct cte *delete_head, *delete_tail;

static errval_t caps_try_delete(struct cte *cte);
static errval_t cleanup_copy(struct cte *cte);
static errval_t cleanup_last(struct cte *cte, struct cte *ret_ram_cap);
static void caps_mark_revoke_copy(struct cte *cte);
static void caps_mark_revoke_generic(struct cte *cte);
static void clear_list_prepend(struct cte *cte);
static errval_t caps_copyout_last(struct cte *target, struct cte *ret_cte);

static uint32_t seqnum = 0;

static inline struct cte *delete_list_remove_head(void)
{
    assert(delete_head);
    struct cte *ret = delete_head;
    if (delete_head->delete_node.next) {
        delete_head = delete_head->delete_node.next;
    } else {
        delete_head = delete_tail = NULL;
    }
    // Clear delete_node.next as clear list uses the same pointer
    ret->delete_node.next = NULL;
    return ret;
}

static inline void delete_list_insert_head(struct cte *cte)
{
    if (!delete_head) {
        assert(!delete_tail);
        delete_head = delete_tail = cte;
        cte->delete_node.next = NULL;
    }
    else {
        assert(delete_tail);
        cte->delete_node.next = delete_head;
        delete_head = cte;
    }
}

static inline void delete_list_insert_tail(struct cte *cte)
{
    if (!delete_tail) {
        assert(!delete_head);
        delete_head = delete_tail = cte;
        cte->delete_node.next = NULL;
    }
    else {
        assert(delete_head);
        assert(!delete_tail->delete_node.next);
        delete_tail->delete_node.next = cte;
        delete_tail = cte;
        cte->delete_node.next = NULL;
    }
}

/**
 * \brief Try a "simple" delete of a cap. If this fails, the monitor needs to
 * negotiate a delete across the system.
 */
static errval_t caps_try_delete(struct cte *cte)
{
    TRACE(KERNEL_CAPOPS, TRY_DELETE, seqnum);
    TRACE_CAP_MSG("trying simple delete", cte);
    if (distcap_is_in_delete(cte) || cte->mdbnode.locked) {
        // locked or already in process of being deleted
        return SYS_ERR_CAP_LOCKED;
    }
    TRACE(KERNEL_CAPOPS, HAS_COPIES, seqnum);
    bool cap_has_copies = has_copies(cte);
    if (distcap_is_foreign(cte) || cap_has_copies) {
        return cleanup_copy(cte);
    }
    else if (cte->mdbnode.remote_copies
             || cte->cap.type == ObjType_L1CNode
             || cte->cap.type == ObjType_L2CNode
             || cte->cap.type == ObjType_Dispatcher)
    {
        return SYS_ERR_DELETE_LAST_OWNED;
    }
    else {
        return cleanup_last(cte, NULL);
    }
}

/**
 * \brief Delete the last copy of a cap in the entire system.
 * \bug Somewhere in the delete process, the remote_ancs property should be
 *      propagated to (remote) immediate descendants.
 */
errval_t caps_delete_last(struct cte *cte, struct cte *ret_ram_cap)
{
    errval_t err;
    assert(!has_copies(cte));

    if (cte->mdbnode.remote_copies) {
        printk(LOG_WARN, "delete_last but remote_copies is set\n");
    }

    TRACE_CAP_MSG("deleting last", cte);

    // try simple delete
    // XXX: this really should always fail, enforce that? -MN
    // XXX: this is probably not the way we should enforce/check this -SG
    err = caps_try_delete(cte);
    if (err_no(err) != SYS_ERR_DELETE_LAST_OWNED &&
        err_no(err) != SYS_ERR_CAP_LOCKED) {
        return err;
    }

    // CNodes and dcbs contain further CTEs, so cannot simply be deleted
    // instead, we place them in a clear list, which is progressivly worked
    // through until each list element contains only ctes that point to
    // other CNodes or dcbs, at which point they are scheduled for final
    // deletion, which only happens when the clear lists are empty.

    if (cte->cap.type == ObjType_L1CNode ||
        cte->cap.type == ObjType_L2CNode)
    {
        debug(SUBSYS_CAPS, "deleting last copy of cnode: %p\n", cte);
        // Mark all non-Null slots for deletion
        for (cslot_t i = 0; i < cnode_get_slots(&cte->cap); i++) {
            struct cte *slot = caps_locate_slot(get_address(&cte->cap), i);
            caps_mark_revoke_generic(slot);
        }

        // At this point the cte we're deleting should always be removed from
        // the delete list.
        assert(cte->delete_node.next == NULL && delete_head != cte);
        cte->delete_node.next = NULL;
        clear_list_prepend(cte);

        return SYS_ERR_OK;
    }
    else if (cte->cap.type == ObjType_Dispatcher)
    {
        debug(SUBSYS_CAPS, "deleting last copy of dispatcher: %p\n", cte);
        struct capability *cap = &cte->cap;
        struct dcb *dcb = cap->u.dispatcher.dcb;

        // Remove from queue
        scheduler_remove(dcb);
        // Reset current if it was deleted
        if (dcb_current == dcb) {
            dcb_current = NULL;
        }

        // Remove from wakeup queue
        wakeup_remove(dcb);

        // Notify monitor
        if (monitor_ep.u.endpointlmp.listener == dcb) {
            printk(LOG_ERR, "monitor terminated; expect badness!\n");
            monitor_ep.u.endpointlmp.listener = NULL;
        } else if (monitor_ep.u.endpointlmp.listener != NULL) {
            uintptr_t payload = dcb->domain_id;
            err = lmp_deliver_payload(&monitor_ep, NULL, &payload, 1, false, false);
            if (err_is_fail(err)) {
                printk(LOG_NOTE, "while notifying monitor about domain exit: %"PRIuERRV".\n", err);
                printk(LOG_NOTE, "please add the console output to the following bug report: https://code.systems.ethz.ch/T78\n");
            }
            assert(err_is_ok(err));
        }

        caps_mark_revoke_generic(&dcb->cspace);
        caps_mark_revoke_generic(&dcb->disp_cte);
        assert(cte->delete_node.next == NULL || delete_head == cte);
        cte->delete_node.next = NULL;
        clear_list_prepend(cte);

        return SYS_ERR_OK;
    }
    else
    {
        // last copy, perform object cleanup
        return cleanup_last(cte, ret_ram_cap);
    }
}

errval_t caps_reclaim_ram(struct cte *ret_ram_cap)
{
    if (kcb_current->pending_ram_in_use > 0) {
        errval_t err;
        // grab last ram cap off array
        struct RAM ram = kcb_current->pending_ram[--kcb_current->pending_ram_in_use];
        if (dcb_current != monitor_ep.u.endpointlmp.listener) {
            printk(LOG_WARN, "sending fresh ram cap to non-monitor?\n");
        }
        assert(ret_ram_cap->cap.type == ObjType_Null);
        ret_ram_cap->cap.u.ram = ram;
        ret_ram_cap->cap.type = ObjType_RAM;
        err = mdb_insert(ret_ram_cap);
        assert(err_is_ok(err));
        TRACE_CAP_MSG("reclaimed", ret_ram_cap);
        // note: this is a "success" code!
        kcb_current->pending_ram[kcb_current->pending_ram_in_use] = (struct RAM){ 0 };
        return SYS_ERR_RAM_CAP_CREATED;
    }
    // if no caps to reclaim, return CAP_NOT_FOUND.
    return SYS_ERR_CAP_NOT_FOUND;
}

/**
 * \brief Cleanup a cap copy but not the object represented by the cap
 */
static errval_t
cleanup_copy(struct cte *cte)
{
    errval_t err;

    TRACE(KERNEL_CAPOPS, CLEANUP_COPY, seqnum);

    TRACE_CAP_MSG("cleaning up copy", cte);

    struct capability *cap = &cte->cap;

    if (type_is_vnode(cap->type) ||
        cap->type == ObjType_Frame ||
        cap->type == ObjType_DevFrame)
    {
        unmap_capability(cte);
    }

    if (distcap_is_foreign(cte)) {
        TRACE_CAP_MSG("cleaning up non-owned copy", cte);
        if (cte->mdbnode.remote_copies || cte->mdbnode.remote_descs) {
            struct cte *ancestor = mdb_find_ancestor(cte);
            if (ancestor) {
                mdb_set_relations(ancestor, RRELS_DESC_BIT, RRELS_DESC_BIT);
            }
        }
    }

    TRACE(KERNEL_CAPOPS, MDB_REMOVE, seqnum);
    err = mdb_remove(cte);
    if (err_is_fail(err)) {
        return err;
    }
    TRACE_CAP_MSG("cleaned up copy", cte);
    assert(!mdb_reachable(cte));
    memset(cte, 0, sizeof(*cte));

    return SYS_ERR_OK;
}

/**
 * \brief Cleanup the last cap copy for an object and the object itself
 */
STATIC_ASSERT(68 == ObjType_Num, "Knowledge of all RAM-backed cap types");
static errval_t
cleanup_last(struct cte *cte, struct cte *ret_ram_cap)
{
    errval_t err;
    TRACE(KERNEL_CAPOPS, CLEANUP_LAST, seqnum);

    TRACE_CAP_MSG("cleaning up last copy", cte);
    struct capability *cap = &cte->cap;

    assert(!has_copies(cte));
    if (cte->mdbnode.remote_copies) {
        printk(LOG_WARN, "cleanup_last but remote_copies is set\n");
    }

    // When deleting the last copy of a mapping cap, destroy the mapping
    if (type_is_mapping(cte->cap.type)) {
        struct Frame_Mapping *mapping = &cte->cap.u.frame_mapping;
        // Only if the ptable the mapping is pointing to is a vnode type
        if (type_is_vnode(mapping->ptable->cap.type)) {
            err = page_mappings_unmap(&mapping->ptable->cap, cte);
            if (err_is_fail(err)) {
                char buf[256];
                sprint_cap(buf, 256, &cte->cap);
                printk(LOG_WARN, "page_mappings_unmap failed for %s\n", buf);
                return err;
            }
        }
    }

    if (ret_ram_cap && ret_ram_cap->cap.type != ObjType_Null) {
        return SYS_ERR_SLOT_IN_USE;
    }

    struct RAM ram = { .bytes = 0 };
    size_t len = sizeof(struct RAM) / sizeof(uintptr_t) + 1;

    if (!has_descendants(cte) && !has_ancestors(cte)) {
        // List all RAM-backed capabilities here
        // NB: ObjType_PhysAddr and ObjType_DevFrame caps are *not* RAM-backed!
        switch(cap->type) {
        case ObjType_RAM:
        case ObjType_Frame:
        case ObjType_EndPointUMP :
        case ObjType_L1CNode:
        case ObjType_L2CNode:
            ram.base = get_address(cap);
            ram.bytes = get_size(cap);
            break;

        case ObjType_Dispatcher:
            // Convert to genpaddr
            ram.base = local_phys_to_gen_phys(mem_to_local_phys((lvaddr_t)cap->u.dispatcher.dcb));
            ram.bytes = OBJSIZE_DISPATCHER;
            break;

        default:
            // Handle VNodes here
            if(type_is_vnode(cap->type)) {
                ram.base = get_address(cap);
                ram.bytes = vnode_objsize(cap->type);
            }
            break;
        }
    }

    // have cap to return to monitor but no allocated slot and no room in
    // monitor channel; have user retry over monitor rpc interface
    if (ram.bytes > 0 &&
        !ret_ram_cap &&
        monitor_ep.type == ObjType_EndPointLMP &&
        err_is_fail(lmp_can_deliver_payload(&monitor_ep, len)))
    {
        return SYS_ERR_RETRY_THROUGH_MONITOR;
    }


    err = cleanup_copy(cte);
    if (err_is_fail(err)) {
        return err;
    }

    if(ram.bytes > 0) {
        // Send back as RAM cap to monitor
        if (ret_ram_cap) {
            TRACE(KERNEL_CAPOPS, CREATE_RAM, seqnum);
            if (dcb_current != monitor_ep.u.endpointlmp.listener) {
                printk(LOG_WARN, "sending fresh ram cap to non-monitor?\n");
            }
            assert(ret_ram_cap->cap.type == ObjType_Null);
            ret_ram_cap->cap.u.ram = ram;
            ret_ram_cap->cap.type = ObjType_RAM;
            err = mdb_insert(ret_ram_cap);
            TRACE_CAP_MSG("reclaimed", ret_ram_cap);
            assert(err_is_ok(err));
            // note: this is a "success" code!
            err = SYS_ERR_RAM_CAP_CREATED;
        }
        else if (monitor_ep.type && monitor_ep.u.endpointlmp.listener != 0) {
#ifdef TRACE_PMEM_CAPS
            struct cte ramcte;
            memset(&ramcte, 0, sizeof(ramcte));
            ramcte.cap.u.ram = ram;
            ramcte.cap.type = ObjType_RAM;
            TRACE_CAP_MSG("reclaimed", &ramcte);
#endif
            TRACE(KERNEL_CAPOPS, CREATE_RAM_LMP, seqnum);
            // XXX: This looks pretty ugly. We need an interface.
            err = lmp_deliver_payload(&monitor_ep, NULL,
                                      (uintptr_t *)&ram,
                                      len, false, false);
        }
        else {
            // this is usually before the monitor is ready to get upcall when
            // a core is started.
            char *action = "dropping";
            if (kcb_current->pending_ram_in_use < 4) {
                action = "storing";
                kcb_current->pending_ram[kcb_current->pending_ram_in_use++] = ram;
            }
            printk(LOG_WARN, "%s ram cap base %08"PRIxGENPADDR" bytes 0x%"PRIxGENSIZE"\n", action, ram.base, ram.bytes);
        }
        if (err_no(err) == SYS_ERR_LMP_BUF_OVERFLOW) {
            // printk(LOG_WARN, "dropped ram cap base %08"PRIxGENPADDR" bytes 0x%"PRIxGENSIZE"\n", ram.base, ram.bytes);
            err = SYS_ERR_OK;

        } else {
            assert(err_is_ok(err));
        }
    }

    return err;
}

/*
 * Mark phase of revoke mark & sweep
 */

static void caps_mark_revoke_copy(struct cte *cte)
{
    errval_t err;
    err = caps_try_delete(cte);
    if (err_is_fail(err)) {
        // this should not happen as there is a copy of the cap
        panic("error while marking/deleting cap copy for revoke:"
              " %"PRIuERRV"\n", err);
    }
}

static void caps_mark_revoke_generic(struct cte *cte)
{
    errval_t err;

    if (cte->cap.type == ObjType_Null) {
        return;
    }
    if (distcap_is_in_delete(cte)) {
        return;
    }

    TRACE_CAP_MSG("marking for revoke", cte);

    err = caps_try_delete(cte);
    // If we get RETRY_THROUGH_MONITOR we're trying to delete a RAM-derived
    // cap that is the last one covering the region; and need to delete it in
    // a proper delete step.
    if (err_no(err) == SYS_ERR_DELETE_LAST_OWNED ||
        err_no(err) == SYS_ERR_RETRY_THROUGH_MONITOR)
    {
        cte->mdbnode.in_delete = true;
        //cte->delete_node.next_slot = 0;

        // insert into delete list
        delete_list_insert_tail(cte);
        TRACE_CAP_MSG("inserted into delete list", cte);

        // because the monitors will perform a 2PC that deletes all foreign
        // copies before starting the delete steps, and because the in_delete
        // bit marks this cap as "busy" (see distcap_get_state), we can clear
        // the remote copies bit.
        cte->mdbnode.remote_copies = 0;
    }
    else if (err_is_fail(err)) {
        // some serious mojo went down in the cleanup voodoo
        panic("error while marking/deleting descendant cap for revoke:"
              " %"PRIuERRV"\n", err);
    } else {
        // slot should now be empty
        assert(cte->cap.type == ObjType_Null);
    }
}

/**
 * \brief Delete all copies of a foreign cap.
 */
errval_t caps_delete_foreigns(struct cte *cte)
{
    errval_t err;
    struct cte *next;
    if (cte->mdbnode.owner == my_core_id) {
        debug(SUBSYS_CAPS, "%s called on %d for %p, owner=%d\n",
                __FUNCTION__, my_core_id, cte, cte->mdbnode.owner);
        return SYS_ERR_DELETE_REMOTE_LOCAL;
    }
    assert(cte->mdbnode.owner != my_core_id);
    if (cte->mdbnode.in_delete) {
        printk(LOG_WARN,
               "foreign caps with in_delete set,"
               " this should not happen");
    }

    TRACE_CAP_MSG("del copies of", cte);

    // Cleanup copies that are > cte in MDB
    next = mdb_successor(cte);
    while (next && is_copy(&cte->cap, &next->cap))
    {
        assert(next->mdbnode.owner != my_core_id);
        if (next->mdbnode.in_delete) {
            printk(LOG_WARN,
                    "foreign caps with in_delete set,"
                    " this should not happen");
        }
        err = cleanup_copy(next);
        if (err_is_fail(err)) {
            panic("error while deleting extra foreign copy for remote_delete:"
                    " %"PRIuERRV"\n", err);
        }
        next = mdb_successor(next);
    }

    // Cleanup copies that are < cte in MDB
    next = mdb_predecessor(cte);
    while (next && is_copy(&cte->cap, &next->cap))
    {
        assert(next->mdbnode.owner != my_core_id);
        if (next->mdbnode.in_delete) {
            printk(LOG_WARN,
                    "foreign caps with in_delete set,"
                    " this should not happen");
        }
        err = cleanup_copy(next);
        if (err_is_fail(err)) {
            panic("error while deleting extra foreign copy for remote_delete:"
                    " %"PRIuERRV"\n", err);
        }
        next = mdb_predecessor(next);
    }

    // The capabilities should all be foreign, by nature of the request.
    // Foreign capabilities are rarely locked, since they can be deleted
    // immediately. The only time a foreign capability is locked is during
    // move and retrieve operations. In either case, the lock on the same
    // capability must also be acquired on the owner for the operation to
    // succeed. Thus, we can safely unlock any capability here iff the
    // monitor guarentees that this operation is only executed when the
    // capability is locked on the owner.
    cte->mdbnode.locked = false;
    err = caps_try_delete(cte);
    if (err_is_fail(err)) {
        panic("error while deleting foreign copy for remote_delete:"
              " %"PRIuERRV"\n", err);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Mark capabilities for a revoke operation.
 * \param base The data for the capability being revoked
 * \param revoked The revoke target if it is on this core. This specific
 *        capability copy will not be marked. If supplied, is_copy(base,
 *        &revoked->cap) must hold.
 * \returns
 *        - CAP_NOT_FOUND if no copies or desendants are present on this core.
 *        - SYS_ERR_OK otherwise.
 */
errval_t caps_mark_revoke(struct capability *base, struct cte *revoked)
{
    assert(base);
    assert(!revoked || revoked->mdbnode.owner == my_core_id);

    // SG: In the following code, 'prev' is kind of a misnomer, this is all
    // just contortions to iterate through all copies and descendants of a
    // given capability. We update prev to be able to iterate through the tree
    // even when we're going up and down the tree structure to find the next
    // predecessor/successor.  -2017-08-29.

    // to avoid multiple mdb_find_greater, we store the predecessor of the
    // current position.
    // prev can already be a descendant if there are only descendants of base
    // on this core.
    struct cte *prev = mdb_find_greater(base, true), *next = NULL;
    if (!prev || !(is_copy(base, &prev->cap)
              || is_ancestor(&prev->cap, base)))
    {
        return SYS_ERR_CAP_NOT_FOUND;
    }

    // Mark copies (backwards): we will never find descendants earlier in the
    // ordering. However we might find copies!
    for (next = mdb_predecessor(prev);
         next && is_copy(base, &next->cap);
         next = mdb_predecessor(prev))
    {
        if (next == revoked) {
            // do not delete the revoked capability, use it as the new prev
            // instead, and delete the old prev.
            next = prev;
            prev = revoked;
        }
        assert(revoked || next->mdbnode.owner != my_core_id);
        caps_mark_revoke_copy(next);
    }
    // Mark copies (forward), use updated "prev". When we're done with this
    // step next should be == revoked, if revoked != NULL, and succ(next)
    // should be the first descendant.
    for (next = mdb_successor(prev);
         next && is_copy(base, &next->cap);
         next = mdb_successor(prev))
    {
        // note: if next is a copy of base, prev will also be a copy
        if (next == revoked) {
            // do not delete the revoked capability, use it as the new prev
            // instead, and delete the old prev.
            next = prev;
            prev = revoked;
        }
        assert(revoked || next->mdbnode.owner != my_core_id);
        caps_mark_revoke_copy(next);
    }

    assert(!revoked || prev == revoked);
    assert(is_copy(&prev->cap, base) || is_ancestor(&prev->cap, base));

    // mdb_find_greater() will always find the first descendant if there's no
    // copies on the core, so we can just mark descendants forwards.
    // XXX: check that this is true! -SG, 2017-09-08.
    // Mark descendants forwards
    for (next = mdb_successor(prev);
         next && is_ancestor(&next->cap, base);
         next = mdb_successor(prev))
    {
        caps_mark_revoke_generic(next);
        if (next->cap.type) {
            // the cap has not been deleted, so we must use it as the new prev
            prev = next;
        }
    }

    if (prev != revoked && !prev->mdbnode.in_delete) {
        if (is_copy(base, &prev->cap)) {
            caps_mark_revoke_copy(prev);
        }
        else {
            // due to early termination the condition, prev must be a
            // descendant
            assert(is_ancestor(&prev->cap, base));
            caps_mark_revoke_generic(prev);
        }
    }

    return SYS_ERR_OK;
}

/*
 * Sweep phase
 */

static void clear_list_prepend(struct cte *cte)
{
    // make sure we don't break delete list by inserting cte that hasn't been
    // removed from delete list into clear list
    assert(cte->delete_node.next == NULL);

    if (!clear_tail) {
        assert(!clear_head);
        clear_head = clear_tail = cte;
        cte->delete_node.next = NULL;
    }
    else {
        assert(clear_head);
        cte->delete_node.next = clear_head;
        clear_head = cte;
    }
    TRACE_CAP_MSG("inserted into clear list", cte);
}

errval_t caps_delete_step(struct cte *ret_next)
{
    errval_t err = SYS_ERR_OK;

    assert(ret_next);
    assert(ret_next->cap.type == ObjType_Null);

    if (!delete_head) {
        assert(!delete_tail);
        return SYS_ERR_CAP_NOT_FOUND;
    }
    assert(delete_head->mdbnode.in_delete == true);

    TRACE_CAP_MSG("performing delete step", delete_head);
    // We remove the head of the delete list here, so that potential calls to
    // caps_delete_last() below, which may insert new elements into the delete
    // list, see the delete list in a consistent state, with the element
    // that's currently being delete removed. -SG, 2018-11-07.
    struct cte *cte = delete_list_remove_head();
    if (cte->mdbnode.locked) {
        err = SYS_ERR_CAP_LOCKED;
    }
    else if (distcap_is_foreign(cte) || has_copies(cte)) {
        err = cleanup_copy(cte);
    }
    else if (cte->mdbnode.remote_copies) {
        err = caps_copyout_last(cte, ret_next);
        if (err_is_ok(err)) {
            err = SYS_ERR_DELETE_LAST_OWNED;
        }
    }
    else {
        // Do delete last, which may enqueue cte on clear list
        err = caps_delete_last(cte, ret_next);
        if (err_is_fail(err)) {
            TRACE_CAP_MSG("delete last failed", cte);
            printk(LOG_WARN, "%s: caps_delete_last failed, reinserting cte=%p in delete list\n",
                    __FUNCTION__, cte);
            // if delete_last fails, reinsert cte in front of delete list
            delete_list_insert_head(cte);
        }
    }

     if (err_is_fail(err) && err_no(err) != SYS_ERR_DELETE_LAST_OWNED) {
         // something went wrong in one of the cases above,  reinsert cte at
         // head of delete list.
         // We don't reinsert when we get SYS_ERR_DELETE_LAST_OWNED, as in
         // that case the delete step succeeded but needs more work in the
         // monitor.
         delete_list_insert_head(cte);
     }

    return err;
}

errval_t caps_clear_step(struct cte *ret_ram_cap)
{
    errval_t err;
    assert(!delete_head);
    assert(!delete_tail);

    if (!clear_head) {
        assert(!clear_tail);
        return SYS_ERR_CAP_NOT_FOUND;
    }
    assert((clear_head == clear_tail) == (!clear_head->delete_node.next));

    struct cte *cte = clear_head;

#ifndef NDEBUG
    // some sanity checks
#define CHECK_SLOT(slot) do { \
    assert((slot)->cap.type == ObjType_Null \
           || (slot)->cap.type == ObjType_L1CNode \
           || (slot)->cap.type == ObjType_L2CNode \
           || (slot)->cap.type == ObjType_Dispatcher); \
    if ((slot)->cap.type != ObjType_Null) { \
        assert((slot)->mdbnode.in_delete); \
    } \
} while (0)

    if (cte->cap.type == ObjType_L1CNode ||
        cte->cap.type == ObjType_L2CNode)
    {
        for (cslot_t i = 0; i < cnode_get_slots(&cte->cap); i++) {
            struct cte *slot = caps_locate_slot(get_address(&cte->cap), i);
            CHECK_SLOT(slot);
        }
    }
    else if (cte->cap.type == ObjType_Dispatcher) {
        struct dcb *dcb = cte->cap.u.dispatcher.dcb;
        CHECK_SLOT(&dcb->cspace);
        CHECK_SLOT(&dcb->disp_cte);
    }
    else {
        panic("Non-CNode/Dispatcher cap type in clear list!");
    }

#undef CHECK_SLOT
#endif

    TRACE_CAP_MSG("caps_clear_step for", cte);
    struct cte *after = cte->delete_node.next;
    err = cleanup_last(cte, ret_ram_cap);
    if (err_is_ok(err)) {
        if (after) {
            clear_head = after;
        }
        else {
            clear_head = clear_tail = NULL;
        }
    }
    return err;
}

static errval_t caps_copyout_last(struct cte *target, struct cte *ret_cte)
{
    errval_t err;

    // create a copy in slot specified by the caller, then delete
    // `next` slot so the new copy is still the last copy.
    err = caps_copy_to_cte(ret_cte, target, false, 0, 0);
    if (err_is_fail(err)) {
        return err;
    }

    err = cleanup_copy(target);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * CNode invocations
 */

errval_t caps_delete(struct cte *cte)
{
    errval_t err;
    // we use the cte pointer as identifier for a set of trace points. This
    // works fine, as we cannot have interleaved cpu driver trace streams on a
    // single core.
    TRACE(KERNEL_CAPOPS, DELETE_ENTER, ++seqnum);

    TRACE_CAP_MSG("deleting", cte);

    if (cte->mdbnode.locked) {
        err = err_push(SYS_ERR_CAP_LOCKED, SYS_ERR_RETRY_THROUGH_MONITOR);
        TRACE(KERNEL_CAPOPS, DELETE_DONE, seqnum);
        return err;
    }

    err = caps_try_delete(cte);
    if (err_no(err) == SYS_ERR_DELETE_LAST_OWNED) {
        err = err_push(err, SYS_ERR_RETRY_THROUGH_MONITOR);
    }

    TRACE(KERNEL_CAPOPS, DELETE_DONE, seqnum);
    return err;
}

errval_t caps_revoke(struct cte *cte)
{
    TRACE_CAP_MSG("revoking", cte);

    if (cte->mdbnode.locked) {
        return SYS_ERR_CAP_LOCKED;
    }

    return SYS_ERR_RETRY_THROUGH_MONITOR;
}
