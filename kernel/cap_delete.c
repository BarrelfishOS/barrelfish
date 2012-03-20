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
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
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

struct cte *clear_head, *clear_tail;
struct cte *delete_head;

static errval_t cleanup_copy(struct cte *cte);
static errval_t cleanup_last(struct cte *cte, struct cte *ret_ram_cap);
static errval_t cleanup_last_dcb(struct cte *cte);

/**
 * \brief Try a "simple" delete of a cap. If this fails, the monitor needs to
 * negotiate a delete across the system.
 */
errval_t caps_try_delete(struct cte *cte)
{
    if (distcap_is_in_delete(cte)) {
        // already in process of being deleted
        return SYS_ERR_OK;
    }

    if (!(distcap_is_foreign(cte) || has_copies(cte))) {
        return SYS_ERR_DELETE_LAST_OWNED;
    }

    return cleanup_copy(cte);
}

/**
 * \brief Delete the last copy of a cap in the entire system.
 */
errval_t caps_delete_last(struct cte *cte, struct cte *ret_ram_cap)
{
    errval_t err;

    // try simple delete
    // XXX: this really should always fail, enforce that? -MN
    err = caps_try_delete(cte);
    if (err_no(err) != SYS_ERR_DELETE_LAST_OWNED) {
        return err;
    }

    if (cte->cap.type == ObjType_CNode ||
        cte->cap.type == ObjType_Dispatcher)
    {
        // CNodes and dcbs contain further CTEs, so cannot simply be deleted
        // instead, we place them in a clear list, which is progressivly worked
        // through until each list element contains only ctes that point to
        // other CNodes or dcbs, at which point they are scheduled for final
        // deletion, which only happens when the clear lists are empty.

        if (cte->cap.type == ObjType_Dispatcher) {
            err = cleanup_last_dcb(cte);
            if (err_is_fail(err)) {
                return err;
            }
        }

        // insert at tail of cleanup list
        distcap_set_deleted(cte);
        if (clear_tail) {
            clear_tail->delete_node.next = cte;
        }
        else {
            assert(!clear_head);
            clear_head = cte;
        }
        clear_tail = cte;
        memset(&cte->delete_node, 0, sizeof(cte->delete_node));

        return SYS_ERR_OK;
    }
    else
    {
        // last copy, perform object cleanup
        return cleanup_last(cte, ret_ram_cap);
    }
}

/**
 * \brief Cleanup a cap copy but not the object represented by the cap
 */
static errval_t
cleanup_copy(struct cte *cte)
{
    errval_t err;
    printk(LOG_WARN, "cleanup_copy: NYI\n");

    struct capability *cap = &cte->cap;

    if (type_is_vnode(cap->type) || cap->type == ObjType_Frame || cap->type == ObjType_DevFrame) {
        // XXX: unmap if mapped
    }

    err = mdb_remove(cte);
    if (err_is_fail(err)) {
        return err;
    }
    memset(cte, 0, sizeof(*cte));

    return SYS_ERR_OK;
}

/**
 * \brief Cleanup the last cap copy for an object and the object itself
 */
static errval_t
cleanup_last(struct cte *cte, struct cte *ret_ram_cap)
{
    errval_t err;
    struct capability *cap = &cte->cap;

    if (ret_ram_cap && ret_ram_cap->cap.type != ObjType_Null) {
        return SYS_ERR_SLOT_IN_USE;
    }

    struct RAM ram = { .bits = 0 };
    size_t len = sizeof(struct RAM) / sizeof(uintptr_t) + 1;

    // List all RAM-backed capabilities here
    // NB: ObjType_PhysAddr and ObjType_DevFrame caps are *not* RAM-backed!
    switch(cap->type) {
    case ObjType_RAM:
        ram.base = cap->u.ram.base;
        ram.bits = cap->u.ram.bits;
        break;

    case ObjType_Frame:
        ram.base = cap->u.frame.base;
        ram.bits = cap->u.frame.bits;
        break;

    case ObjType_CNode:
        ram.base = cap->u.cnode.cnode;
        ram.bits = cap->u.cnode.bits + OBJBITS_CTE;
        break;

    case ObjType_Dispatcher:
        // Convert to genpaddr
        ram.base = local_phys_to_gen_phys(mem_to_local_phys((lvaddr_t)cap->u.dispatcher.dcb));
        ram.bits = OBJBITS_DISPATCHER;
        break;

    default:
        // Handle VNodes here
        if(type_is_vnode(cap->type)) {
            // XXX: Assumes that all VNodes store base as first
            // parameter and that it's a genpaddr_t
            ram.base = cap->u.vnode_x86_64_pml4.base;
            ram.bits = vnode_objbits(cap->type);
        }
        break;
    }

    err = cleanup_copy(cte);
    if (err_is_fail(err)) {
        return err;
    }

    if(ram.bits > 0) {
        // Send back as RAM cap to monitor
        if (ret_ram_cap) {
            if (dcb_current != monitor_ep.u.endpoint.listener) {
                printk(LOG_WARN, "sending fresh ram cap to non-monitor?\n");
            }
            assert(ret_ram_cap->cap.type == ObjType_Null);
            memcpy(&ret_ram_cap->cap, &ram, sizeof(struct capability));
            err = mdb_insert(ret_ram_cap);
            assert(err_is_ok(err));
            // note: this is a "success" code!
            err = SYS_ERR_RAM_CAP_CREATED;
        }
        else {
            // XXX: This looks pretty ugly. We need an interface.
            err = lmp_deliver_payload(&monitor_ep, NULL,
                                      (uintptr_t *)&ram,
                                      len, false);
        }
        assert(err_is_ok(err));
    }

    return SYS_ERR_OK;
}

/**
 * \brief Remove dispatcher from system
 */
static errval_t
cleanup_last_dcb(struct cte *cte)
{
    printk(LOG_WARN, "cleanup_dcb: NYI\n");

    struct capability *cap = &cte->cap;
    struct dcb *dcb = cap->u.dispatcher.dcb;
    // NOTE: do not clear CTEs in dcb as they are processed later

    // Remove from queue
    scheduler_remove(dcb);
    // Reset curent if it was deleted
    if (dcb_current == dcb) {
        dcb_current = NULL;
    }

    // Remove from wakeup queue
    wakeup_remove(dcb);

    // Notify monitor
    if (monitor_ep.u.endpoint.listener == dcb) {
        printk(LOG_ERR, "monitor terminated; expect badness!\n");
        monitor_ep.u.endpoint.listener = NULL;
    } else if (monitor_ep.u.endpoint.listener != NULL) {
        errval_t err;
        uintptr_t payload = dcb->domain_id;
        err = lmp_deliver_payload(&monitor_ep, NULL, &payload, 1, false);
        assert(err_is_ok(err));
    }

    return SYS_ERR_OK;
}

static errval_t caps_copyout_last(struct cte *target, struct cte *ret_cte);
static errval_t caps_clear_next_cnode(struct cte *cnode, struct cte *ret_next);
static errval_t caps_clear_next_dispatcher(struct cte *dispatcher, struct cte *ret_next);

/*
 * \brief Delete next element in cleared cnode/dcb graph
 */

errval_t caps_continue_clear(struct cte *ret_next)
{
    errval_t err = SYS_ERR_OK;
    assert(ret_next->cap.type == ObjType_Null);

    while (clear_head) {
        // get next element from clear list head
        struct cte *current = clear_head;

        // clear cap as far as possible
        switch (current->cap.type) {
        case ObjType_CNode:
            err = caps_clear_next_cnode(current, ret_next);
            break;
        case ObjType_Dispatcher:
            err = caps_clear_next_dispatcher(current, ret_next);
            break;
        default:
            printk(LOG_ERR, "Unexpected cap type in delete_next");
            break;
        }

        if (err_is_fail(err)) {
            return err;
        }

        // cap was cleared successfully, now contains only other cnodes and
        // dcbs marked as deleted

        // remove element from clear list head
        if (clear_tail == current) {
            assert(clear_head == current);
            clear_head = clear_tail = NULL;
        }
        else {
            clear_head = current->delete_node.next;
        }
        memset(&current->delete_node, 0, sizeof(current->delete_node));

        // put element on "final delete" queue
        current->delete_node.next = delete_head;
        delete_head = current;
    };

    assert(!clear_head);
    assert(!clear_tail);

    if (delete_head) {
        struct cte *cleared = delete_head;
        delete_head = cleared->delete_node.next;
        return cleanup_last(delete_head, ret_next);
    }

    return SYS_ERR_OK;
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

static errval_t
caps_clear_next_cnode(struct cte *cte, struct cte *ret_next)
{
    errval_t err = SYS_ERR_OK;

    assert(cte->cap.type == ObjType_CNode);
    struct CNode *cnode = &cte->cap.u.cnode;
    size_t size = 1 << cnode->bits;
    cslot_t index = cte->delete_node.next_slot;

    if (index > size) {
        printk(LOG_ERR, "Unexpected delete index into cnode slots");
        return SYS_ERR_OK;
    }

    // iterate through remaining slots until all are cleared or an issue is
    // encountered
    for (; err_is_ok(err) && index < size; index++) {
        // determine slot for current index
        struct cte *current = caps_locate_slot(cnode->cnode, index);

        if (current->cap.type == ObjType_Null) {
            // skip empty slots
            continue;
        }

        // try simple delete
        err = caps_try_delete(current);

        if (err_no(err) == SYS_ERR_DELETE_LAST_OWNED) {
            // create a copy in slot specified by the caller, then delete
            // `next` slot so the new copy is still the last copy.
            err = caps_copyout_last(current, ret_next);
            assert(err_is_ok(err));

            return SYS_ERR_DELETE_LAST_OWNED;
        }
    }

    return err;
}

static errval_t
caps_clear_next_dispatcher(struct cte *cte, struct cte *ret_next)
{
    errval_t err = SYS_ERR_OK;
    assert(cte->cap.type == ObjType_Dispatcher);
    cslot_t index = cte->delete_node.next_slot;
    struct dcb *dcb = cte->cap.u.dispatcher.dcb;
    size_t size = 2; // dcb has 2 slots

    if (index > size) {
        printk(LOG_ERR, "Unexpected delete index into dcb slots");
        return SYS_ERR_OK;
    }

    for (; err_is_ok(err) && index < size; index++) {
        // determine slot for current index
        struct cte *current;
        switch (index) {
        case 0:
            current = &dcb->cspace;
            break;
        case 1:
            current = &dcb->disp_cte;
            break;
        default:
            assert(false);
        }

        if (current->cap.type == ObjType_Null) {
            // skip empty slots
            continue;
        }

        // try simple delete
        err = caps_try_delete(current);

        if (err_no(err) == SYS_ERR_DELETE_LAST_OWNED) {
            // create a copy in slot specified by the caller, then delete
            // `next` slot so the new copy is still the last copy.
            err = caps_copyout_last(current, ret_next);
            assert(err_is_ok(err));
        }
    }

    return err;
}

errval_t caps_continue_revoke(struct cte *target, struct cte *ret_next)
{
    errval_t err;

    assert(ret_next);
    assert(ret_next->cap.type == ObjType_Null);

    // delete copies backwards
    struct cte *next = mdb_predecessor(target);
    while (next && is_copy(&next->cap, &target->cap)) {
        err = cleanup_copy(next);
        if (err_is_fail(err)) {
            printk(LOG_WARN, "Error during revoke: %"PRIuPTR"\n", err);
            return err;
        }
        next = mdb_predecessor(target);
    }

    // delete copies forwards
    next = mdb_successor(target);
    while (next && is_copy(&next->cap, &target->cap)) {
        err = cleanup_copy(next);
        if (err_is_fail(err)) {
            printk(LOG_WARN, "Error during revoke: %"PRIuPTR"\n", err);
            return err;
        }
        next = mdb_successor(target);
    }

    // delete descendants
    while (next && is_ancestor(&next->cap, &target->cap)) {
        // recursing here ensures leaves are fully deleted before their parents
        err = caps_continue_revoke(next, ret_next);
        if (err_is_fail(err)) {
            if (err_no(err) == SYS_ERR_DELETE_LAST_OWNED) {
                assert(ret_next->cap.type != ObjType_Null);
            }
            return err;
        }

        // try simple delete
        err = caps_try_delete(next);

        if (err_no(err) == SYS_ERR_DELETE_LAST_OWNED) {
            // create a copy in slot specified by the caller, then delete
            // `next` slot so the new copy is still the last copy.
            err = caps_copyout_last(next, ret_next);
            assert(err_is_ok(err));

            return SYS_ERR_DELETE_LAST_OWNED;
        }
        if (err_is_fail(err)) {
            return err;
        }

        next = mdb_successor(target);
    }

    return SYS_ERR_OK;
}

errval_t caps_delete(struct cte *cte)
{
    errval_t err;

    err = caps_try_delete(cte);
    if (err_no(err) == SYS_ERR_DELETE_LAST_OWNED) {
        err = err_push(err, SYS_ERR_RETRY_THROUGH_MONITOR);
    }

    return err;
}

errval_t caps_revoke(struct cte *cte)
{
    return SYS_ERR_RETRY_THROUGH_MONITOR;
}
