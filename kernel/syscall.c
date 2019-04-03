/**
 * \file
 * \brief Arch-generic system calls implementation.
 */

/*
 * Copyright (c) 2007-2010,2012, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <string.h>
#include <syscall.h>
#include <barrelfish_kpi/syscalls.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <coreboot.h>
#include <mdb/mdb.h>
#include <mdb/mdb_tree.h>
#include <cap_predicates.h>
#include <dispatch.h>
#include <distcaps.h>
#include <wakeup.h>
#include <paging_kernel_helper.h>
#include <paging_kernel_arch.h>
#include <exec.h>
#include <irq.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <kcb.h>
#include <useraccess.h>
#include <systime.h>

errval_t sys_print(const char *str, size_t length)
{
    /* FIXME: check that string is mapped and accessible to caller! */
    printf("%.*s", (int)length, str);
    return SYS_ERR_OK;
}

/* FIXME: lots of missing argument checks in this function */
struct sysret
sys_dispatcher_setup(struct capability *to, capaddr_t cptr, uint8_t level,
                     capaddr_t vptr, capaddr_t dptr, bool run, capaddr_t odptr)
{
    errval_t err = SYS_ERR_OK;
    assert(to->type == ObjType_Dispatcher);
    struct dcb *dcb = to->u.dispatcher.dcb;
    assert(dcb != dcb_current);

    lpaddr_t lpaddr;

    /* 0. Handle sys_dispatcher_setup for guest domains */
    if (cptr == 0x0) {
        assert(dcb->is_vm_guest);
        assert(vptr == 0x0);
        assert(dptr == 0x0);
        assert(odptr == 0x0);
        if (!dcb->is_vm_guest || vptr != 0x0 || dptr != 0x0 || odptr != 0x0) {
            return SYSRET(SYS_ERR_DISP_NOT_RUNNABLE);
        }
        if (run) {
            // Dispatchers run disabled the first time
            dcb->disabled = 1;
            make_runnable(dcb);
        }
        return SYSRET(SYS_ERR_OK);
    }

    assert(!dcb->is_vm_guest);
    assert(!cptr == 0x0);
    assert(!vptr == 0x0);
    assert(!dptr == 0x0);
    assert(!odptr == 0x0);

    if (cptr == 0x0 || vptr == 0x0 || dptr == 0x0 || odptr == 0x0) {
        return SYSRET(SYS_ERR_DISP_NOT_RUNNABLE);
    }

    /* 1. set cspace root */
    struct cte *root;
    err = caps_lookup_slot(&dcb_current->cspace.cap, cptr, level,
                           &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        debug(SUBSYS_CAPS, "caps_lookup_cap for croot=%"PRIxCADDR", level=%d: %"PRIuERRV"\n", cptr, level, err);
        return SYSRET(err_push(err, SYS_ERR_DISP_CSPACE_ROOT));
    }
    if (root->cap.type != ObjType_L1CNode) {
        return SYSRET(err_push(err, SYS_ERR_DISP_CSPACE_INVALID));
    }
    err = caps_copy_to_cte(&dcb->cspace, root, false, 0, 0);
    if (err_is_fail(err)) {
        debug(SUBSYS_CAPS, "caps_copy_to_cte for croot: %"PRIuERRV"\n", err);
        return SYSRET(err_push(err, SYS_ERR_DISP_CSPACE_ROOT));
    }

    /* 2. set vspace root */
    struct capability *vroot;
    err = caps_lookup_cap(&root->cap, vptr, CNODE_TYPE_COUNT, &vroot, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        debug(SUBSYS_CAPS, "caps_lookup_cap for vroot=%"PRIxCADDR": %"PRIuERRV"\n", vptr, err);
        return SYSRET(err_push(err, SYS_ERR_DISP_VSPACE_ROOT));
    }

    // Insert as dispatcher's VSpace root
    if (!type_is_vroot(vroot->type)) {
        return SYSRET(SYS_ERR_DISP_VSPACE_INVALID);
    }
    dcb->vspace = gen_phys_to_local_phys(get_address(vroot));

    /* 3. set dispatcher frame pointer */
    struct cte *dispcte;
    err = caps_lookup_slot(&root->cap, dptr, CNODE_TYPE_COUNT, &dispcte,
                           CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DISP_FRAME));
    }
    struct capability *dispcap = &dispcte->cap;
    if (dispcap->type != ObjType_Frame) {
        return SYSRET(SYS_ERR_DISP_FRAME_INVALID);
    }
    if (get_size(dispcap) < DISPATCHER_FRAME_SIZE) {
        return SYSRET(SYS_ERR_DISP_FRAME_SIZE);
    }
    /* FIXME: check rights? */

    lpaddr = gen_phys_to_local_phys(get_address(dispcap));
    dcb->disp = local_phys_to_mem(lpaddr);
    // Copy the cap to dcb also
    err = caps_copy_to_cte(&dcb->disp_cte, dispcte, false, 0, 0);
    // If copy fails, something wrong in kernel
    assert(err_is_ok(err));

    /* 5. Make runnable if desired */
    if (run) {
        if (dcb->vspace == 0 || dcb->disp == 0 || dcb->cspace.cap.type != ObjType_L1CNode) {
            return SYSRET(err_push(err, SYS_ERR_DISP_NOT_RUNNABLE));
        }

        // XXX: dispatchers run disabled the first time they start
        dcb->disabled = 1;
        //printf("DCB: %p %.*s\n", dcb, DISP_NAME_LEN, dcb->disp->name);
        make_runnable(dcb);
    }

    /* 6. Copy domain ID off given dispatcher */
    // XXX: We generally pass the current dispatcher as odisp, see e.g.
    // lib/spawndomain/spawn.c:spawn_run().  In that case the new domain gets
    // the same domain id as the domain doing the spawning. cf. T271
    // -SG, 2016-07-21.
    struct capability *odisp;
    err = caps_lookup_cap(&dcb_current->cspace.cap, odptr, CNODE_TYPE_COUNT,
                          &odisp, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DISP_OCAP_LOOKUP));
    }
    if (odisp->type != ObjType_Dispatcher) {
        return SYSRET(SYS_ERR_DISP_OCAP_TYPE);
    }
    dcb->domain_id = odisp->u.dispatcher.dcb->domain_id;

    /* 7. (HACK) Set current core id */
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(dcb->disp);
    disp->curr_core_id = my_core_id;

    /* 8. Enable tracing for new domain */
    err = trace_new_application(disp->name, (uintptr_t) dcb);

    if (err == TRACE_ERR_NO_BUFFER) {
        // Try to use the boot buffer.
        trace_new_boot_application(disp->name, (uintptr_t) dcb);
    }

    // Setup systime frequency
    disp->systime_frequency = systime_frequency;

    return SYSRET(SYS_ERR_OK);
}

struct sysret
sys_dispatcher_properties(struct capability *to,
                          enum task_type type, unsigned long deadline,
                          unsigned long wcet, unsigned long period,
                          unsigned long release, unsigned short weight)
{
    assert(to->type == ObjType_Dispatcher);

#ifdef CONFIG_SCHEDULER_RBED
    struct dcb *dcb = to->u.dispatcher.dcb;

    assert(type >= TASK_TYPE_BEST_EFFORT && type <= TASK_TYPE_HARD_REALTIME);
    assert(wcet <= deadline);
    assert(wcet <= period);
    assert(type != TASK_TYPE_BEST_EFFORT || weight > 0);

    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_SCHED_REMOVE,
                152);
    scheduler_remove(dcb);

    /* Set task properties */
    dcb->type = type;
    dcb->deadline = deadline;
    dcb->wcet = wcet;
    dcb->period = period;
    dcb->release_time = (release == 0) ? systime_now() : release;
    dcb->weight = weight;

    make_runnable(dcb);
#endif

    return SYSRET(SYS_ERR_OK);
}

/**
 * \param root                  Source CSpace root cnode to invoke
 * \param source_croot          Source capability cspace root
 * \param source_cptr           Source capability cptr
 * \param offset                Offset into source capability from which to retype
 * \param type                  Type to retype to
 * \param objsize               Object size for variable-sized types
 * \param count                 number of objects to create
 * \param dest_cspace_cptr      Destination CSpace cnode cptr relative to
 *                              source cspace root
 * \param dest_cnode_cptr       Destination cnode cptr
 * \param dest_slot             Destination slot number
 * \param dest_cnode_level      Level/depth of destination cnode
 */
struct sysret
sys_retype(struct capability *root, capaddr_t source_croot, capaddr_t source_cptr,
           gensize_t offset, enum objtype type, gensize_t objsize, size_t count,
           capaddr_t dest_cspace_cptr, capaddr_t dest_cnode_cptr,
           uint8_t dest_cnode_level, cslot_t dest_slot, bool from_monitor)
{
    errval_t err;

    /* Parameter checking */
    if (type == ObjType_Null || type >= ObjType_Num) {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    /* Lookup source cspace root cnode */
    struct capability *source_root;
    err = caps_lookup_cap(root, source_croot, 2, &source_root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_SOURCE_ROOTCN_LOOKUP));
    }
    /* Source capability */
    struct cte *source_cte;
    // XXX: level from where
    err = caps_lookup_slot(source_root, source_cptr, 2, &source_cte,
                           CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_SOURCE_CAP_LOOKUP));
    }
    assert(source_cte != NULL);

    /* Destination cspace root cnode in source cspace */
    struct capability *dest_cspace_root;
    // XXX: level from where?
    err = caps_lookup_cap(root, dest_cspace_cptr, 2,
                          &dest_cspace_root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_ROOTCN_LOOKUP));
    }
    /* dest_cspace_root must be L1 CNode */
    if (dest_cspace_root->type != ObjType_L1CNode) {
        return SYSRET(SYS_ERR_CNODE_TYPE);
    }

    /* Destination cnode in destination cspace */
    struct capability *dest_cnode_cap;
    err = caps_lookup_cap(dest_cspace_root, dest_cnode_cptr, dest_cnode_level,
                          &dest_cnode_cap, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_CNODE_LOOKUP));
    }

    /* check that destination cnode is actually a cnode */
    if (dest_cnode_cap->type != ObjType_L1CNode &&
        dest_cnode_cap->type != ObjType_L2CNode) {
        debug(SUBSYS_CAPS, "destcn type: %d\n", dest_cnode_cap->type);
        return SYSRET(SYS_ERR_DEST_CNODE_INVALID);
    }

    return SYSRET(caps_retype(type, objsize, count, dest_cnode_cap, dest_slot,
                              source_cte, offset, from_monitor));
}

struct sysret sys_create(struct capability *root, enum objtype type,
                         size_t objsize, capaddr_t dest_cnode_cptr,
                         uint8_t dest_level, cslot_t dest_slot)
{
    errval_t err;
    uint8_t size = 0;
    genpaddr_t base = 0;

    /* Paramter checking */
    if (type == ObjType_Null || type >= ObjType_Num) {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    /* Destination CNode */
    struct capability *dest_cnode_cap;
    err = caps_lookup_cap(root, dest_cnode_cptr, dest_level,
                          &dest_cnode_cap, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_CNODE_LOOKUP));
    }

    /* Destination slot */
    struct cte *dest_cte;
    dest_cte = caps_locate_slot(get_address(dest_cnode_cap), dest_slot);
    if (dest_cte->cap.type != ObjType_Null) {
        return SYSRET(SYS_ERR_SLOTS_IN_USE);
    }

    /* List capabilities allowed to be created at runtime. */
    switch(type) {

    case ObjType_ID:
        break;

    // only certain types of capabilities can be created at runtime
    default:
        return SYSRET(SYS_ERR_TYPE_NOT_CREATABLE);
    }

    return SYSRET(caps_create_new(type, base, size, objsize, my_core_id, dest_cte));
}

/**
 * Common code for copying and minting except the mint flag and param passing
 *
 * \param root              Source cspace root cnode
 * \param dest_cspace_cptr  Destination cspace root cnode cptr in source cspace
 * \parma destcn_cptr       Destination cnode cptr relative to destination cspace
 * \param dest_slot         Destination slot
 * \param source_cptr       Source capability cptr relative to source cspace
 * \param destcn_level      Level/depth of destination cnode
 * \param source_level      Level/depth of source cap
 * \param param1            First parameter for mint
 * \param param2            Second parameter for mint
 * \param mint              Call is a minting operation
 */
struct sysret
sys_copy_or_mint(struct capability *root, capaddr_t dest_cspace_cptr,
                 capaddr_t destcn_cptr, cslot_t dest_slot, capaddr_t
                 source_croot_ptr, capaddr_t source_cptr,
                 uint8_t destcn_level, uint8_t source_level,
                 uintptr_t param1, uintptr_t param2, bool mint)
{
    errval_t err;

    if (!mint) {
        param1 = param2 = 0;
    }

    if (root->type != ObjType_L1CNode) {
        debug(SUBSYS_CAPS, "%s: root->type = %d\n", __FUNCTION__, root->type);
        return SYSRET(SYS_ERR_CNODE_NOT_ROOT);
    }
    assert(root->type == ObjType_L1CNode);

    /* Lookup source cspace in our cspace */
    struct capability *src_croot;
    err = caps_lookup_cap(root, source_croot_ptr, 2, &src_croot,
                          CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_SOURCE_ROOTCN_LOOKUP));
    }
    if (src_croot->type != ObjType_L1CNode) {
        debug(SUBSYS_CAPS, "%s: src rootcn type = %d\n", __FUNCTION__, src_croot->type);
        return SYSRET(SYS_ERR_CNODE_NOT_ROOT);
    }
    /* Lookup source cap in source cspace */
    struct cte *src_cap;
    err = caps_lookup_slot(src_croot, source_cptr, source_level, &src_cap,
                           CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_SOURCE_CAP_LOOKUP));
    }

    /* Destination cspace root cnode in source cspace */
    struct capability *dest_cspace_root;
    // XXX: level from where?
    err = caps_lookup_cap(root, dest_cspace_cptr, 2, &dest_cspace_root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_ROOTCN_LOOKUP));
    }
    /* dest_cspace_root must be L1 CNode */
    if (dest_cspace_root->type != ObjType_L1CNode) {
        debug(SUBSYS_CAPS, "%s: dest rootcn type = %d\n", __FUNCTION__, src_croot->type);
        return SYSRET(SYS_ERR_CNODE_TYPE);
    }

    /* Destination cnode in destination cspace */
    struct cte *dest_cnode_cap;
    err = caps_lookup_slot(dest_cspace_root, destcn_cptr, destcn_level,
                           &dest_cnode_cap, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_CNODE_LOOKUP));
    }

    /* Perform copy */
    if (dest_cnode_cap->cap.type == ObjType_L1CNode ||
        dest_cnode_cap->cap.type == ObjType_L2CNode)
    {
        return SYSRET(caps_copy_to_cnode(dest_cnode_cap, dest_slot, src_cap,
                                         mint, param1, param2));
    } else {
        return SYSRET(SYS_ERR_DEST_TYPE_INVALID);
    }
}

struct sysret
sys_map(struct capability *ptable, cslot_t slot, capaddr_t source_root_cptr,
        capaddr_t source_cptr, uint8_t source_level, uintptr_t flags,
        uintptr_t offset, uintptr_t pte_count, capaddr_t mapping_crootptr,
        capaddr_t mapping_cnptr, uint8_t mapping_cn_level, cslot_t mapping_slot)
{
    assert (type_is_vnode(ptable->type));

    errval_t err;

    /* XXX: TODO: make root explicit argument for sys_map() */
    struct capability *root = &dcb_current->cspace.cap;

    if (!(ptable->rights & CAPRIGHTS_WRITE)) {
        return SYSRET(SYS_ERR_DEST_CAP_RIGHTS);
    }

    /* Lookup source root cn cap in own cspace */
    struct capability *src_root;
    err = caps_lookup_cap(root, source_root_cptr, source_level, &src_root,
                          CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_SOURCE_ROOTCN_LOOKUP));
    }
    if (src_root->type != ObjType_L1CNode) {
        return SYSRET(SYS_ERR_CNODE_NOT_ROOT);
    }
    /* Lookup source cap in source cspace */
    struct cte *src_cte;
    err = caps_lookup_slot(src_root, source_cptr, source_level, &src_cte,
                           CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_SOURCE_CAP_LOOKUP));
    }

    /* Lookup mapping cspace root in our cspace */
    struct capability *mapping_croot;
    err = caps_lookup_cap(root, mapping_crootptr, 2, &mapping_croot,
                          CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_ROOTCN_LOOKUP));
    }

    /* Lookup mapping slot in dest cspace */
    struct cte *mapping_cnode_cte;
    err = caps_lookup_slot(mapping_croot, mapping_cnptr, mapping_cn_level,
                           &mapping_cnode_cte, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_CNODE_LOOKUP));
    }

    if (mapping_cnode_cte->cap.type != ObjType_L2CNode) {
        return SYSRET(SYS_ERR_DEST_TYPE_INVALID);
    }

    struct cte *mapping_cte = caps_locate_slot(get_address(&mapping_cnode_cte->cap),
                                               mapping_slot);
    if (mapping_cte->cap.type != ObjType_Null) {
        return SYSRET(SYS_ERR_SLOT_IN_USE);
    }

    /* Perform map */
    // XXX: this does not check if we do have CAPRIGHTS_READ_WRITE on
    // the destination cap (the page table we're inserting into)
    return SYSRET(caps_copy_to_vnode(cte_for_cap(ptable), slot, src_cte, flags,
                                     offset, pte_count, mapping_cte));
}

struct sysret
sys_copy_remap(struct capability *ptable, cslot_t slot, capaddr_t source_cptr,
               int source_level, uintptr_t flags, uintptr_t offset,
               uintptr_t pte_count, capaddr_t mapping_cnptr,
               uint8_t mapping_cn_level, cslot_t mapping_slot)
{
    assert (type_is_vnode(ptable->type));

    errval_t err;

    /* Lookup source cap */
    struct capability *root = &dcb_current->cspace.cap;
    struct cte *src_cte;
    err = caps_lookup_slot(root, source_cptr, source_level, &src_cte,
                           CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_SOURCE_CAP_LOOKUP));
    }

    /* Lookup slot for mapping in our cspace */
    struct cte *mapping_cnode_cte;
    err = caps_lookup_slot(root, mapping_cnptr, mapping_cn_level,
                           &mapping_cnode_cte, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_CNODE_LOOKUP));
    }

    if (mapping_cnode_cte->cap.type != ObjType_L2CNode) {
        return SYSRET(SYS_ERR_DEST_TYPE_INVALID);
    }

    struct cte *mapping_cte = caps_locate_slot(get_address(&mapping_cnode_cte->cap),
                                               mapping_slot);
    if (mapping_cte->cap.type != ObjType_Null) {
        return SYSRET(SYS_ERR_SLOT_IN_USE);
    }

    /* Perform map */
    // XXX: this does not check if we do have CAPRIGHTS_READ_WRITE on
    // the destination cap (the page table we're inserting into)
    return SYSRET(paging_copy_remap(cte_for_cap(ptable), slot, src_cte, flags,
                                    offset, pte_count, mapping_cte));
}

struct sysret sys_delete(struct capability *root, capaddr_t cptr, uint8_t level)
{
    errval_t err;
    struct cte *slot;
    err = caps_lookup_slot(root, cptr, level, &slot, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    err = caps_delete(slot);
    return SYSRET(err);
}

struct sysret sys_revoke(struct capability *root, capaddr_t cptr, uint8_t level)
{
    errval_t err;
    struct cte *slot;
    err = caps_lookup_slot(root, cptr, level, &slot, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    err = caps_revoke(slot);
    return SYSRET(err);
}

struct sysret sys_get_state(struct capability *root, capaddr_t cptr, uint8_t level)
{
    errval_t err;
    struct cte *slot;
    err = caps_lookup_slot(root, cptr, level, &slot, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    distcap_state_t state = distcap_get_state(slot);
    return (struct sysret) { .error = SYS_ERR_OK, .value = state };
}

struct sysret sys_get_size_l1cnode(struct capability *root)
{
    assert(root->type == ObjType_L1CNode);

    return (struct sysret) { .error = SYS_ERR_OK,
        .value = root->u.l1cnode.allocated_bytes};
}


struct sysret sys_resize_l1cnode(struct capability *root, capaddr_t newroot_cptr,
                                 capaddr_t retcn_cptr, cslot_t retslot)
{
    errval_t err;

    if (root->type != ObjType_L1CNode) {
        return SYSRET(SYS_ERR_RESIZE_NOT_L1);
    }
    assert(root->type == ObjType_L1CNode);

    // Lookup new L1 CNode cap
    struct cte *newroot;
    err = caps_lookup_slot(root, newroot_cptr, 2, &newroot, CAPRIGHTS_ALLRIGHTS);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }
    if (newroot->cap.type != ObjType_L1CNode) {
        return SYSRET(SYS_ERR_INVALID_SOURCE_TYPE);
    }
    // TODO: check size of new CNode

    // Lookup slot for returning RAM of old CNode
    struct capability *retcn;
    err = caps_lookup_cap(root, retcn_cptr, 1, &retcn, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }
    struct cte *ret = caps_locate_slot(get_address(retcn), retslot);
    if (ret->cap.type != ObjType_Null) {
        return SYSRET(SYS_ERR_SLOT_IN_USE);
    }

    // Copy over caps from old root cnode to new root cnode
    cslot_t root_slots = cnode_get_slots(root);
    cslot_t newroot_slots = cnode_get_slots(&newroot->cap);
    for (cslot_t i = 0; i < min(root_slots, newroot_slots); i++) {
        struct cte *src = caps_locate_slot(get_address(root), i);
        if (src->cap.type == ObjType_Null) {
            // skip empty slots in old root cnode
            continue;
        }
        struct cte *dest = caps_locate_slot(get_address(&newroot->cap), i);
        if (dest->cap.type != ObjType_Null) {
            // fail if slot in destination cnode occupied
            return SYSRET(SYS_ERR_SLOT_IN_USE);
        }
        // do proper cap copy
        err = caps_copy_to_cte(dest, src, false, 0, 0);
        if (err_is_fail(err)) {
            return SYSRET(err);
        }
    }

    // Copy old root cnode into ret slot, this way we can delete the copies
    // in the task cnode and the dispatcher that we need to update.
    err = caps_copy_to_cte(ret, cte_for_cap(root), false, 0, 0);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    // Set new root cnode in dispatcher
    err = caps_delete(&dcb_current->cspace);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }
    err = caps_copy_to_cte(&dcb_current->cspace, newroot, false, 0, 0);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    // Set new root cnode in task cnode
    struct cte *taskcn = caps_locate_slot(get_address(&newroot->cap),
                                          ROOTCN_SLOT_TASKCN);
    struct cte *rootcn_cap = caps_locate_slot(get_address(&taskcn->cap),
                                              TASKCN_SLOT_ROOTCN);
    assert(rootcn_cap == cte_for_cap(root));
    err = caps_delete(rootcn_cap);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }
    err = caps_copy_to_cte(rootcn_cap, newroot, false, 0, 0);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    return SYSRET(SYS_ERR_OK);
}

/**
 * \brief return redacted 'struct capability' for given capability
 */
struct sysret sys_identify_cap(struct capability *root, capaddr_t cptr,
                               uint8_t level, struct capability *out)
{
    errval_t err;
    if (!access_ok(ACCESS_WRITE, (lvaddr_t)out, sizeof(*out))) {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }

    if (root->type != ObjType_L1CNode) {
        return SYSRET(SYS_ERR_CNODE_NOT_ROOT);
    }

    struct capability *thecap;
    // XXX: what's the correct caprights here?
    err = caps_lookup_cap(root, cptr, level, &thecap, CAPRIGHTS_ALLRIGHTS);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    memcpy(out, thecap, sizeof(*out));

    redact_capability(out);

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_yield(capaddr_t target)
{
    dispatcher_handle_t handle = dcb_current->disp;
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);


    debug(SUBSYS_DISPATCH, "%.*s yields%s\n", DISP_NAME_LEN, disp->name,
          !disp->haswork && disp->lmp_delivered == disp->lmp_seen
           ? " and is removed from the runq" : "");

    if (dcb_current->disabled == false) {
        printk(LOG_ERR, "SYSCALL_YIELD while enabled\n");
        dump_dispatcher(disp);
        return SYSRET(SYS_ERR_CALLER_ENABLED);
    }

    struct capability *yield_to = NULL;
    if (target != CPTR_NULL) {
        errval_t err;

        /* directed yield */
        err = caps_lookup_cap(&dcb_current->cspace.cap, target, 2,
                              &yield_to, CAPRIGHTS_READ);
        if (err_is_fail(err)) {
            return SYSRET(err);
        } else if (yield_to == NULL ||
                   (yield_to->type != ObjType_EndPointLMP
                    && yield_to->type != ObjType_Dispatcher)) {
            return SYSRET(SYS_ERR_INVALID_YIELD_TARGET);
        }
        /* FIXME: check rights? */
    }

    // Since we've done a yield, we explicitly ensure that the
    // dispatcher is upcalled the next time (on the understanding that
    // this is what the dispatcher wants), otherwise why call yield?
    dcb_current->disabled = false;

    // Remove from queue when no work and no more messages and no missed wakeup
    systime_t wakeup = disp->wakeup;
    if (!disp->haswork && disp->lmp_delivered == disp->lmp_seen
        && (wakeup == 0 || wakeup > (systime_now() + kcb_current->kernel_off))) {

        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_SCHED_REMOVE,
            (uint32_t)(lvaddr_t)dcb_current & 0xFFFFFFFF);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_SCHED_REMOVE,
                151);

        scheduler_remove(dcb_current);
        if (wakeup != 0) {
            wakeup_set(dcb_current, wakeup);
        }
    } else {
        // Otherwise yield for the timeslice
        scheduler_yield(dcb_current);
    }

    if (yield_to != NULL) {
        struct dcb *target_dcb = NULL;
        if (yield_to->type == ObjType_EndPointLMP) {
            target_dcb = yield_to->u.endpointlmp.listener;
        } else if (yield_to->type == ObjType_Dispatcher) {
            target_dcb = yield_to->u.dispatcher.dcb;
        } else {
            panic("invalid type in yield cap");
        }

        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_YIELD,
            (uint32_t)(lvaddr_t)target_dcb & 0xFFFFFFFF);
        make_runnable(target_dcb);
        dispatch(target_dcb);
    } else {
//        trace_event(TRACE_SUBSYS_BNET, TRACE_EVENT_BNET_YIELD,
//            0);

        /* undirected yield */
        dispatch(schedule());
    }

    panic("Yield returned!");
}

struct sysret sys_suspend(bool do_halt)
{
    dispatcher_handle_t handle = dcb_current->disp;
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);

    debug(SUBSYS_DISPATCH, "%.*s suspends (halt: %d)\n", DISP_NAME_LEN, disp->name, do_halt);

    if (dcb_current->disabled == false) {
        printk(LOG_ERR, "SYSCALL_SUSPEND while enabled\n");
        return SYSRET(SYS_ERR_CALLER_ENABLED);
    }

    dcb_current->disabled = false;

    if (do_halt) {
        //printf("%s:%s:%d: before halt of core (%"PRIuCOREID")\n",
        //       __FILE__, __FUNCTION__, __LINE__, my_core_id);
        halt();
    } else {
        // Note this only works if we're calling this inside
        // the kcb we're currently running
        printk(LOG_NOTE, "in sys_suspend(<no_halt>)!\n");
        printk(LOG_NOTE, "calling switch_kcb!\n");
        struct kcb *next = kcb_current->next;
        kcb_current->next = NULL;
        switch_kcb(next);
        // enable kcb scheduler
        printk(LOG_NOTE, "enabling kcb scheduler!\n");
        kcb_sched_suspended = false;
        // schedule something in the other kcb
        dispatch(schedule());
    }

    panic("Yield returned!");
}


/**
 * The format of the returned ID is:
 *
 * --------------------------------------------------------------------
 * |             0 (unused) | coreid |         core_local_id          |
 * --------------------------------------------------------------------
 * 63                        39       31                              0 Bit
 *
 */
struct sysret sys_idcap_identify(struct capability *cap, idcap_id_t *id)
{
    STATIC_ASSERT_SIZEOF(coreid_t, 1);

    idcap_id_t coreid = (idcap_id_t) cap->u.id.coreid;
    *id = coreid << 32 | cap->u.id.core_local_id;

    return SYSRET(SYS_ERR_OK);
}

/**
 * Calls correct handler function to spawn an app core.
 *
 * At the moment spawn_core_handlers is set-up per
 * architecture inside text_init() usually found in init.c.
 *
 * \note Generally the x86 terms of BSP and APP core are used
 * throughout Barrelfish to distinguish between bootstrap core (BSP)
 * and application cores (APP).
 *
 * \param  core_id  Identifier of the core which we want to boot
 * \param  cpu_type Architecture of the core.
 * \param  entry    Entry point for code to start execution.
 *
 * \retval SYS_ERR_OK Core successfully booted.
 * \retval SYS_ERR_ARCHITECTURE_NOT_SUPPORTED No handler registered for
 *     the specified cpu_type.
 * \retval SYS_ERR_CORE_NOT_FOUND Core failed to boot.
 */

struct sysret sys_monitor_spawn_core(hwid_t target, enum cpu_type cpu_type,
                                     genvaddr_t entry, genpaddr_t context)
{
    errval_t err;

    assert(cpu_type < CPU_TYPE_NUM);
    // TODO(gz): assert core_id valid
    // TODO(gz): assert entry range?

    if (cpu_type >= CPU_TYPE_NUM) {
        return SYSRET(SYS_ERR_ARCHITECTURE_NOT_SUPPORTED);
    }

    coreboot_start_fn_t start_fn = coreboot_get_spawn_handler(cpu_type);

    if (start_fn == NULL) {
        return SYSRET(SYS_ERR_ARCHITECTURE_NOT_SUPPORTED);
    }

    err = start_fn(target, entry, context);
    if(err_is_fail(err)) {
        err = err_push(err, SYS_ERR_CORE_NOT_FOUND);
    }
    return SYSRET(err);
}

struct sysret sys_kernel_add_kcb(struct kcb *new_kcb)
{
    kcb_add(new_kcb);

    // update kernel_now offset
    new_kcb->kernel_off -= systime_now();
    // reset scheduler statistics
    scheduler_reset_time();
    // update current core id of all domains
    kcb_update_core_id(new_kcb);
    // upcall domains with registered interrupts to tell them to re-register
    irq_table_notify_domains(new_kcb);

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_kernel_remove_kcb(struct kcb * to_remove)
{
    return SYSRET(kcb_remove(to_remove));
}

struct sysret sys_kernel_suspend_kcb_sched(bool suspend)
{
    printk(LOG_NOTE, "in kernel_suspend_kcb_sched invocation!\n");
    kcb_sched_suspended = suspend;
    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_handle_kcb_identify(struct capability* to, struct frame_identity *fi)
{
    // Return with physical base address of frame
    // XXX: pack size into bottom bits of base address
    assert(to->type == ObjType_KernelControlBlock);
    lvaddr_t vkcb = (lvaddr_t) to->u.kernelcontrolblock.kcb;
    assert((vkcb & BASE_PAGE_MASK) == 0);

    if (!access_ok(ACCESS_WRITE, (lvaddr_t)fi, sizeof(struct frame_identity))) {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }

    fi->base = get_address(to);
    fi->bytes = get_size(to);

    return SYSRET(SYS_ERR_OK);
}
