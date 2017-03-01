/**
 * \file
 * \brief Capability system user code
 */

/*
 * Copyright (c) 2007-2010, 2012, 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdint.h>
#include <stdbool.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/cspace.h>
#include <barrelfish/caddr.h>
#include <barrelfish/lmp_endpoints.h>
#include <if/monitor_defs.h>
#include <if/monitor_blocking_defs.h>
#include <barrelfish/monitor_client.h>
#include <trace/trace.h>
#include <stdio.h>

/// Root CNode
#define ROOT_CNODE_INIT { \
    .croot = CPTR_ROOTCN, \
    .cnode = 0, \
    .level = CNODE_TYPE_ROOT, }

struct cnoderef cnode_root = ROOT_CNODE_INIT;

#define TASK_CNODE_INIT { \
    .croot = CPTR_ROOTCN, \
    .cnode = CPTR_TASKCN_BASE, \
    .level = CNODE_TYPE_OTHER, }

#define PAGE_CNODE_INIT { \
    .croot = CPTR_ROOTCN, \
    .cnode = CPTR_PAGECN_BASE, \
    .level = CNODE_TYPE_OTHER, }

/// Task CNode
struct cnoderef cnode_task = TASK_CNODE_INIT;

/// Base CNode
struct cnoderef cnode_base = {
    .cnode = CPTR_BASE_PAGE_CN_BASE,
    .level = CNODE_TYPE_OTHER,
    .croot = CPTR_ROOTCN,
};

/// Super CNode
struct cnoderef cnode_super = {
    .cnode = CPTR_SUPERCN_BASE,
    .level = CNODE_TYPE_OTHER,
    .croot = CPTR_ROOTCN,
};

/// Page CNode
struct cnoderef cnode_page = PAGE_CNODE_INIT;

/// Module CNode
struct cnoderef cnode_module = {
    .cnode = CPTR_MODULECN_BASE,
    .level = CNODE_TYPE_OTHER,
    .croot = CPTR_ROOTCN,
};

/// Capability to Root CNode
struct capref cap_root = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_ROOTCN
};

/// Capability for IRQ table
struct capref cap_irq = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_IRQ
};

/// Capability for legacy IO
struct capref cap_io = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_IO
};

/// Capability for endpoint to self
struct capref cap_selfep = {
    .cnode = TASK_CNODE_INIT,
    .slot = TASKCN_SLOT_SELFEP
};

/// Capability for dispatcher
struct capref cap_dispatcher = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_DISPATCHER
};

/// Capability for dispatcher
struct capref cap_dispframe = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_DISPFRAME
};

/// Capability for ArgSpace
struct capref cap_argcn = {
    .cnode = ROOT_CNODE_INIT,
    .slot  = ROOTCN_SLOT_ARGCN
};

/// Capability for monitor endpoint
struct capref cap_monitorep = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_MONITOREP
};

/// Capability for kernel (only in monitor)
struct capref cap_kernel = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_KERNELCAP
};

/// Capability for IPI sending (only in monitor)
struct capref cap_ipi = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_IPI
};

/// PerfMon CNode
struct capref cap_perfmon = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_PERF_MON
};

/// Capability for endpoint to init (only in monitor/mem_serv)
struct capref cap_initep = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_INITEP
};

/// Session ID
struct capref cap_sessionid = {
    .cnode = TASK_CNODE_INIT,
    .slot = TASKCN_SLOT_SESSIONID
};

/// Root PML4 VNode
struct capref cap_vroot = {
    .cnode = PAGE_CNODE_INIT,
    .slot = PAGECN_SLOT_VROOT,
};

static inline bool backoff(int count)
{
    // very crude exponential backoff based upon core id
    int yieldcnt = 2^count * disp_get_core_id();
    for (int i=0; i<yieldcnt; i++) {
        thread_yield();
    }
    return true;
}

/**
 * \brief Retype a capability into one or more new capabilities, going through
 * the monitor to ensure consistancy with other cores.  Only necessary for
 * caps that have been sent remotely.
 */
static errval_t cap_retype_remote(struct capref src_root, struct capref dest_root,
                                  capaddr_t src, gensize_t offset, enum objtype new_type,
                                  gensize_t objsize, size_t count, capaddr_t to,
                                  capaddr_t slot, int to_level)
{
    struct monitor_blocking_binding *mrc = get_monitor_blocking_binding();
    if (!mrc) {
        return LIB_ERR_MONITOR_RPC_NULL;
    }

    errval_t err, remote_cap_err;
    int send_count = 0;
    do {
        err = mrc->rpc_tx_vtbl.remote_cap_retype(mrc, src_root, dest_root, src,
                                          offset, (uint64_t)new_type, objsize,
                                          count, to, slot, to_level, &remote_cap_err);
        if (err_is_fail(err)){
            DEBUG_ERR(err, "remote cap retype\n");
        }
    } while (err_no(remote_cap_err) == MON_ERR_REMOTE_CAP_RETRY && backoff(++send_count));

    return remote_cap_err;

}


/**
 * \brief Delete the given capability, going through  the monitor to ensure
 * consistancy with other cores.  Only necessary for caps that have been sent
 * remotely.
 *
 * \param cap Capability to be deleted
 *
 * Deletes (but does not revoke) the given capability, allowing the CNode slot
 * to be reused.
 */
static errval_t cap_delete_remote(struct capref root, capaddr_t src, uint8_t level)
{
    struct monitor_blocking_binding *mrc = get_monitor_blocking_binding();
    if (!mrc) {
        return LIB_ERR_MONITOR_RPC_NULL;
    }

    errval_t err, remote_cap_err;
    int count = 0;
    do {
        err = mrc->rpc_tx_vtbl.remote_cap_delete(mrc, root, src, level,
                                          &remote_cap_err);
        if (err_is_fail(err)){
            DEBUG_ERR(err, "remote cap delete\n");
        }
    } while (err_no(remote_cap_err) == MON_ERR_REMOTE_CAP_RETRY && backoff(++count));

    return remote_cap_err;
}

/**
 * \brief Revoke (delete all copies and descendants of) the given capability,
 * going through the monitor to ensure consistancy with other cores.  Only
 * necessary for caps that have been sent remotely.
 *
 * \param cap Capability to be revoked
 *
 * Deletes all copies and descendants of the given capability, but not the
 * capability itself. If this succeeds, the capability is guaranteed to be
 * the only copy in the system.
 */
static errval_t cap_revoke_remote(struct capref root, capaddr_t src, uint8_t level)
{
    struct monitor_blocking_binding *mrc = get_monitor_blocking_binding();
    if (!mrc) {
        return LIB_ERR_MONITOR_RPC_NULL;
    }

    errval_t err, remote_cap_err;
    int count = 0;
    do {
        err = mrc->rpc_tx_vtbl.remote_cap_revoke(mrc, root, src, level,
                                          &remote_cap_err);
        if (err_is_fail(err)){
            DEBUG_ERR(err, "remote cap delete\n");
        }
    } while (err_no(remote_cap_err) == MON_ERR_REMOTE_CAP_RETRY && backoff(++count));

    return remote_cap_err;
}

/**
 * \brief Retype (part of) a capability into one or more new capabilities
 *
 * \param dest_start    Location of first destination slot, which must be empty
 * \param src           Source capability to retype
 * \param offset        Offset into source capability
 * \param new_type      Kernel object type to retype to.
 * \param objsize       Size of created objects in bytes
 *                      (ignored for fixed-size objects)
 * \param count         The number of new objects to create
 *
 * When retyping IRQSrc capabilities, offset and objsize represent the start
 * and end of the to be created interrupt range. Count must be 1 for IRQSrc.
 *
 * Retypes (part of) the given source capability into a number of new
 * capabilities, which may be of the same or of different type. The new
 * capabilities are created in the slots starting from dest_start, which must
 * all be empty and lie in the same CNode. The number of objects created is
 * determined by the argument `count`.
 */
errval_t cap_retype(struct capref dest_start, struct capref src, gensize_t offset,
                    enum objtype new_type, gensize_t objsize, size_t count)
{
    errval_t err;

    // Address of destination cspace
    capaddr_t dcs_addr = get_croot_addr(dest_start);
    // Address of the cap to the destination CNode
    capaddr_t dcn_addr = get_cnode_addr(dest_start);
    // Depth/Level of destination cnode
    enum cnode_type dcn_level = get_cnode_level(dest_start);
    // Address of source cspace
    capaddr_t scp_root = get_croot_addr(src);
    // Address of source capability
    capaddr_t scp_addr = get_cap_addr(src);

    err = invoke_cnode_retype(cap_root, scp_root, scp_addr, offset, new_type,
                              objsize, count, dcs_addr, dcn_addr, dcn_level,
                              dest_start.slot);

    if (err_no(err) == SYS_ERR_RETRY_THROUGH_MONITOR) {
        struct capref src_root = get_croot_capref(src);
        struct capref dest_root = get_croot_capref(dest_start);
        return cap_retype_remote(src_root, dest_root, scp_addr, offset, new_type,
                                 objsize, count, dcn_addr, dest_start.slot,
                                 dcn_level);
    } else {
        return err;
    }
}


/**
 * \brief Create a capability
 *
 * \param dest      Location where to create the cap, which must be empty.
 * \param type      Kernel object type to create.
 * \param size      Size of the created capability in bytes.
 *                  (ignored for fixed-size objects)
 *
 * Only certain types of capabilities can be created this way. If invoked on
 * a capability type, that is not creatable at runtime the error
 * SYS_ERR_TYPE_NOT_CREATABLE is returned. Most capabilities have to be retyped
 * from other capabilities with cap_retype().
 */
errval_t cap_create(struct capref dest, enum objtype type, size_t size)
{
    errval_t err;

    // Address of the cap to the destination CNode
    capaddr_t dest_cnode_cptr = get_cnode_addr(dest);
    enum cnode_type dest_cnode_level = get_cnode_level(dest);

    err = invoke_cnode_create(cap_root, type, size, dest_cnode_cptr,
                              dest_cnode_level, dest.slot);

    return err;
}

/**
 * \brief Delete the given capability
 *
 * \param cap Capability to be deleted
 *
 * Deletes (but does not revoke) the given capability, allowing the CNode slot
 * to be reused.
 */
errval_t cap_delete(struct capref cap)
{
    errval_t err;
    struct capref croot = get_croot_capref(cap);
    capaddr_t caddr = get_cap_addr(cap);
    enum cnode_type level = get_cap_level(cap);

    err = invoke_cnode_delete(croot, caddr, level);

    if (err_no(err) == SYS_ERR_RETRY_THROUGH_MONITOR) {
        return cap_delete_remote(croot, caddr, level);
    } else {
        return err;
    }
}

/**
 * \brief Revoke (delete all copies and descendants of) the given capability
 *
 * \param cap Capability to be revoked
 *
 * Deletes all copies and descendants of the given capability, but not the
 * capability itself. If this succeeds, the capability is guaranteed to be
 * the only copy in the system.
 */
errval_t cap_revoke(struct capref cap)
{
    errval_t err;
    struct capref croot = get_croot_capref(cap);
    capaddr_t caddr = get_cap_addr(cap);
    enum cnode_type level = get_cap_level(cap);

    err = invoke_cnode_revoke(croot, caddr, level);

    if (err_no(err) == SYS_ERR_RETRY_THROUGH_MONITOR) {
        return cap_revoke_remote(croot, caddr, level);
    } else {
        return err;
    }
}

/**
 * \brief Destroy a capability, i.e. delete it and free the slot.
 *
 * \param cap           Capability to be destroyed
 */
errval_t cap_destroy(struct capref cap)
{
    errval_t err;
    err = cap_delete(cap);
    if (err_is_fail(err)) {
        return err;
    }

    err = slot_free(cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_WHILE_FREEING_SLOT);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Replace own L1 CNode
 *
 * \param new the replacement L1 CNode
 * \param ret the slot to put the old L1 CNode
 */
errval_t root_cnode_resize(struct capref new, struct capref ret)
{
    assert(get_croot_addr(new) == CPTR_ROOTCN);
    assert(get_cap_level(new) == CNODE_TYPE_COUNT);
    capaddr_t new_cptr = get_cap_addr(new);

    assert(get_croot_addr(ret) == CPTR_ROOTCN);
    assert(get_cap_level(ret) == CNODE_TYPE_COUNT);
    capaddr_t retcn_ptr= get_cnode_addr(ret);

    return invoke_cnode_resize(cap_root, new_cptr, retcn_ptr, ret.slot);
}

/**
 * \brief Create a CNode from a given RAM capability in a specific slot
 *
 * \param dest location in which to place newly-created CNode cap
 * \param src  location of RAM capability to be retyped to new CNode
 * \param cnoderef cnoderef struct, filled-in if non-NULL with relevant info
 * \param slots number of slots in created CNode
 *                  must match size of RAM capability.
 *
 * This function requires that dest refer to an existing but empty slot. It
 * retypes the given memory to a new CNode.
 */
errval_t cnode_create_from_mem(struct capref dest, struct capref src,
                               enum objtype cntype, struct cnoderef *cnoderef,
                               size_t slots)
{
    errval_t err;

    if (cntype != ObjType_L1CNode &&
        cntype != ObjType_L2CNode)
    {
        return LIB_ERR_CNODE_TYPE;
    }


    // Retype it to the destination
    err = cap_retype(dest, src, 0, cntype, slots * (1UL << OBJBITS_CTE), 1);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_RETYPE);
    }

    // Construct the cnoderef to return
    if (cnoderef != NULL) {
        enum cnode_type ref_cntype = cntype == ObjType_L1CNode ? CNODE_TYPE_ROOT : CNODE_TYPE_OTHER;
        *cnoderef = build_cnoderef(dest, ref_cntype);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Create a CNode from newly-allocated RAM in a newly-allocated slot
 *
 * \param ret_dest capref struct to be filled-in with location of CNode
 * \param cnoderef cnoderef struct, filled-in if non-NULL with relevant info
 * \param slots Minimum number of slots in created CNode
 * \param retslots If non-NULL, filled in with the  number of slots in created CNode
 */
errval_t cnode_create(struct capref *ret_dest, struct cnoderef *cnoderef,
                      cslot_t slots, cslot_t *retslots)
{
    USER_PANIC("cnode_create deprecated; use cnode_create_l1, cnode_create_l2, or cnode_create_foreign_l2: %p %p %p %p\n",
            __builtin_return_address(0),
#ifdef __x86_64__
            __builtin_return_address(1),
            __builtin_return_address(2),
            __builtin_return_address(3)
#else
            NULL, NULL, NULL
#endif
            );
    return LIB_ERR_NOT_IMPLEMENTED;
}

/**
 * \brief Create a L2 CNode from newly-allocated RAM in a newly-allocated slot
 *
 * \param ret_dest capref struct to be filled-in with location of CNode
 * \param cnoderef cnoderef struct, filled-in if non-NULL with relevant info
 *
 * This function always creates a L2 CNode which contains 256 capabilities
 */
errval_t cnode_create_l2(struct capref *ret_dest, struct cnoderef *cnoderef)
{
    errval_t err;

    // Allocate a slot in root cn for destination
    assert(ret_dest != NULL);
    err = slot_alloc_root(ret_dest);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    cslot_t retslots;
    err = cnode_create_raw(*ret_dest, cnoderef, ObjType_L2CNode,
                           L2_CNODE_SLOTS, &retslots);
    if (retslots != L2_CNODE_SLOTS) {
        debug_printf("Unable to create properly sized L2 CNode: got %"PRIuCSLOT" slots instead of %"PRIuCSLOT"\n",
                retslots, (cslot_t)L2_CNODE_SLOTS);
    }
    return err;
}

errval_t cnode_create_l1(struct capref *ret_dest, struct cnoderef *cnoderef)
{
    errval_t err;

    // Allocate a slot in root cn for destination
    assert(ret_dest != NULL);
    err = slot_alloc(ret_dest);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    cslot_t retslots;
    err = cnode_create_raw(*ret_dest, cnoderef, ObjType_L1CNode,
                           L2_CNODE_SLOTS, &retslots);
    if (retslots != L2_CNODE_SLOTS) {
        debug_printf("Unable to create initial L1 CNode: got %"PRIuCSLOT" slots instead of %"PRIuCSLOT"\n",
                     retslots, (cslot_t)L2_CNODE_SLOTS);
    }
    return err;
}

/**
 * \brief Create a CNode for another cspace from newly-allocated RAM in a
 *        newly-allocated slot
 *
 * \param dest_l1   capref to L1 (root) cnode of destination cspace
 * \param dest_slot slot to fill with new cnode in destination L1 cnode
 * \param cnoderef  cnoderef struct, filled-in if non-NULL with relevant info
 *
 * This function creates a CNode which contains 256 capabilities initially
 * and puts it in a slot in our cspace.
 */
errval_t cnode_create_foreign_l2(struct capref dest_l1, cslot_t dest_slot,
                                 struct cnoderef *cnoderef)
{
    errval_t err;

    if (capref_is_null(dest_l1)) {
        return LIB_ERR_CROOT_NULL;
    }
    assert(!capref_is_null(dest_l1));

    struct capref dest;
    dest.cnode = build_cnoderef(dest_l1, CNODE_TYPE_ROOT);
    dest.slot = dest_slot;

    cslot_t retslots;
    err = cnode_create_raw(dest, NULL, ObjType_L2CNode, L2_CNODE_SLOTS, &retslots);
    if (retslots != L2_CNODE_SLOTS) {
        debug_printf("Unable to create properly sized foreign CNode: "
                     "got %"PRIuCSLOT" slots instead of %"PRIuCSLOT"\n",
                     retslots, (cslot_t)L2_CNODE_SLOTS);
    }

    // Create proper cnoderef for foreign L2
    if (cnoderef) {
        cnoderef->croot = get_cap_addr(dest_l1);
        cnoderef->cnode = ROOTCN_SLOT_ADDR(dest_slot);
        cnoderef->level = CNODE_TYPE_OTHER;
    }
    return err;
}

/**
 * \brief Create a CNode from newly-allocated RAM in the given slot
 *
 * \param dest location in which to place CNode cap
 * \param cnoderef cnoderef struct, filled-in if non-NULL with relevant info
 * \param cntype, type of new cnode
 * \param slots Minimum number of slots in created CNode
 * \param retslots If non-NULL, filled in with the  number of slots in created CNode
 *
 * This function requires that dest refer to an existing but empty slot. It
 * allocates memory (using #ram_alloc), and retypes that memory to a new CNode.
 * The intermediate ram cap is destroyed.
 */
errval_t cnode_create_raw(struct capref dest, struct cnoderef *cnoderef,
                          enum objtype cntype, cslot_t slots, cslot_t *retslots)
{
    errval_t err;
    struct capref ram;

    assert(slots > 0);

    if (cntype != ObjType_L1CNode &&
        cntype != ObjType_L2CNode)
    {
        return LIB_ERR_CNODE_TYPE;
    }

    if (slots < L2_CNODE_SLOTS ||
        (cntype == ObjType_L2CNode && slots != L2_CNODE_SLOTS))
    {
        return LIB_ERR_CNODE_SLOTS;
    }

    if (retslots != NULL) {
        *retslots = slots;
    }

    // XXX: mem_serv should serve non-power-of-two requests
    uint8_t bits = log2ceil(slots);
    assert(slots >= (1UL << bits));

    // Allocate some memory
    err = ram_alloc(&ram, bits + OBJBITS_CTE);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC);
    }

    err = cnode_create_from_mem(dest, ram, cntype, cnoderef, slots);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE_FROM_MEM);
    }

    err = cap_destroy(ram);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_DESTROY);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Create CNode with a given guard
 *
 * \param dest         Location where to place the cnode
 * \param cnoderef   Filled in cnoderef struct if non-NULL
 * \param slots Minimum number of slots in created CNode
 * \param retslots If non-NULL, filled in with the  number of slots in created CNode
 * \param guard        The guard value to set
 * \param guard_size   The length of the guard in bits
 *
 * This function requires that dest refer to an existing but empty slot. It
 * allocates memory (using #ram_alloc), and retypes that memory to a new CNode
 * with the given guard value and size. An intermediate slot is used in order to
 * set the guard value.
 */
errval_t cnode_create_with_guard(struct capref dest, struct cnoderef *cnoderef,
                                 cslot_t slots, cslot_t *retslots,
                                 uint64_t guard, uint8_t guard_size)
{
    USER_PANIC("%s: GPT CNodes are deprecated\n", __FUNCTION__);
}

/**
 * \brief Create a VNode in newly-allocated memory
 *
 * \param dest location to place new VNode cap
 * \param type VNode type to create
 *
 * This function requires that dest refer to an existing but empty slot.
 * The intermidiate ram cap is destroyed.
 */
errval_t vnode_create(struct capref dest, enum objtype type)
{
    errval_t err;

    struct capref ram;

    size_t objbits_vnode = vnode_objbits(type);
    err = ram_alloc(&ram, objbits_vnode);
    if (err_no(err) == LIB_ERR_RAM_ALLOC_WRONG_SIZE && type != ObjType_VNode_ARM_l1) {
        // can only get 4kB pages, cannot create ARM_l1, and waste 3kB for
        // ARM_l2
        err = ram_alloc(&ram, BASE_PAGE_BITS);
    }
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC);
    }

    assert(type_is_vnode(type));
    err = cap_retype(dest, ram, 0, type, vnode_objsize(type), 1);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_RETYPE);
    }

    err = cap_destroy(ram);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_DESTROY);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Create a Frame cap referring to newly-allocated RAM in a given slot
 *
 * \param dest  Location to place new frame cap
 * \param bytes Minimum size of frame to create
 * \param retbytes If non-NULL, filled in with size of created frame
 *
 * This function requires that dest refer to an existing but empty slot.
 * #ram_alloc is used to allocate memory. After retyping the intermediate
 * ram cap is destroyed.
 *
 * This function will returns a special error code if ram_alloc fails
 * due to the constrains on the memory server (size of cap or region
 * of memory). This is to facilitate retrying with different
 * constraints.
 */
errval_t frame_create(struct capref dest, size_t bytes, size_t *retbytes)
{
    assert(bytes > 0);
    uint8_t bits = log2ceil(bytes);
    assert((1UL << bits) >= bytes);
    errval_t err;

    if (bits < BASE_PAGE_BITS) {
        bits = BASE_PAGE_BITS;
    }

    struct capref ram;
    err = ram_alloc(&ram, bits);
    if (err_is_fail(err)) {
        if (err_no(err) == MM_ERR_NOT_FOUND ||
            err_no(err) == LIB_ERR_RAM_ALLOC_WRONG_SIZE) {
            return err_push(err, LIB_ERR_RAM_ALLOC_MS_CONSTRAINTS);
        }
        return err_push(err, LIB_ERR_RAM_ALLOC);
    }

    err = cap_retype(dest, ram, 0, ObjType_Frame, (1UL << bits), 1);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_RETYPE);
    }

    err = cap_destroy(ram);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_DESTROY);
    }

    if (retbytes != NULL) {
        *retbytes = 1UL << bits;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Create a Dispatcher in newly-allocated memory
 *
 * \param dest location to place new dispatcher cap
 *
 * This function requires that dest refer to an existing but empty slot. It does
 * not map in nor initialise the Dispatcher.
 * The intermediate ram cap is destroyed.
 */
errval_t dispatcher_create(struct capref dest)
{
    errval_t err;

    struct capref ram;
    assert(1 << log2ceil(OBJSIZE_DISPATCHER) == OBJSIZE_DISPATCHER);
    err = ram_alloc(&ram, log2ceil(OBJSIZE_DISPATCHER));
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC);
    }

    err = cap_retype(dest, ram, 0, ObjType_Dispatcher, 0, 1);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_RETYPE);
    }

    err = cap_destroy(ram);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_DESTROY);
    }
    return SYS_ERR_OK;
}

/**
 * \brief Create endpoint to caller on current dispatcher.
 *
 * \param buflen  Length of incoming LMP buffer, in words
 * \param retcap  Pointer to capref struct, filled-in with location of cap
 * \param retep   Double pointer to LMP endpoint, filled-in with allocated EP
 */
errval_t endpoint_create(size_t buflen, struct capref *retcap,
                         struct lmp_endpoint **retep)
{
    errval_t err = slot_alloc(retcap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    return lmp_endpoint_create_in_slot(buflen, *retcap, retep);
}

/**
 * \brief Create a Frame cap referring to newly-allocated RAM in an allocated slot
 *
 * \param dest  Pointer to capref struct, filled-in with location of new cap
 * \param bytes Minimum size of frame to create
 * \param retbytes If non-NULL, filled in with size of created frame
 */
errval_t frame_alloc(struct capref *dest, size_t bytes, size_t *retbytes)
{
    errval_t err = slot_alloc(dest);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    return frame_create(*dest, bytes, retbytes);
}

/**
 * \brief Create a DevFrame cap by retyping out of given source PhysAddr cap
 *
 * \param dest          Pointer to capref struct, filled-in with location of new cap
 * \param src           Cap_info struct for the source PhysAddr cap
 * \param size_bits     Size of created objects as a power of two
 *                      (ignored for fixed-size objects)
 */
errval_t devframe_type(struct capref *dest, struct capref src, uint8_t bits)
{
    errval_t err = slot_alloc(dest);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    return cap_retype(*dest, src, 0, ObjType_DevFrame, 1UL << bits, 1);
}

/**
 * \brief Create an ID cap in a newly allocated slot.
 *
 * \param dest  Pointer to capref struct, filld-in with location of new cap.
 *
 * The caller is responsible for revoking the cap after using it.
 */
errval_t idcap_alloc(struct capref *dest)
{
    errval_t err = slot_alloc(dest);

    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    return idcap_create(*dest);
}

/**
 * \brief Create an ID cap in the specified slot.
 *
 * \param dest  Capref, where ID cap should be created.
 *
 * The caller is responsible for revoking the cap after using it.
 */
errval_t idcap_create(struct capref dest)
{
    return cap_create(dest, ObjType_ID, 0);
}

/**
 * \brief Builds a #cnoderef struct from a #capref struct using cap
 *        identification.
 *
 * \param cnoder Pointer to a cnoderef struct, fill-in by function.
 * \param capr   Capref to a CNode capability.
 */
errval_t cnode_build_cnoderef(struct cnoderef *cnoder, struct capref capr)
{
    struct capability cap;
    errval_t err = debug_cap_identify(capr, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    if (cap.type != ObjType_L1CNode &&
        cap.type != ObjType_L2CNode) {
        return LIB_ERR_NOT_CNODE;
    }

    if (!cnodecmp(capr.cnode, cnode_root)) {
        USER_PANIC("cnode_build_cnoderef NYI for non rootcn caprefs");
    }

    cnoder->croot = get_croot_addr(capr);
    cnoder->cnode = capr.slot << L2_CNODE_BITS;
    cnoder->level = CNODE_TYPE_OTHER;

    return SYS_ERR_OK;
}
