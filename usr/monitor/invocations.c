/**
 * \file
 * \brief Arch-generic capability invocation wrappers specific to the monitors
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"

bool monitor_can_send_cap(struct capability *cap)
{
    /* Cannot send caps of these types so send error */
    return !((cap->type == ObjType_Null) || (cap->type == ObjType_EndPoint)
        || (cap->type == ObjType_Dispatcher) || (cap->type == ObjType_Kernel)
        || (cap->type == ObjType_IRQTable));
}

/**
 * \brief Invoke the kernel cap to acquire the raw metadata of a cap.
 *
 * \param cap     The cap to identify
 * \param out     Struct to return the metadata
 */
errval_t monitor_cap_identify(struct capref cap, struct capability *out)
{
    // If it's a NULL cap reference, return a fabricated Null cap
    if(capref_is_null(cap)) {
        goto nullcap;
    }

    uint8_t level = get_cap_level(cap);
    capaddr_t caddr = get_cap_addr(cap);
    errval_t err = invoke_monitor_identify_cap(caddr, level, out);
    if (err_no(err) == SYS_ERR_IDENTIFY_LOOKUP &&
        err_no(err>>10) == SYS_ERR_CAP_NOT_FOUND)
    {
        // XXX: is it ok to return a fabricated null cap when doing cap
        // identify on an empty slot? -SG, 2013-07-31
        goto nullcap;
    }
    return err;

nullcap:
    memset(out, 0, sizeof(struct capability));
    out->type = ObjType_Null;
    return SYS_ERR_OK;
}


/**
 * \brief Invoke the kernel cap to acquire the raw metadata of a cap that is 
 * located off another domains root cnode
 *
 * \param croot   The root cnode of the process to which this cap belongs
 * \param cap     The caddr of the cap to identify
 * \param level   CSpace level of the cap to identify
 * \param out     Struct to return the metadata
 */
errval_t monitor_domains_cap_identify(struct capref croot, capaddr_t cap,
                                      int level, struct capability *out)
{
    assert (out != NULL);

    uint8_t rootcap_level = get_cap_level(croot);
    capaddr_t rootcap_addr = get_cap_addr(croot);

    return invoke_monitor_identify_domains_cap(rootcap_addr, rootcap_level,
                                               cap, level, out);
}

/**
 * Let the kernel know that this capability has remote relations, and read the
 * resulting remote relation flags.
 */
errval_t monitor_domcap_remote_relations(struct capref croot, capaddr_t cptr,
                                         int level, uint8_t relations,
                                         uint8_t mask, uint8_t *ret_relations)
{
    uint8_t rootcap_level = get_cap_level(croot);
    capaddr_t rootcap_addr = get_cap_addr(croot);

    return invoke_monitor_remote_relations(rootcap_addr, rootcap_level,
                                           cptr, level, relations, mask,
                                           ret_relations);
}

errval_t monitor_remote_relations(struct capref cap, uint8_t relations,
                                  uint8_t mask, uint8_t *ret_relations)
{
    uint8_t level = get_cap_level(cap);
    capaddr_t cptr = get_cap_addr(cap);
    return monitor_domcap_remote_relations(cap_root, cptr, level, relations,
                                           mask, ret_relations);
}

/**
 *
 */

errval_t monitor_cap_has_relations(struct capref cap, uint8_t mask,
                                   uint8_t *res)
{
    capaddr_t caddr = get_cap_addr(cap);
    uint8_t level = get_cap_level(cap);
    return invoke_monitor_cap_has_relations(caddr, level, mask, res);
}

/**
 * \brief Invoke the kernel cap to set the type of #cap to null
 */
errval_t monitor_nullify_cap(struct capref cap)
{
    capaddr_t caddr = get_cap_addr(cap);
    uint8_t level = get_cap_level(cap);
    return invoke_monitor_nullify_cap(caddr, level);
}

/**
 * \brief Invoke the kernel cap to create a new cap.
 *
 * \param dest     Location to place the new cap in.
 * \param cap      Metadata of the cap to create
 * \param owner    Core that currently owns the cap
 */
errval_t monitor_cap_create(struct capref dest, struct capability *cap,
                            coreid_t owner)
{
    capaddr_t caddr = get_cnode_addr(dest);
    uint8_t level = get_cnode_level(dest);
    size_t  slot  = dest.slot;

    return invoke_monitor_create_cap((uint64_t*)cap, caddr, level, slot, owner);
}

/** 
 * \brief Retype a capability on behalf of another domains.  Capabilities which
 * are remote (cross-core) must be retyped through the monitor to maintain 
 * cross-core consistancy.
 */
errval_t monitor_retype_remote_cap(struct capref croot, capaddr_t src, gensize_t offset,
                                   enum objtype newtype, gensize_t objsize,
                                   gensize_t count, capaddr_t to, capaddr_t slot, int level)
{
    uint8_t rootcap_level = get_cap_level(croot);
    capaddr_t rootcap_addr = get_cap_addr(croot);

    return invoke_monitor_remote_cap_retype(rootcap_addr, rootcap_level, src, offset,
                                            newtype, objsize, count, to, slot, level);
}

errval_t monitor_create_caps(struct capref src_root, struct capref dest_root,
                             enum objtype newtype, gensize_t objsize,
                             size_t count, capaddr_t src,
                             int src_level, size_t offset, capaddr_t dest_cn,
                             int dest_level, cslot_t dest_slot)
{
    capaddr_t src_root_cptr = get_cap_addr(src_root);
    capaddr_t dest_root_cptr = get_cap_addr(dest_root);

    return invoke_monitor_remote_cap_retype(src_root_cptr, src, offset,
                                            newtype, objsize, count,
                                            dest_root_cptr, dest_cn,
                                            dest_slot, dest_level);
}

errval_t monitor_copy_if_exists(struct capability* cap, struct capref dest)
{
    capaddr_t croot = get_croot_addr(dest);
    capaddr_t caddr = get_cnode_addr(dest);
    uint8_t level = get_cnode_level(dest);
    size_t  slot  = dest.slot;

    return invoke_monitor_copy_existing((uint64_t*)cap, croot, caddr, level, slot);
}

/**
 * \brief Determine the current owner of a cap and its copies.
 */
errval_t monitor_get_cap_owner(struct capref croot, capaddr_t cptr, int level, coreid_t *ret_owner)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_level = get_cap_level(croot);

    return invoke_monitor_get_cap_owner(root_addr, root_level, cptr, level, ret_owner);
}

/**
 * \brief Change the owner of a cap and its copies.
 */
errval_t monitor_set_cap_owner(struct capref croot, capaddr_t cptr, int level, coreid_t owner)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_level = get_cap_level(croot);

    return invoke_monitor_set_cap_owner(root_addr, root_level, cptr, level, owner);
}

/**
 * \brief Lock the cap and its copies
 */
errval_t monitor_lock_cap(struct capref croot, capaddr_t cptr, int level)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_level = get_cap_level(croot);

    return invoke_monitor_lock_cap(root_addr, root_level, cptr, level);
}

/**
 * \brief Unlock the cap and its copies
 */
errval_t monitor_unlock_cap(struct capref croot, capaddr_t cptr, int level)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_level = get_cap_level(croot);

    return invoke_monitor_unlock_cap(root_addr, root_level, cptr, level);
}

errval_t monitor_has_descendants(struct capability *cap, bool *res)
{
    return invoke_monitor_has_descendants((uint64_t*)cap, res);
}

errval_t monitor_is_retypeable(struct capability *cap, gensize_t offset,
                               gensize_t objsize, size_t count)
{
    return invoke_monitor_is_retypeable((uint64_t*)cap, offset, objsize, count);
}

errval_t monitor_delete_last(struct capref croot, capaddr_t cptr, int level, struct capref ret_cap)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_level = get_cap_level(croot);
    capaddr_t ret_cn = get_cnode_addr(ret_cap);
    uint8_t ret_cn_level = get_cnode_level(ret_cap);
    cslot_t ret_slot = ret_cap.slot;
    return invoke_monitor_delete_last(root_addr, root_level, cptr, level,
                                      ret_cn, ret_cn_level, ret_slot);
}

errval_t monitor_delete_foreigns(struct capref cap)
{
    capaddr_t cptr = get_cap_addr(cap);
    uint8_t level = get_cap_level(cap);
    return invoke_monitor_delete_foreigns(cptr, level);
}

errval_t monitor_revoke_mark_target(struct capref croot, capaddr_t cptr,
                                    int level)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_level = get_cap_level(croot);
    return invoke_monitor_revoke_mark_target(root_addr, root_level, cptr, level);
}

errval_t monitor_revoke_mark_relations(struct capability *cap)
{
    return invoke_monitor_revoke_mark_relations((uint64_t*)cap);
}

errval_t monitor_delete_step(struct capref ret_cap)
{
    return invoke_monitor_delete_step(get_cnode_addr(ret_cap),
                                      get_cnode_level(ret_cap),
                                      ret_cap.slot);
}

errval_t monitor_clear_step(struct capref ret_cap)
{
    return invoke_monitor_clear_step(get_cnode_addr(ret_cap),
                                     get_cnode_level(ret_cap),
                                     ret_cap.slot);
}
