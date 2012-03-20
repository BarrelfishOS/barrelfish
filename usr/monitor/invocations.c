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
        memset(out, 0, sizeof(struct capability));
        out->type = ObjType_Null;
        return SYS_ERR_OK;
    }

    uint8_t vbits = get_cap_valid_bits(cap);
    capaddr_t caddr = get_cap_addr(cap) >> (CPTR_BITS - vbits);
    return invoke_monitor_identify_cap(caddr, vbits, out);
}


/**
 * \brief Invoke the kernel cap to acquire the raw metadata of a cap that is 
 * located off another domains root cnode
 *
 * \param croot   The root cnode of the process to which this cap belongs
 * \param cap     The caddr of the cap to identify
 * \param vbits   Valid bits of the cap to identify
 * \param out     Struct to return the metadata
 */
errval_t monitor_domains_cap_identify(struct capref croot, capaddr_t cap,
                                      int vbits, struct capability *out)
{
    assert (out != NULL);

    uint8_t rootcap_vbits = get_cap_valid_bits(croot);
    capaddr_t rootcap_addr  = get_cap_addr(croot) >> (CPTR_BITS - rootcap_vbits);


    return invoke_monitor_identify_domains_cap(rootcap_addr, rootcap_vbits,
                                               cap, vbits, out);
}

/**
 * Let the kernel know that this capability has been sent to a remote core, or
 * is no longer on a remote core
 * 
 * param cap       capability which is having a remote copy made or deleted
 * param is_remote whether the capability is on a remote core or not
 * param has_decendants outputs whether the cap has decendants or not
 */
errval_t monitor_cap_remote(struct capref cap, bool is_remote, 
                            bool * has_decendants)
{
    uint8_t vbits = get_cap_valid_bits(cap);
    capaddr_t caddr = get_cap_addr(cap) >> (CPTR_BITS - vbits);
    return invoke_monitor_cap_remote(caddr, vbits, is_remote, has_decendants);
}

/**
 * \brief Invoke the kernel cap to set the type of #cap to null
 */
errval_t monitor_nullify_cap(struct capref cap)
{
    uint8_t vbits = get_cap_valid_bits(cap);
    capaddr_t caddr = get_cap_addr(cap) >> (CPTR_BITS - vbits);
    return invoke_monitor_nullify_cap(caddr, vbits);
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
    uint8_t vbits = get_cnode_valid_bits(dest);
    size_t  slot  = dest.slot;

    return invoke_monitor_create_cap((uint64_t*)cap, caddr, vbits, slot, owner);
}

/**
 * \brief Walk the mdb to lookup a local copy of the cnode and return the #slot
 * within that cnode.
 */
errval_t monitor_identify_cnode_get_cap(struct capability *cnode_raw,
                                        capaddr_t slot, struct capability *ret)
{
    assert(cnode_raw != NULL);
    assert(ret != NULL);

    // Assert cnode type
    assert(cnode_raw->type == ObjType_CNode);
    // Assert slot within the cnode
    assert(slot < (1UL << cnode_raw->u.cnode.bits));

    return invoke_monitor_identify_cnode_get_cap((uint64_t*)cnode_raw,
                                                 slot, ret);
}

/** 
 * \brief Retype a capability on behalf of another domains.  Capabilities which
 * are remote (cross-core) must be retyped through the monitor to maintain 
 * cross-core consistancy.
 */
errval_t monitor_retype_remote_cap(struct capref croot, capaddr_t src, 
                                   enum objtype newtype, int objbits, 
                                   capaddr_t to, capaddr_t slot, int bits)
{
    uint8_t rootcap_vbits = get_cap_valid_bits(croot);
    capaddr_t rootcap_addr  = get_cap_addr(croot) >> (CPTR_BITS - rootcap_vbits);

    return invoke_monitor_remote_cap_retype(rootcap_addr, rootcap_vbits, src,
                                            newtype, objbits, to, slot, bits);
}

errval_t monitor_create_caps(struct capref croot, enum objtype newtype,
                             int objbits, capaddr_t src, int src_bits,
                             capaddr_t dest_cn, int dest_bits,
                             cslot_t dest_slot)
{
    uint8_t rootcap_vbits = get_cap_valid_bits(croot);
    capaddr_t rootcap_addr  = get_cap_addr(croot) >> (CPTR_BITS - rootcap_vbits);

    return invoke_monitor_remote_cap_retype(rootcap_addr, rootcap_vbits, src,
                                            newtype, objbits, dest_cn,
                                            dest_slot, dest_bits);
}

/**
 * \brief Determine the current owner of a cap and its copies.
 */
errval_t monitor_get_cap_owner(struct capref croot, capaddr_t cptr, int bits, coreid_t *ret_owner)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_bits = get_cap_valid_bits(croot);

    return invoke_monitor_get_cap_owner(root_addr, root_bits, cptr, bits, ret_owner);
}

/**
 * \brief Change the owner of a cap and its copies.
 */
errval_t monitor_set_cap_owner(struct capref croot, capaddr_t cptr, int bits, coreid_t owner)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_bits = get_cap_valid_bits(croot);

    return invoke_monitor_set_cap_owner(root_addr, root_bits, cptr, bits, owner);
}

/**
 * \brief Lock the cap and its copies
 */
errval_t monitor_lock_cap(struct capref croot, capaddr_t cptr, int bits)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_bits = get_cap_valid_bits(croot);

    return invoke_monitor_lock_cap(root_addr, root_bits, cptr, bits);
}

/**
 * \brief Unlock the cap and its copies
 */
errval_t monitor_unlock_cap(struct capref croot, capaddr_t cptr, int bits)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_bits = get_cap_valid_bits(croot);

    return invoke_monitor_unlock_cap(root_addr, root_bits, cptr, bits);
}

errval_t monitor_delete_last(struct capref croot, capaddr_t cptr, int bits, struct capref ret_cap)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_bits = get_cap_valid_bits(croot);
    capaddr_t ret_cn = ret_cap.cnode.address;
    uint8_t ret_cn_bits = ret_cap.cnode.address_bits;
    cslot_t ret_slot = ret_cap.slot;
    return invoke_monitor_delete_last(root_addr, root_bits, cptr, bits,
                                      ret_cn, ret_cn_bits, ret_slot);
}

errval_t monitor_continue_revoke(struct capref croot, capaddr_t cptr, int bits, struct capref ret_cap)
{
    capaddr_t root_addr = get_cap_addr(croot);
    uint8_t root_bits = get_cap_valid_bits(croot);
    capaddr_t ret_cn = ret_cap.cnode.address;
    uint8_t ret_cn_bits = ret_cap.cnode.address_bits;
    cslot_t ret_slot = ret_cap.slot;
    return invoke_monitor_delete_last(root_addr, root_bits, cptr, bits,
                                      ret_cn, ret_cn_bits, ret_slot);
}
