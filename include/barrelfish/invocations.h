/**
 * \file
 * \brief Architecture independent capability invocations
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INVOCATIONS_H
#define INVOCATIONS_H

#include <barrelfish_kpi/dispatcher_shared.h>
#include <barrelfish_kpi/distcaps.h> // for distcap_state_t
#include <barrelfish/caddr.h>
#include <barrelfish/cap_predicates.h> // get_address(), get_size()
#include <barrelfish/debug.h>

#include <barrelfish/invocations_arch.h>
#include <barrelfish/idc.h>

static inline errval_t invoke_ram_noop(struct capref ram)
{
    return cap_invoke1(ram, RAMCmd_Noop).error;
}

/**
 * \brief Create a capability.
 *
 * Create a new capability of type 'type' and size 'objbits'. The new cap will
 * be placed in the slot 'dest_slot' of the CNode located at 'dest_cnode_cptr'
 * in the address space rooted at 'root'.
 *
 * See also cap_create(), which wraps this.
 *
 * \param root            Capability of the CNode to invoke.
 * \param type            Kernel object type to create.
 * \param objsize         Size of created object
 *                        (ignored for fixed-size objects)
 * \param dest_cnode_cptr Address of CNode cap, where newly created cap will be
 *                        placed into.
 * \param dest_level      Depth/level of destination CNode.
 * \param dest_slot       Slot in CNode cap to place new cap.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_create(struct capref root,
                                           enum objtype type, size_t objsize,
                                           capaddr_t dest_cnode_cptr,
                                           enum cnode_type dest_level,
                                           capaddr_t dest_slot)
{
    assert(dest_cnode_cptr != CPTR_NULL);
    return cap_invoke6(root, CNodeCmd_Create, type, objsize, dest_cnode_cptr,
                       dest_level, dest_slot).error;
}

/**
 * \brief "Mint" a capability.
 *
 * Copies CPtr 'from' into slot 'slot' in the CNode, addressed by 'to', within
 * the address space, rooted at 'root' and with 'tobits' and 'frombits' address
 * bits of 'to' and 'from' valid, respectively.
 *
 * See also cap_mint(), which wraps this.
 *
 * \param root          Capability of the source cspace root CNode to invoke
 * \param to_cspace     Destination cspace cap address relative to source cspace
 * \param to            Destination CNode address relative to destination cspace
 * \param slot          Slot in destination CNode cap to place copy into
 * \param from          Address of cap to copy.
 * \param tolevel       Level/depth of 'to'.
 * \param fromlevel     Level/depth of 'from'.
 * \param param1        1st cap-dependent parameter.
 * \param param2        2nd cap-dependent parameter.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_mint(struct capref root, capaddr_t to_cspace,
                                         capaddr_t to, capaddr_t slot,
                                         capaddr_t from_cspace, capaddr_t from,
                                         enum cnode_type tolevel,
                                         enum cnode_type fromlevel,
                                         uint64_t param1, uint64_t param2)
{
    return cap_invoke10(root, CNodeCmd_Mint, to_cspace, to, slot, from_cspace,
                        from, tolevel, fromlevel, param1, param2).error;
}

/**
 * \brief Copy a capability.
 *
 * Copies CPtr 'from' into slot 'slot' in the CNode, addressed by 'to', within
 * the address space, rooted at 'root' and with 'tobits' and 'frombits' address
 * bits of 'to' and 'from' valid, respectively.
 *
 * See also cap_copy(), which wraps this.
 *
 * \param root          Capability of the source cspace root CNode to invoke
 * \param to_cspace     Capability address of destination root cnode relative
 *                      to our cspace
 * \param to            CNode address to place copy into relative to
 *                      destination cspace.
 * \param slot          Slot in CNode cap to place copy into.
 * \param from_cspace   Capability address of source root cnode relative
 *                      to our cspace
 * \param from          Address of cap to copy.
 * \param tolevel       Level/depth of 'to'.
 * \param fromlevel     Level/depth of 'from'.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_copy(struct capref root, capaddr_t to_cspace,
                                         capaddr_t to, capaddr_t slot,
                                         capaddr_t from_cspace, capaddr_t from,
                                         enum cnode_type tolevel,
                                         enum cnode_type fromlevel)
{
    return cap_invoke8(root, CNodeCmd_Copy, to_cspace, to, slot, from_cspace,
                       from, tolevel, fromlevel).error;
}

/**
 * \brief Delete a capability.
 *
 * Delete the capability pointed to by 'cap', with 'bits' address bits
 * of it valid, from the address space rooted at 'root'.
 *
 * \param root  Capability of the CNode to invoke
 * \param cap   Address of cap to delete.
 * \param level Level/depth of 'cap'.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_delete(struct capref root, capaddr_t cap,
                                           enum cnode_type level)
{
    return cap_invoke3(root, CNodeCmd_Delete, cap, level).error;
}

static inline errval_t invoke_cnode_revoke(struct capref root, capaddr_t cap,
                                           enum cnode_type level)
{
    return cap_invoke3(root, CNodeCmd_Revoke, cap, level).error;
}

static inline errval_t invoke_cnode_get_state(struct capref root, capaddr_t cap,
                                              enum cnode_type level, distcap_state_t *ret)
{
    struct sysret sysret = cap_invoke3(root, CNodeCmd_GetState, cap, level);

    assert(ret != NULL);
    if (err_is_ok(sysret.error)) {
        *ret = sysret.value;
    }
    else {
        *ret = 0;
    }
    return sysret.error;
}


/**
 * \brief Get the size of a L1 CNode in bytes.
 *
 * \param root Size of this L1 CNode will be returned
 * \param ret  Result will be stored here in bytes.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_get_size(struct capref root, size_t * ret)
{
    struct sysret sys_ret =  cap_invoke1(root, CNodeCmd_GetSize);
    if(err_is_ok(sys_ret.error)){
        *ret = sys_ret.value;
    } else {
        *ret = 0;
    }

    return sys_ret.error;
}

static inline errval_t invoke_cnode_resize(struct capref root, capaddr_t new_cptr,
                                           capaddr_t retcn_ptr, cslot_t retslot)
{
    return cap_invoke4(root, CNodeCmd_Resize, new_cptr, retcn_ptr, retslot).error;
}

static inline errval_t invoke_vnode_unmap(struct capref cap,
                                          capaddr_t mapping_addr,
                                          enum cnode_type level)
{
    return cap_invoke3(cap, VNodeCmd_Unmap, mapping_addr, level).error;
}

static inline errval_t invoke_vnode_modify_flags(struct capref cap,
                                          size_t entry, size_t num_pages,
                                          size_t attr)
{
    return cap_invoke4(cap, VNodeCmd_ModifyFlags, entry, num_pages, attr).error;
}

static inline errval_t invoke_vnode_copy_remap(struct capref ptable, capaddr_t slot,
                                        capaddr_t src, enum cnode_type srclevel,
                                        size_t flags, size_t offset, size_t pte_count,
                                        capaddr_t mcn_addr, cslot_t mapping_slot,
                                        enum cnode_type mcn_level)
{
    return cap_invoke10(ptable, VNodeCmd_CopyRemap, slot, src, srclevel, flags,
                        offset, pte_count, mcn_addr, mapping_slot, mcn_level).error;
}

/**
 * \brief Return the physical address of a kernel control block
 *
 * \param kcb      CSpace address of kernel control block capability
 * \param ret      frame_identity struct filled in with relevant data
 *
 * \return Error code
 */
static inline errval_t invoke_kcb_identify(struct capref kcb,
                                           struct frame_identity *ret)
{
    assert(ret != NULL);
    assert(get_croot_addr(kcb) == CPTR_ROOTCN);

    ret->base = 0;
    ret->bytes = 0;
    ret->pasid = 0;

    return cap_invoke2(kcb, KCBCmd_Identify, (uintptr_t)ret).error;
}

/**
 * \brief Return capability representation of a given CSpace address
 *
 * \param cap      CSpace address of capability
 * \param ret      capability struct filled in with relevant data
 *
 * \return Error code
 */
static inline errval_t invoke_cap_identify(struct capref cap,
                                           struct capability *ret)
{
    struct capref croot = get_croot_capref(cap);
    return cap_invoke4(croot, CNodeCmd_CapIdentify,
                       get_cap_addr(cap), get_cap_level(cap),
                       (uintptr_t)ret).error;
}

static inline errval_t invoke_vnode_identify(struct capref vnode,
					     struct vnode_identity *ret)
{
    errval_t err;
    struct capability retcap;
    err = invoke_cap_identify(vnode, &retcap);

    assert(ret != NULL);

    ret->base = 0;
    ret->type = 0;

    if (err_is_ok(err) && type_is_vnode(retcap.type)) {
        ret->base = get_address(&retcap);
	ret->type = retcap.type;
    } else if (err_is_ok(err)) {
        err = SYS_ERR_INVALID_SOURCE_TYPE;
    }
    return err;
}

static inline errval_t invoke_device_identify(struct capref deviceid,
                                              struct device_identity *ret)
{
    assert(ret != NULL);
    assert(get_croot_addr(deviceid) == CPTR_ROOTCN);

    struct sysret sysret = cap_invoke2(deviceid, DeviceID_Identify, (uintptr_t)ret);

    if (err_is_ok(sysret.error)) {
        return sysret.error;
    }

    ret->bus = 0;
    ret->device = 0;
    ret->function = 0;
    ret->flags = 0;
    ret->type = DEVICE_ID_TYPE_UNKNOWN;
    ret->segment = 0;
    return sysret.error;
}


static inline errval_t invoke_endpoint_identify(struct capref ep,
                                                struct endpoint_identity *ret)
{
    assert(ret != NULL);

    ret->iftype = 0;
    ret->base = 0;
    ret->length = 0;

    struct capability retcap;

    errval_t err = invoke_cap_identify(ep, &retcap);

    if (err_is_ok(err)) {
        if (retcap.type == ObjType_EndPointLMP) {
            // fill out with LMP info
            ret->base = (uintptr_t)retcap.u.endpointlmp.listener +
                        retcap.u.endpointlmp.epoffset;
            ret->length = retcap.u.endpointlmp.epbuflen;
            ret->iftype = retcap.u.endpointlmp.iftype;
            ret->eptype = IDC_ENDPOINT_LMP;
        } else if (retcap.type == ObjType_EndPointUMP) {
            // fill out with UMP info
            ret->base = get_address(&retcap);
            ret->length = get_size(&retcap);
            ret->iftype = retcap.u.endpointump.iftype;
            ret->eptype = IDC_ENDPOINT_UMP;
        } else {
            // fail if cap not endpoint
            ret->eptype = 0;
            err = SYS_ERR_INVALID_SOURCE_TYPE;
        }
    }

    return err;
}

/**
 * \brief Cleans all dirty bits in a page table.
 */
static inline errval_t invoke_clean_dirty_bits(struct capref vnode, size_t* how_many)
{

    struct sysret ret = cap_invoke1(vnode, VNodeCmd_CleanDirtyBits);
    if (how_many != NULL) {
        *how_many = ret.value;
    }
    return ret.error;
}

/**
 * \brief Modify mapping flags on parts of a mapping
 *
 * \param mapping  CSpace address of mapping capability
 * \param off      Offset (in #pages) of the first page to get new set of flags
 *                 from the first page in the mapping identified by `frame`
 * \param pages    Number of pages that should get new set of flags
 * \param flags    New set of flags
 * \param va_hint  Hint for selective TLB flushing
 *
 * \return Error code
 */
static inline errval_t invoke_mapping_modify_flags(struct capref mapping,
                                                   size_t offset,
                                                   size_t pages,
                                                   size_t flags,
                                                   genvaddr_t va_hint)
{
    return cap_invoke5(mapping, MappingCmd_Modify, offset,
                       pages, flags, va_hint).error;
}

/**
 * \brief Setup a dispatcher, possibly making it runnable
 *
 * \param dispatcher    Address of dispatcher capability relative to own
 *                      cspace
 * \param domdispatcher Address of existing dispatcher for domain ID relative
 *                      to own cspace
 * \param cspace        Root of CSpace for new dispatcher relative to own
 *                      cspace
 * \param vspace        Root of VSpace for new dispatcher relative to cspace
 *                      for new dispatcher.
 * \param dispframe     Frame capability for dispatcher structure relative to
 *                      cspace for new dispatcher.
 * \param run           Make runnable if true
 *
 * Need to either supply caprefs for all or none of cspace, vspace, dispframe
 * and domdispatcher.
 *
 * \return Error code
 */
static inline errval_t
invoke_dispatcher(struct capref dispatcher, struct capref domdispatcher,
                  struct capref cspace, struct capref vspace,
                  struct capref dispframe, bool run)
{
    assert(get_croot_addr(dispatcher) == CPTR_ROOTCN);
    assert(capref_is_null(cspace) || get_croot_addr(cspace) == CPTR_ROOTCN);
    assert(capref_is_null(domdispatcher) || get_croot_addr(domdispatcher) == CPTR_ROOTCN);
    assert(capref_is_null(vspace) || get_croot_addr(vspace) == get_cap_addr(cspace));
    assert(capref_is_null(dispframe) || get_croot_addr(dispframe) == get_cap_addr(cspace));

    capaddr_t root_caddr = get_cap_addr(cspace);
    uint8_t   root_level = get_cap_level(cspace);
    capaddr_t vtree_caddr = get_cap_addr(vspace);
    capaddr_t disp_caddr = get_cap_addr(dispframe);
    capaddr_t dd_caddr = get_cap_addr(domdispatcher);

    return cap_invoke7(dispatcher, DispatcherCmd_Setup, root_caddr,
                       root_level, vtree_caddr, disp_caddr, run,
                       dd_caddr).error;
}

static inline errval_t
invoke_dispatcher_properties(struct capref dispatcher,
                             enum task_type type, unsigned long deadline,
                             unsigned long wcet, unsigned long period,
                             unsigned long release, unsigned short weight)
{
    return cap_invoke7(dispatcher, DispatcherCmd_Properties, type, deadline,
                       wcet, period, release, weight).error;
}


static inline errval_t invoke_dispatcher_dump_ptables(struct capref dispcap, lvaddr_t vaddr)
{
    return cap_invoke2(dispcap, DispatcherCmd_DumpPTables, vaddr).error;
}

static inline errval_t invoke_dispatcher_dump_capabilities(struct capref dispcap)
{
    return cap_invoke1(dispcap, DispatcherCmd_DumpCapabilities).error;
}

/**
 * IRQ manipulations
 */
static inline errval_t invoke_irqdest_connect(struct capref irqcap, struct capref epcap)
{
    struct sysret ret = cap_invoke2(irqcap, IRQDestCmd_Connect, get_cap_addr(epcap));
    return ret.error;
}

static inline errval_t invoke_irqdest_get_vector(struct capref irqcap, uint64_t * out_vec)
{
    struct sysret ret = cap_invoke1(irqcap, IRQDestCmd_GetVector);
    *out_vec = ret.value;
    return ret.error;
}

static inline errval_t invoke_irqdest_get_cpu(struct capref irqcap, uint64_t * out_cpu)
{
    struct sysret ret = cap_invoke1(irqcap, IRQDestCmd_GetCpu);
    *out_cpu = ret.value;
    return ret.error;
}

static inline errval_t invoke_irqsrc_get_vec_start(struct capref irqcap, uint64_t * out_vec)
{
    struct sysret ret = cap_invoke1(irqcap, IRQSrcCmd_GetVecStart);
    *out_vec = ret.value;
    return ret.error;
}

static inline errval_t invoke_irqsrc_get_vec_end(struct capref irqcap, uint64_t * out_vec)
{
    struct sysret ret = cap_invoke1(irqcap, IRQSrcCmd_GetVecEnd);
    *out_vec = ret.value;
    return ret.error;
}

static inline errval_t invoke_irqtable_alloc_dest_cap(struct capref irqcap, struct capref dest_cap)
{
    uint8_t dcn_level = get_cnode_level(dest_cap);
    capaddr_t dcn_addr = get_cnode_addr(dest_cap);
    struct sysret ret = cap_invoke4(irqcap, IRQTableCmd_AllocDestCap,
                                    dcn_level, dcn_addr, dest_cap.slot);
    return ret.error;
}

/**
 * Deprecated. Use invoke_irqtable_alloc_dest_cap
 */
static inline errval_t invoke_irqtable_alloc_vector(struct capref irqcap, int *retirq)
{
    struct sysret ret = cap_invoke1(irqcap, IRQTableCmd_Alloc);
    if (err_is_ok(ret.error)) {
        *retirq = ret.value;
    } else {
        *retirq = 0;
    }
    return ret.error;
}

static inline errval_t invoke_irqtable_set(struct capref irqcap, int irq,
                                           struct capref ep)
{
    return cap_invoke3(irqcap, IRQTableCmd_Set, irq, get_cap_addr(ep)).error;
}

static inline errval_t invoke_irqtable_delete(struct capref irqcap, int irq)
{
    return cap_invoke2(irqcap, IRQTableCmd_Delete, irq).error;
}

/**
 * \brief do a kernel cap invocation to get the core id
 */
static inline errval_t invoke_kernel_get_core_id(struct capref kern_cap,
                                                 coreid_t *core_id)
{
    assert(core_id != NULL);

    struct sysret sysret = cap_invoke1(kern_cap, KernelCmd_Get_core_id);
    if (sysret.error == SYS_ERR_OK) {
        *core_id = sysret.value;
    }
    return sysret.error;
}

#endif // INVOCATIONS_H
