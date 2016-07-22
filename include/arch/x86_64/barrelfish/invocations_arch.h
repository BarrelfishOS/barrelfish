/**
 * \file
 * \brief Low-level capability invocations
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INVOCATIONS_ARCH_H
#define INVOCATIONS_ARCH_H

#include <barrelfish/syscall_arch.h>
#include <barrelfish_kpi/dispatcher_shared.h>
#include <barrelfish_kpi/distcaps.h>
#include <barrelfish/caddr.h>
#include <barrelfish_kpi/paging_arch.h>

static inline struct sysret cap_invoke(struct capref to, uintptr_t arg1,
                                       uintptr_t arg2, uintptr_t arg3,
                                       uintptr_t arg4, uintptr_t arg5,
                                       uintptr_t arg6, uintptr_t arg7,
                                       uintptr_t arg8, uintptr_t arg9,
                                       uintptr_t arg10)
{
    capaddr_t invoke_cptr = get_cap_addr(to);
    enum cnode_type invoke_level = get_cap_level(to);

    return syscall(SYSCALL_INVOKE, (uint64_t)invoke_cptr << 32 |
                   (uint64_t)invoke_level << 16 | 10 << 8, 0,
                   arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
                   arg10);
}

#define cap_invoke10(to, _a, _b, _c, _d, _e, _f, _g, _h, _i, _j)   \
    cap_invoke(to, _a, _b, _c, _d, _e, _f, _g, _h, _i, _j)
#define cap_invoke9(to, _a, _b, _c, _d, _e, _f, _g, _h, _i)        \
    cap_invoke10(to, _a, _b, _c, _d, _e, _f, _g, _h, _i, 0)
#define cap_invoke8(to, _a, _b, _c, _d, _e, _f, _g, _h)    \
    cap_invoke9(to, _a, _b, _c, _d, _e, _f, _g, _h, 0)
#define cap_invoke7(to, _a, _b, _c, _d, _e, _f, _g)    \
    cap_invoke8(to, _a, _b, _c, _d, _e, _f, _g, 0)
#define cap_invoke6(to, _a, _b, _c, _d, _e, _f)        \
    cap_invoke7(to, _a, _b, _c, _d, _e, _f, 0)
#define cap_invoke5(to, _a, _b, _c, _d, _e)            \
    cap_invoke6(to, _a, _b, _c, _d, _e, 0)
#define cap_invoke4(to, _a, _b, _c, _d)                \
    cap_invoke5(to, _a, _b, _c, _d, 0)
#define cap_invoke3(to, _a, _b, _c)                    \
    cap_invoke4(to, _a, _b, _c, 0)
#define cap_invoke2(to, _a, _b)                        \
    cap_invoke3(to, _a, _b, 0)
#define cap_invoke1(to, _a)                            \
    cap_invoke2(to, _a, 0)


/**
 * \brief Retype (part of) a capability.
 *
 * Retypes (part of) CPtr 'cap' into 'objsize'd caps of type 'newtype' and places them
 * into slots starting at slot 'slot' in the CNode, addressed by 'to', with
 * 'bits' address bits of 'to' valid.
 *
 * See also cap_retype(), which wraps this.
 *
 * \param root          Capability of the source cspace root CNode to invoke
 * \param src_cspace    Source cspace cap address in our cspace.
 * \param cap           Address of cap to retype in source cspace.
 * \param offset        Offset into cap to retype
 * \param newtype       Kernel object type to retype to.
 * \param objsize       Size of created objects, for variable-sized types
 * \param count         Number of objects to create
 * \param to_cspace     Destination cspace cap address in our cspace
 * \param to            Address of CNode cap in destination cspcae to place
 *                      retyped caps into.
 * \param to_level      Level/depth of CNode cap in destination cspace
 * \param slot          Slot in CNode cap to start placement.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_retype(struct capref root, capaddr_t src_cspace,
                                           capaddr_t cap, gensize_t offset,
                                           enum objtype newtype, gensize_t objsize,
                                           size_t count, capaddr_t to_cspace,
                                           capaddr_t to, enum cnode_type to_level,
                                           capaddr_t slot)
{
    assert(cap != CPTR_NULL);
    return cap_invoke10(root, CNodeCmd_Retype,
                        ((uint64_t)src_cspace << 32) | (uint64_t)cap, offset,
                        newtype, objsize, count, to_cspace, to, slot, to_level).error;
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
 * \param objbits         Size of created object
 *                        (ignored for fixed-size objects)
 * \param dest_cnode_cptr Address of CNode cap, where newly created cap will be
 *                        placed into.
 * \param dest_slot       Slot in CNode cap to place new cap.
 * \param dest_level      Depth/level of destination CNode.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_create(struct capref root,
                                           enum objtype type, uint8_t objbits,
                                           capaddr_t dest_cnode_cptr,
                                           capaddr_t dest_slot,
                                           enum cnode_type dest_level)
{
    assert(dest_cnode_cptr != CPTR_NULL);
    return cap_invoke6(root, CNodeCmd_Create, type, objbits, dest_cnode_cptr,
                       dest_slot, dest_level).error;
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

static inline errval_t invoke_vnode_map(struct capref ptable, capaddr_t slot,
                                        capaddr_t src_root, capaddr_t src,
                                        enum cnode_type srclevel, size_t
                                        flags, size_t offset, size_t pte_count,
                                        capaddr_t mcnroot, capaddr_t mcnaddr,
                                        enum cnode_type mcnlevel, cslot_t mapping_slot)
{
    return cap_invoke10(ptable, VNodeCmd_Map, slot,
                        ((uint64_t)src_root << 32) | (uint64_t)src, srclevel,
                        flags, offset, pte_count,
                        ((uint64_t)mcnroot << 32) | (uint64_t)mcnaddr,
                        mcnlevel, mapping_slot).error;
}

static inline errval_t invoke_vnode_unmap(struct capref cap,
                                          capaddr_t mapping_addr,
                                          enum cnode_type level)
{
    return cap_invoke3(cap, VNodeCmd_Unmap, mapping_addr, level).error;
}

/**
 * \brief Return the physical address and size of a frame capability
 *
 * \param frame    CSpace address of frame capability
 * \param ret      frame_identity struct filled in with relevant data
 *
 * \return Error code
 */
static inline errval_t invoke_frame_identify(struct capref frame,
                                             struct frame_identity *ret)
{
    assert(ret != NULL);
    assert(get_croot_addr(frame) == CPTR_ROOTCN);

    struct sysret sysret = cap_invoke2(frame, FrameCmd_Identify, (uintptr_t)ret);

    if (err_is_ok(sysret.error)) {
        return sysret.error;
    }

    ret->base = 0;
    ret->bytes = 0;
    return sysret.error;
}

static inline errval_t invoke_vnode_identify(struct capref vnode,
					     struct vnode_identity *ret)
{
    struct sysret sysret = cap_invoke1(vnode, VNodeCmd_Identify);

    assert(ret != NULL);
    if (err_is_ok(sysret.error)) {
        ret->base = sysret.value & (~BASE_PAGE_MASK);
	ret->type = sysret.value & BASE_PAGE_MASK;
        return sysret.error;
    }

    ret->base = 0;
    ret->type = 0;
    return sysret.error;
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

static inline errval_t invoke_iocap_in(struct capref iocap, enum io_cmd cmd,
                                       uint16_t port, uint32_t *data)
{
    struct sysret sysret = cap_invoke2(iocap, cmd, port);

    if (err_is_ok(sysret.error)) {
        assert(data != NULL);
        *data = sysret.value;
    }
    return sysret.error;
}

static inline errval_t invoke_iocap_out(struct capref iocap, enum io_cmd cmd,
                                        uint16_t port, uint32_t data)
{
    return cap_invoke3(iocap, cmd, port, data).error;
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
    assert(get_croot_addr(cspace) == CPTR_ROOTCN);
    assert(get_croot_addr(domdispatcher) == CPTR_ROOTCN);
    assert(get_croot_addr(vspace) == get_cap_addr(cspace));
    assert(get_croot_addr(dispframe) == get_cap_addr(cspace));

    capaddr_t root_caddr = get_cap_addr(cspace);
    uint8_t   root_level = get_cap_level(cspace);
    capaddr_t vtree_caddr = get_cap_addr(vspace);
    capaddr_t disp_caddr = get_cap_addr(dispframe);
    capaddr_t dd_caddr = get_cap_addr(domdispatcher);

    return cap_invoke7(dispatcher, DispatcherCmd_Setup, root_caddr,
                       root_level, vtree_caddr, disp_caddr, run,
                       dd_caddr).error;
}

/**
 * \brief Execute vmread on the VMCS of the VM guest DCB
 *
 * The VMCS must be current and active.
 *
 * \param dcb       Dispatcher capability
 * \param encoding  Encoding of the field to read from the VMCS
 * \param addr      The address to write the value of the field to.
 */
static inline errval_t invoke_dispatcher_vmread(struct capref dispatcher, 
						uintptr_t encoding, 
						lvaddr_t *addr)
{
    return cap_invoke3(dispatcher, DispatcherCmd_Vmread, 
		       encoding, (uintptr_t)addr).error; 
}

/**
 * \brief Execute vmwrite on the VMCS of the VM guest DCB
 *
 * The VMCS must be current and active.
 *
 * \param dcb       Dispatcher capability
 * \param encoding  Encoding of the field to write to the VMCS.
 * \param value     Value of the field to write.
 */

static inline errval_t invoke_dispatcher_vmwrite(struct capref dispatcher, 
						 uintptr_t encoding, 
						 uintptr_t value)
{
    return cap_invoke3(dispatcher, DispatcherCmd_Vmwrite, 
		       encoding, value).error; 
}

/**
 * \brief Execute vmptrld on the VMCS of the VM guest DCB
 *
 * \param dcb       Dispatcher capability
 */
static inline errval_t invoke_dispatcher_vmptrld(struct capref dispatcher)
{
    return cap_invoke1(dispatcher, DispatcherCmd_Vmptrld).error; 
}

/**
 * \brief Execute vmclear on the VMCS of the VM guest DCB
 *   
 * \param dcb       Dispatcher capability
 */
static inline errval_t invoke_dispatcher_vmclear(struct capref dispatcher)
{
    return cap_invoke1(dispatcher, DispatcherCmd_Vmclear).error; 
}

/**
 * \brief Setup a VM guest DCB
 *
 * \param dcb       Dispatcher capability
 */
static inline errval_t
invoke_dispatcher_setup_guest(struct capref dispatcher,
                              struct capref ep_cap,
                              struct capref vnode,
                              struct capref vmkit_guest,
                              struct capref guest_control_cap)
{
    return cap_invoke5(dispatcher, DispatcherCmd_SetupGuest,
                       get_cap_addr(ep_cap), get_cap_addr(vnode),
                       get_cap_addr(vmkit_guest),
                       get_cap_addr(guest_control_cap)).error;
}

static inline errval_t invoke_irqdest_connect(struct capref irqcap, struct capref epcap)
{
    struct sysret ret = cap_invoke2(irqcap, IRQDestCmd_Connect, get_cap_addr(epcap));
    return ret.error;
}

static inline errval_t invoke_irqdest_get_vector(struct capref irqcap, uint32_t * out_vec)
{
    struct sysret ret = cap_invoke1(irqcap, IRQDestCmd_GetVector);
    *out_vec = ret.value;
    return ret.error;
}

static inline errval_t invoke_irqsrc_get_vector(struct capref irqcap, uint32_t * out_vec)
{
    struct sysret ret = cap_invoke1(irqcap, IRQSrcCmd_GetVector);
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

static inline errval_t invoke_dispatcher_dump_ptables(struct capref dispcap)
{
    return cap_invoke1(dispcap, DispatcherCmd_DumpPTables).error;
}

static inline errval_t invoke_dispatcher_dump_capabilities(struct capref dispcap)
{
    return cap_invoke1(dispcap, DispatcherCmd_DumpCapabilities).error;
}

static inline errval_t invoke_perfmon_activate(struct capref perfmon_cap,
                                               uint8_t event, uint8_t perf_umask,
                                               bool kernel, uint8_t counter_id,
                                               uint64_t counter_value,
                                               capaddr_t ep_addr)
{
    return cap_invoke7(perfmon_cap, PerfmonCmd_Activate,
                       event, perf_umask, counter_id, kernel,
                       counter_value, ep_addr).error;
}

static inline errval_t invoke_perfmon_write(struct capref perfmon_cap,
                                                  uint8_t counter_id,
                                                  uint64_t counter_value)
{
    return cap_invoke3(perfmon_cap, PerfmonCmd_Write, counter_id, counter_value).error;
}

static inline errval_t invoke_perfmon_deactivate(struct capref perfmon_cap)
{
    return cap_invoke1(perfmon_cap, PerfmonCmd_Deactivate).error;
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

static inline errval_t invoke_ipi_notify_send(struct capref notify_cap)
{
    return cap_invoke1(notify_cap, NotifyCmd_Send).error;
}

/**
 * \brief Return the system-wide unique ID of the passed ID capability.
 *
 * \param idcap ID capability to invoke.
 * \param id    Filled-in with system-wide unique ID of ID cap.
 *
 * \return      Error code
 */
static inline errval_t invoke_idcap_identify(struct capref idcap,
                                             idcap_id_t *id)
{
    assert(id != NULL);
    assert(get_croot_addr(idcap) == CPTR_ROOTCN);

    struct sysret sysret = cap_invoke1(idcap, IDCmd_Identify);

    if (err_is_ok(sysret.error)) {
        *id = sysret.value;
    }

    return sysret.error;
}

static inline errval_t invoke_send_init_ipi(struct capref ipi_cap, coreid_t core_id)
{
    return cap_invoke2(ipi_cap, IPICmd_Send_Init,
                       core_id).error;
}

static inline errval_t invoke_send_start_ipi(struct capref ipi_cap, coreid_t core_id, forvaddr_t entry)
{
    return cap_invoke3(ipi_cap, IPICmd_Send_Start,
                       core_id, entry).error;
}

static inline errval_t invoke_get_global_paddr(struct capref kernel_cap, genpaddr_t* global)
{ 
    struct sysret sr = cap_invoke1(kernel_cap, KernelCmd_GetGlobalPhys);
    if (err_is_ok(sr.error)) {
        *global = sr.value;
    }

    return sr.error;
}

#endif
