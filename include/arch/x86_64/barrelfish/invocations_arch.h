/**
 * \file
 * \brief Arch-specific Low-level capability invocations
 */

/*
 * Copyright (c) 2007-2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INVOCATIONS_ARCH_H
#define INVOCATIONS_ARCH_H

#include <barrelfish/syscall_arch.h>
#include <barrelfish_kpi/dispatcher_shared.h>
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
    return cap_invoke9(root, CNodeCmd_Retype,
                       ((uint64_t)src_cspace << 32) | (uint64_t)cap, offset,
                       newtype, objsize, count,
                       ((uint64_t)to_cspace << 32) | (uint64_t)to, slot, to_level).error;
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

static inline errval_t invoke_ipi_notify_send(struct capref notify_cap)
{
    return cap_invoke1(notify_cap, NotifyCmd_Send).error;
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

static inline errval_t invoke_vnode_modify_flags(struct capref cap,
                                          size_t entry, size_t num_pages,
                                          size_t attr)
{
    return cap_invoke4(cap, VNodeCmd_ModifyFlags, entry, num_pages, attr).error;
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

static inline errval_t invoke_get_global_paddr(struct capref kernel_cap, genpaddr_t* global)
{
    struct sysret sr = cap_invoke1(kernel_cap, KernelCmd_GetGlobalPhys);
    if (err_is_ok(sr.error)) {
        *global = sr.value;
    }

    return sr.error;
}

#endif
